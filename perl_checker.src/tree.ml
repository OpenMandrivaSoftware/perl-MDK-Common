open Types
open Common
open Printf
open Parser_helper

type special_export = Re_export_all | Export_all

type exports = { 
    export_ok : (context * string) list ;
    export_auto : (context * string) list ;
    export_tags : (string * (context * string) list) list ;
    special_export : special_export option ;
  }

type uses = (string * ((context * string) list option * pos)) list

type per_package = {
    file_name : string ;
    package_name : string ; has_package_name : bool ;
    vars_declared : (context * string, pos) Hashtbl.t ;
    imported : ((context * string) * string) list option ref ;
    exports : exports ;
    uses : uses ;
    body : fromparser list;
  }
type state = {
    per_package : (string * per_package) list ;
    files_parsed : string list ;
    global_vars_declared : (context * string * string, pos) Hashtbl.t ;
    global_vars_used : ((context * string * string) * pos) list ref ;
  }

type vars = { 
    my_vars : ((context * string) * (pos * bool ref)) list list ;
    our_vars : ((context * string) * (pos * bool ref)) list list ;
    locally_imported : ((context * string) * string) list ;
    required_vars : (context * string * string) list ;
    current_package : per_package ;
    state : state ;
  }

let anonymous_package_count = ref 0
let default_state = { per_package = []; files_parsed = []; global_vars_declared = Hashtbl.create 256; global_vars_used = ref [] }
let empty_exports = { export_ok = []; export_auto = []; export_tags = []; special_export = None }
let ignored_packages = ref []
let use_lib = ref []

let die_with_pos pos msg = failwith (Info.pos2sfull pos ^ msg)
let warn_with_pos pos msg = prerr_endline (Info.pos2sfull pos ^ msg)

let s2context s = 
  match s.[0] with
  | '$' -> I_scalar, skip_n_char 1 s
  | '%' -> I_hash  , skip_n_char 1 s
  | '@' -> I_array , skip_n_char 1 s
  | '&' -> I_func  , skip_n_char 1 s
  | '*' -> I_star  , skip_n_char 1 s
  | _ -> I_raw, s



let get_current_package t =
  match t with
  | Package(Ident _ as ident) :: body ->
      let rec bundled_packages packages current_package found_body = function
	| [] -> (Some current_package, List.rev found_body) :: packages
	| Package(Ident _ as ident) :: body ->
	    let packages = (Some current_package, List.rev found_body) :: packages in
	    bundled_packages packages (string_of_Ident ident) [] body
	| instr :: body ->
	    bundled_packages packages current_package (instr :: found_body) body
      in
      bundled_packages [] (string_of_Ident ident) [] body
  | _ -> 
      if str_ends_with !Info.current_file ".pm" then warn_with_pos (!Info.current_file, 0, 0) (sprintf "module %s does not have \"package xxxx;\" on its first line" !Info.current_file) ;
      [ None, t ]

let from_qw = function
  | Call_op("qw", [ Raw_string(s, pos)], _) -> 
      List.map (fun s -> 
	let context, s' = s2context s in
	let context =
	  match context with
	  | I_raw -> if s'.[0] = ':' then I_raw else I_func
	  | I_func -> warn_with_pos pos "weird, exported name with a function context especially given"; I_func
	  | _ -> context 
	in context, s'
	       ) (words s)
  | String(_, pos) ->
      warn_with_pos pos "not recognised yet" ;
      []
  | _ -> internal_error "get_exported"

let get_exported t =
  List.fold_left (fun exports e ->
    match e with
    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT", _)); Call _ ], pos) ]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT")], _);  Call _ ], pos) ] ->
	if exports.special_export = None then warn_with_pos pos "unrecognised @EXPORT" ;
	exports

    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT", _)); v ], pos)]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT")], _); v ], pos)] ->
	if exports.export_auto <> [] then warn_with_pos pos "weird, @EXPORT set twice" ;
	{ exports with export_auto = from_qw v }

    | Perl_checker_comment("RE-EXPORT-ALL", _) -> { exports with special_export = Some Re_export_all }
    | Perl_checker_comment("EXPORT-ALL",    _) -> { exports with special_export = Some Export_all }

    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT_OK", _)); v ], pos)]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT_OK")], _);  v ], pos)] ->
	if exports.export_ok <> [] then warn_with_pos pos "weird, @EXPORT_OK set twice" ;
	(match v with
	| Call(Deref(I_func, Ident(None, "map", _)), 
	       [ Anonymous_sub(Block [List [Deref(I_array, Deref(I_scalar, Ident (None, "_", _)))]], _);
		 Call(Deref(I_func, Ident(None, "values", _)), [ Deref(I_hash, Ident(None, "EXPORT_TAGS", _))])]) ->
		   { exports with export_ok = collect snd exports.export_tags }
	| _ -> { exports with export_ok = from_qw v })

    | List [ Call_op("=", [ Deref(I_hash, Ident(None, "EXPORT_TAGS", _)); v ], pos)]
    | List [ Call_op("=", [ My_our("our", [(I_hash, "EXPORT_TAGS")], _);  v ], pos)] ->
	(try
	  let export_tags =
	    match v with
	    | List [ List l ] ->
		List.map (function
		  | Raw_string(tag, _), Ref(I_array, List [List [v]]) ->
		      let para =
			match v with
			| Deref(I_array, Ident(None, "EXPORT_OK", _)) -> exports.export_ok
			| _ -> from_qw v
		      in
		      ":" ^ tag, para
		  | _ -> raise Not_found
	        ) (group_by_2 l)
	    | _ -> raise Not_found
	  in
	  if exports.export_tags <> [] then warn_with_pos pos "weird, %EXPORT_TAGS set twice" ;
	  { exports with export_tags = export_tags }
	with _ ->
	  warn_with_pos pos "unrecognised %EXPORT_TAGS" ;
	  exports)
    | List (My_our _ :: _) ->
	let _,_ = e,e in
	exports
    | _ -> exports
  ) empty_exports t

let uses_external_package = function
  | "vars" | "MDK::Common::Globals" | "Exporter" | "diagnostics" | "strict" | "lib" | "POSIX" | "Gtk" | "Gtk2"
  | "Config" | "Socket" | "Net::FTP" | "IO::Socket" | "DynaLoader" | "Data::Dumper" -> true
  | _ -> false

let get_uses t =
  List.fold_left (fun uses e ->
    match e with
    | Use(Ident(None, "lib", _), [libs]) ->
	use_lib := List.map snd (from_qw libs) @ !use_lib ;
	uses
    | Use(Ident _ as pkg, _) when uses_external_package (string_of_Ident pkg) -> uses
    | Use(Ident(_, _, pos) as ident, l) ->
	let package = string_of_Ident ident in
	let para = if l = [] then None else Some(from_qw (List.hd l)) in
	(package, (para, pos)) :: uses
    | _ -> uses
  ) [] t

let get_vars_declaration state package = 
  List.iter (function
    | Sub_declaration(Ident(None, name, pos), _proto, _) ->
	Hashtbl.replace package.vars_declared (I_func, name) pos
    | Sub_declaration(Ident(Some fq, name, pos), _proto, _) ->
	Hashtbl.replace state.global_vars_declared (I_func, fq, name) pos

    | List [ Call_op("=", [My_our("our", ours, pos); _], _) ]
    | List [ My_our("our", ours, pos) ]
    | My_our("our", ours, pos) ->
	List.iter (fun (context, name) -> Hashtbl.replace package.vars_declared (context, name) pos) ours

    | Use(Ident(Some "MDK::Common", "Globals", pos), [ String _ ; ours ])
    | Use(Ident(None, "vars", pos), [ours]) -> 
	List.iter (fun (context, name) -> Hashtbl.replace package.vars_declared (context, name) pos) (from_qw ours)
    | Use(Ident(None, "vars", pos), _) -> 
	die_with_pos pos "usage: use vars qw($var func)"

    | List [ Method_call(Raw_string(pkg, pos), Raw_string("bootstrap", _), _) ] ->
	if pkg <> package.package_name then
	  warn_with_pos pos "strange bootstrap (the package name is not the same as the current package)"
	else
	  (try
	    let cfile = Filename.chop_extension package.file_name ^ ".c" in
	    let prefix = "newXS(\"" ^ pkg ^ "::" in
	    ignore (fold_lines (fun in_bootstrap s ->
	      if in_bootstrap then
		(try
		  let offset = strstr s prefix + String.length prefix in
		  let end_ = String.index_from s offset '"' in
		  let ident = String.sub s offset (end_ - offset) in
		  match split_at2 ':'':' ident with
		  | [_] -> Hashtbl.replace package.vars_declared (I_func, ident) pos
		  | l -> 
		      if l <> [] then
			let fql, name = split_last l in
			let fq = String.concat "::" (pkg :: fql) in
			Hashtbl.replace state.global_vars_declared (I_func, fq, name) pos	
		with Not_found -> ());
	      in_bootstrap || str_contains s "XS_VERSION_BOOTCHECK"
	    ) false (open_in cfile))
	  with Invalid_argument _ | Sys_error _ -> ())
    | _ -> ()
  ) package.body

let rec get_imported state (package_name, (imports, pos)) =
  try
    let package_used = List.assoc package_name state.per_package in
    let exports = package_used.exports in
    match imports with
    | None ->
	let re = match exports.special_export with
	| Some Re_export_all -> get_imports state package_used
	| Some Export_all -> Hashtbl.fold (fun var _ l -> (var, package_name) :: l) package_used.vars_declared []
	| _ -> [] in
	let l = List.map (fun (context, name) -> (context, name), package_name) exports.export_auto in
	re @ l
    | Some l -> 
	let imports_vars = 
	  collect (function
	    | I_raw, tag -> 
		(try 
		  List.assoc tag exports.export_tags
		with Not_found -> die_with_pos pos (sprintf "package %s doesn't export tag %s" package_name tag))
	    | variable ->
		if List.mem variable exports.export_ok then
		  [ variable ]
		else
		  die_with_pos pos (sprintf "package %s doesn't export %s" package_name (variable2s variable))
		  ) l
	in
	List.map (fun (context, name) -> (context, name), package_name) imports_vars
  with Not_found -> []

and get_imports state package =
  match !(package.imported) with
  | Some l -> l
  | None ->
      let l = collect (get_imported state) package.uses in
      package.imported := Some l ;
      l

let rec fold_tree f env e = 
  match f env e with
  | Some env -> env
  | None ->
  match e with
  | Anonymous_sub(e', _)
  | Ref(_, e')
  | Deref(_, e')
    -> fold_tree f env e'

  | Diamond(e')
    -> fold_tree_option f env e'

  | String(l, _)
    -> List.fold_left (fun env (_, e) -> fold_tree f env e) env l

  | Sub_declaration(e1, _, e2)
  | Deref_with(_, _, e1, e2)
    -> 
      let env = fold_tree f env e1 in
      let env = fold_tree f env e2 in
      env

  | Use(_, l)
  | List l
  | Block l
  | Call_op(_, l, _)
    -> List.fold_left (fold_tree f) env l

  | Call(e', l)
    -> 
      let env = fold_tree f env e' in
      List.fold_left (fold_tree f) env l

  | Method_call(e1, e2, l)
    ->
      let env = fold_tree f env e1 in
      let env = fold_tree f env e2 in
      List.fold_left (fold_tree f) env l

  | _ -> env

and fold_tree_option f env = function
  | None -> env
  | Some e -> fold_tree f env e


let get_global_info_from_package t =
  let current_packages = get_current_package t in
  map_withenv (fun required_packages (current_package, t) ->
    let exports = get_exported t in
    let uses = get_uses t in
    let package_name =
      match current_package with
      | None -> 
	  if exports.export_ok <> [] || exports.export_auto <> [] || exports.export_tags <> [] then
	    die_with_pos (!Info.current_file, 0, 0) "file with no package name wants to export!"
	  else
	    (incr anonymous_package_count ; sprintf "anonymous%d" !anonymous_package_count)
      | Some name -> name
    in
    let required_packages = List.fold_left (fold_tree (fun l -> 
      function
	| Call(Deref(I_func, Ident (None, "require", pos)), [Ident _ as pkg]) ->
	    let package = string_of_Ident pkg in
	    if uses_external_package package then None else Some((package, pos) :: l)
	| Call(Deref(I_func, Ident (None, "require", pos)), [Raw_string(pkg, _)])
	    when not (String.contains pkg '/') && Filename.check_suffix pkg ".pm" ->
	    let package = Filename.chop_suffix pkg ".pm" in
	    if uses_external_package package then None else Some((package, pos) :: l)
	| _ -> None)
      ) required_packages t in
    { 
      file_name = !Info.current_file ; 
      package_name = package_name;
      has_package_name = current_package <> None ;
      exports = exports ;
      imported = ref None ;
      vars_declared = Hashtbl.create 16 ;
      uses = uses ;
      body = t ;
    }, required_packages
  ) [] current_packages

let is_my_declared vars t = 
  List.exists (fun l ->
    List.mem_assoc t l && (snd (List.assoc t l) := true ; true)
  ) vars.my_vars
let is_our_declared vars t = 
  List.exists (fun l ->
    List.mem_assoc t l && (snd (List.assoc t l) := true ; true)
  ) vars.our_vars
let is_var_declared vars (context, name) = 
  List.mem_assoc (context, name) vars.locally_imported ||
  List.mem_assoc (context, name) (get_imports vars.state vars.current_package) ||
  Hashtbl.mem vars.current_package.vars_declared (context, name)
let is_global_var_declared vars (context, fq, name) =
  Hashtbl.mem vars.state.global_vars_declared (context, fq, name) ||
  (try
    let package = List.assoc fq vars.state.per_package in
    Hashtbl.mem package.vars_declared (context, name) ||
    List.mem_assoc (context, name) (get_imports vars.state package)
  with Not_found -> false)

 

let is_global_var context ident = 
  match context with
  | I_scalar -> 
      (match ident with
      | "@" | "!" | ">" | "\\" | "$" | "^A" | "'" | "/" | "?" | "<" | "^W" | "|" | "^I" | "&"
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> true
      | _ -> false)
  | I_array -> 
      (match ident with
      | "ARGV" | "INC" -> true
      | _ -> false)
  | I_hash ->
      (match ident with
      | "ENV" | "SIG" -> true
      | _ -> false)
  | I_star ->
      (match ident with
      | "STDIN" | "STDOUT" | "STDERR"
      | "__FILE__" | "__LINE__" | "undef" -> true
      | _ -> false)
  | I_func ->
      (match ident with
      | "-b" | "-d" | "-e" | "-f" | "-l" | "-r" | "-s" | "-w" | "-x"
      | "abs" | "alarm" | "basename" | "bless" 
      | "caller" | "chdir" | "chmod" | "chomp" | "chop" | "chown" | "chr" | "chroot" | "close" | "closedir" | "crypt"
      | "defined" | "delete" | "die"
      | "each" | "endpwent" | "eof" | "eval" | "exec" | "exists" | "exit"
      | "fcntl" | "fileno" | "formline" | "fork"
      | "gethostbyaddr" | "gethostbyname" | "getgrnam" | "getgrgid" | "getppid" | "getpwent" | "getpwnam" | "getpwuid" | "gmtime" | "goto" | "grep" | "hex"
      | "index" | "int" | "ioctl" | "join" | "keys" | "kill"
      | "last" | "lc" | "length" | "link" | "localtime" | "log" | "lstat"
      | "map" | "mkdir" | "next" | "no" | "oct" | "open" | "opendir" | "ord"
      | "pack" | "pipe" | "pop" | "print" | "printf" | "push" | "quotemeta" 
      | "rand" | "read" | "readdir" | "readlink" | "redo" | "ref" | "rename" | "require" | "return" | "reverse" | "rmdir"
      | "scalar" | "seek" | "select" | "setpwent" | "shift" | "sleep" | "sort" | "splice" | "split" | "sprintf" | "stat" | "substr"
      | "symlink" | "syscall" | "sysopen" | "sysread" | "sysseek" | "system" | "syswrite" | "tie" | "time"
      | "uc" | "umask" | "undef" | "unlink" | "unpack" | "unshift" | "utime" | "values" | "vec" | "waitpid" | "wantarray" | "warn" | "write"
	  -> true

      | _ -> false)
  | _ -> false

let check_variable (context, var) vars = 
  match var with
  | Ident(Some pkg, _, _) when uses_external_package pkg || List.mem pkg !ignored_packages -> ()
  | Ident(None, ident, pos) ->
      if is_my_declared vars (context, ident) || is_our_declared vars (context, ident) || is_global_var context ident || is_var_declared vars (context, ident)
      then () 
      else warn_with_pos pos (if context = I_func then "unknown function " ^ ident else "undeclared variable " ^ variable2s(context, ident))
  | Ident(Some fq, name, pos) -> 
      if (fq = "CORE") && is_global_var context name || is_global_var_declared vars (context, fq, name)
      then ()
      else 
	if context = I_func then 
	  warn_with_pos pos ("unknown function " ^ string_of_Ident var)
	else
	  lpush vars.state.global_vars_used ((context, fq, name), pos)
  | _ -> ()

let declare_My vars (mys, pos) =
  let l_new = List.filter (fun (context, ident) ->
    if context = I_raw then
      if ident = "undef" then false else die_with_pos pos (sprintf "bad ident %s in my" ident)
    else true
  ) mys in
  let l_pre = List.hd vars.my_vars in
  List.iter (fun v ->
    if List.mem_assoc v l_pre then warn_with_pos pos (sprintf "redeclared variable %s" (variable2s v))
  ) l_new ;
  { vars with my_vars = (List.map (fun v -> v, (pos, ref false)) l_new @ l_pre) :: List.tl vars.my_vars }

let declare_Our vars (ours, pos) =
  match vars.our_vars with
  | [] -> vars (* we're at the toplevel, already declared in vars_declared *)
  | l_pre :: other ->
      List.iter (fun v ->
	if List.mem_assoc v l_pre && v <> (I_scalar, "_") then warn_with_pos pos (sprintf "redeclared variable %s" (variable2s v))
      ) ours ;
      { vars with our_vars = (List.map (fun v -> v, (pos, ref false)) ours @ l_pre) :: other }

let declare_My_our vars (my_or_our, l, pos) =
  match my_or_our with
  | "my"  -> declare_My  vars (l, pos)
  | "local"
  | "our" -> declare_Our vars (l, pos)
  | _ -> internal_error "declare_My_our"

let check_unused_local_variables vars =
  List.iter (fun ((_, s as v), (pos, used)) ->
    if not !used && s.[0] != '_' then warn_with_pos pos (sprintf "unused variable %s" (variable2s v))
  ) (List.hd vars.my_vars)
  


let check_variables vars t = 
  let rec check_variables_ vars t = fold_tree check vars t
  and check vars = function
    | Block l ->
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' l in
	check_unused_local_variables vars' ;
	Some vars
    | Call(Deref(I_func, Ident(None, "sort", _)), (Anonymous_sub(Block f, pos) :: l)) ->
	let vars = List.fold_left check_variables_ vars l in
	let vars' = { vars with my_vars = [ (I_scalar, "a"), (pos, ref true) ; (I_scalar, "b"), (pos, ref true) ] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' f in
	check_unused_local_variables vars' ;
	Some vars

    | Call(Deref(I_func, Ident(None, func, _)), Anonymous_sub(Block f, pos) :: l) when func = "grep" || func = "map" || func = "substInFile" || func = "map_index" || func = "each_index" || func = "partition" || func = "find_index" || func = "grep_index" ->
	let vars = List.fold_left check_variables_ vars l in
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [(I_scalar, "_"), (pos, ref true)] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' f in
	check_unused_local_variables vars' ;
	Some vars

    | Call_op("while infix", [ expr ; (List [ Call_op("<>", _, _) ] as l) ], pos)
    | Call_op("for infix", [ expr ; l ], pos) ->
	let vars = check_variables_ vars l in
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [(I_scalar, "_"), (pos, ref true)] :: vars.our_vars } in
	let vars' = check_variables_ vars' expr in
	if List.hd(vars'.my_vars) <> [] then warn_with_pos pos "you can't declare variables in foreach infix";
	Some vars

    | Call_op("foreach my", [my; expr; Block block], _) ->
	let vars = check_variables_ vars expr in
	let vars = check_variables_ vars (Block (my :: block)) in
	Some vars
    | Call_op(op, cond :: Block first_bl :: other, _) when op = "if" || op = "while" || op = "unless" || op = "until" ->
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars' = check_variables_ vars' cond in
	let vars' = List.fold_left check_variables_ vars' first_bl in
	check_unused_local_variables vars' ;
	let vars = List.fold_left check_variables_ vars other in
	Some vars

    | Sub_declaration(Ident(fq, name, pos) as ident, _proto, Block l) ->
	let vars = declare_Our vars ([ I_func, string_of_Ident ident ], pos) in
	let local_vars = ((I_array, "_"), (pos, ref true)) :: (if fq = None && name = "AUTOLOAD" then [ (I_scalar, "AUTOLOAD"), (pos, ref true) ] else []) in
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = local_vars :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' l in
	check_unused_local_variables vars' ;
	Some vars

    | Anonymous_sub(Block l, pos) ->
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [(I_array, "_"), (pos, ref true)] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' l in
	check_unused_local_variables vars' ;
	Some vars

    | Call_op("foreach", [ expr ; Block l ], pos) ->
	let vars = check_variables_ vars expr in
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [(I_scalar, "_"), (pos, ref true)] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' l in
	check_unused_local_variables vars' ;
	Some vars

    | Anonymous_sub _
    | Sub_declaration _ -> internal_error "check_variables"

    | Ident _ as var ->
	check_variable (I_star, var) vars ;
	Some vars

    | My_our(my_or_our, mys, pos) -> Some(declare_My_our vars (my_or_our, mys, pos))
    | Deref(context, (Ident _ as var)) -> 
	check_variable (context, var) vars ;
	Some vars
    | Deref_with(context, _, (Ident _ as var), para) -> 
	let vars = check_variables_ vars para in
	check_variable (context, var) vars ;
	Some vars

    | Call_op("=", [My_our(my_or_our, mys, pos); e], _) ->
	(* check e first *)
	let vars = check_variables_ vars e in
	List.iter (fun (context, var) ->
	  if non_scalar_context context then die_with_pos pos (sprintf "%s takes all the arguments, %s is undef in any case" (variable2s (context, var)) (variable2s (last mys)))
	) (removelast mys) ; (* mys is never empty *)
	Some(declare_My_our vars (my_or_our, mys, pos))

    | Call_op("if infix", [List [My_our _]; List [Num("0", _)]], _) -> None (* special allowed case *)
    | Call_op(op, List (My_our _ :: _) :: _, pos) 
    | Call_op(op, My_our _ :: _, pos)
    | Call_op(op, Call_op("local", _, _) :: _, pos) ->
	if op <> "=" then warn_with_pos pos (sprintf "applying %s on a new initialized variable is wrong" op);
	None

    | Call(Deref(I_func, Ident(None, "require", _)), [Ident _]) -> Some vars

    | Method_call(Raw_string(package_name, pos), Raw_string ("import", _), para) ->
	let args =
	  match para with
	  | [] -> None
	  | [ List [v] ] -> Some(from_qw v)
	  | _ -> die_with_pos pos "bad import statement" in
	let l = get_imported vars.state (package_name, (args, pos)) in
	let vars = { vars with locally_imported = l @ vars.locally_imported } in
	Some vars
	
    | _ -> None
  in
  let vars = List.fold_left check_variables_ { vars with my_vars = [[]] } t in
  vars

let check_tree state package =
  let vars = { my_vars = [[]]; our_vars = []; locally_imported = []; required_vars = []; current_package = package; state = state } in
  let _vars = check_variables vars package.body in
  ()

let add_package_to_state state package = 
  let per_package =
    try
      update_assoc (fun existing_package ->
	(*prerr_endline (existing_package.file_name ^ " vs " ^ package.file_name); *)
	Hashtbl.iter (fun var pos -> Hashtbl.replace existing_package.vars_declared var pos) package.vars_declared ;
	{ existing_package with
	  body = existing_package.body @ package.body ;
	  uses = existing_package.uses @ package.uses ;
	  exports = { export_ok   = existing_package.exports.export_ok   @ package.exports.export_ok ;
		      export_auto = existing_package.exports.export_auto @ package.exports.export_auto ;
		      export_tags = existing_package.exports.export_tags @ package.exports.export_tags ;
		      special_export = None }
	}
      ) package.package_name state.per_package
  with Not_found ->
    (package.package_name, package) :: state.per_package
  in
  { state with 
    per_package = per_package ; 
    files_parsed = package.file_name :: state.files_parsed }
