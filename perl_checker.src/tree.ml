open Types
open Common
open Printf

type exports = { 
    export_ok : (context * string) list ;
    export_auto : (context * string) list ;
    export_tags : (string * (context * string) list) list ;
    re_export_all : bool ;
  }

type uses = (string * ((context * string) list option * pos)) list

type per_package = {
    file_name : string ;
    package_name : string ; has_package_name : bool ;
    exports : exports ;
    uses : uses ;
    body : fromparser list;
  }
type state = {
    per_package : (string * per_package) list ;
    global_vars_declared : (context * string * string, pos) Hashtbl.t ;
    global_vars_used : ((context * string * string) * pos) list ref ;
  }

type vars = { 
    my_vars : (context * string) list list ;
    our_vars : (context * string) list list ;
    imported : ((context * string) * string) list ;
    current_package : string ;
    state : state ;
  }

let anonymous_package_count = ref 0
let default_state = { per_package = []; global_vars_declared = Hashtbl.create 256; global_vars_used = ref [] }
let empty_exports = { export_ok = []; export_auto = []; export_tags = []; re_export_all = false }


let die_with_pos pos msg = failwith (Info.pos2sfull pos ^ msg)
let warn_with_pos pos msg = prerr_endline (Info.pos2sfull pos ^ msg)

let context2s = function
  | I_scalar -> "$"
  | I_hash -> "%"
  | I_array -> "@"
  | I_func -> "&"
  | I_raw -> ""
  | I_star -> "*"
let variable2s(context, ident) = context2s context ^ ident
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
  | Package(Ident _ as ident) :: _ -> 
      Some (Parser_helper.string_of_Ident ident)
  | _ -> 
      if str_ends_with !Info.current_file ".pm" then warn_with_pos (!Info.current_file, 0, 0) (sprintf "module %s does not have \"package xxxx;\" on its first line" !Info.current_file) ;
      None

let from_qw = function
  | Call_op("qw", [ Raw_string(s, pos)]) -> 
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
    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT", pos)); Call _ ]) ]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT")], pos);  Call _ ]) ] ->
	if not exports.re_export_all then warn_with_pos pos "unrecognised @EXPORT" ;
	exports

    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT", pos)); v ])]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT")], pos); v ])] ->
	if exports.export_auto <> [] then warn_with_pos pos "weird, @EXPORT set twice" ;
	{ exports with export_auto = from_qw v }

    | Perl_checker_comment("RE-EXPORT-ALL", _) -> { exports with re_export_all = true }

    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT_OK", pos)); v ])]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT_OK")], pos);  v ])] ->
	if exports.export_ok <> [] then warn_with_pos pos "weird, @EXPORT_OK set twice" ;
	(match v with
	| Call(Deref(I_func, Ident(None, "map", _)), 
	       [ Anonymous_sub(Block [List [Deref(I_array, Deref(I_scalar, Ident (None, "_", _)))]]);
		 Call(Deref(I_func, Ident(None, "values", _)), [ Deref(I_hash, Ident(None, "EXPORT_TAGS", _))])]) ->
		   { exports with export_ok = collect snd exports.export_tags }
	| _ -> { exports with export_ok = from_qw v })

    | List [ Call_op("=", [ Deref(I_hash, Ident(None, "EXPORT_TAGS", pos)); v ])]
    | List [ Call_op("=", [ My_our("our", [(I_hash, "EXPORT_TAGS")], pos);  v ])] ->
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
  | "vars" | "MDK::Common::Globals" | "Exporter" | "diagnostics" | "strict" | "lib" | "POSIX" 
  | "Config" | "Socket" | "Net::FTP" | "IO::Socket" | "DynaLoader" -> true
  | _ -> false

let get_uses t =
  List.fold_left (fun uses e ->
    match e with
    | Use(Ident _ as pkg, _) when uses_external_package (Parser_helper.string_of_Ident pkg) -> uses
    | Use(Ident(_, _, pos) as ident, l) ->
	let package = Parser_helper.string_of_Ident ident in
	let para = if l = [] then None else Some(from_qw (List.hd l)) in
	(package, (para, pos)) :: uses
    | _ -> uses
  ) [] t

let get_global_info_from_package t =
  let exports = get_exported t in
  let uses = get_uses t in
  let current_package = get_current_package t in
  let package_name =
    match current_package with
    | None -> 
	if exports.export_ok <> [] || exports.export_auto <> [] || exports.export_tags <> [] then
	  die_with_pos (!Info.current_file, 0, 0) "file with no \"package\" wants to export!"
	else
	  (incr anonymous_package_count ; sprintf "anonymous%d" !anonymous_package_count)
    | Some name -> name
  in { file_name = !Info.current_file ; package_name = package_name; has_package_name = current_package <> None ; exports = exports ; uses = uses ; body = t }

let get_global_vars_declaration state package = 
  List.iter (function
    | Sub_declaration(Ident(fq, name, pos), _proto, _) ->
	Hashtbl.add state.global_vars_declared (I_func, some_or fq package.package_name, name) pos

    | List [ Call_op("=", [My_our("our", ours, pos); _]) ]
    | List [ My_our("our", ours, pos) ]
    | My_our("our", ours, pos) ->
	List.iter (fun (context, name) -> Hashtbl.add state.global_vars_declared (context, package.package_name, name) pos) ours

    | Use(Ident(Some "MDK::Common", "Globals", pos), [ String _ ; ours ])
    | Use(Ident(None, "vars", pos), [ours]) -> 
	List.iter (fun (context, name) -> Hashtbl.add state.global_vars_declared (context, package.package_name, name) pos) (from_qw ours)
    | Use(Ident(None, "vars", pos), _) -> 
	die_with_pos pos "usage: \"use vars qw($var func)\""
    | _ -> ()
  ) package.body

let get_imports state package =
  let rec get_one (package_name, (imports, pos)) =
    try
      let package_used = List.assoc package_name state.per_package in
      let exports = package_used.exports in
      let imports_vars =
	match imports with
	| None ->
	    let re = 
	      if exports.re_export_all 
	      then collect (fun (package_name, _) -> (List.assoc package_name state.per_package).exports.export_ok) package_used.uses
	      else [] in
	    exports.export_auto @ re
	| Some l -> 
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
  in
  collect get_one package.uses

let rec fold_tree f env e = 
  match f env e with
  | Some env -> env
  | None ->
  match e with
  | Anonymous_sub(e')
  | Ref(_, e')
  | Deref(_, e')
      -> fold_tree f env e'

  | Diamond(e')
       -> fold_tree_option f env e'

  | Sub_declaration(e1, _, e2)
  | Deref_with(_, e1, e2)
  | Binop(_, e1, e2)
       -> 
	 let env = fold_tree f env e1 in
	 let env = fold_tree f env e2 in
	 env

  | Use(_, l)
  | List l
  | Block l
  | Call_op(_, l)
      -> List.fold_left (fold_tree f) env l

  | Call(e', l)
  | CallP(e', l)
    -> 
      let env = fold_tree f env e' in
      List.fold_left (fold_tree f) env l

  | Method_call(e1, e2, l)
  | Method_callP(e1, e2, l)
    ->
      let env = fold_tree f env e1 in
      let env = fold_tree f env e2 in
      List.fold_left (fold_tree f) env l

  | If_then_else(_, t_l, e') 
    -> 
      let env = fold_tree_option f env e' in
      List.fold_left (fun env (e1, e2) -> 
	let env = fold_tree f env e1 in
	let env = fold_tree f env e2 in
	env
      ) env t_l

  | _ -> env

and fold_tree_option f env = function
  | None -> env
  | Some e -> fold_tree f env e


let is_my_declared vars t = List.exists (List.exists ((=) t)) vars.my_vars
let is_our_declared vars t = List.exists (List.exists ((=) t)) vars.our_vars
let is_global_var_declared vars (context, fq, name) =
  let fq = some_or fq vars.current_package in
  Hashtbl.mem vars.state.global_vars_declared (context, fq, name)

let is_global_var context ident = 
  match context with
  | I_scalar -> 
      (match ident with
      | "_" | "@" | "!" | ">" | "\\" | "$" | "^A" | "'" | "/" | "?" | "<" | "^W" | "|" | "^I"
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> true
      | _ -> false)
  | I_array -> 
      (match ident with
      | "_" | "ARGV" | "INC" -> true
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
      | "abs" | "alarm" | "basename" | "bless" 
      | "caller" | "chdir" | "chmod" | "chomp" | "chop" | "chown" | "chr" | "chroot" | "close" | "closedir" | "crypt" | "delete" | "die"
      | "each" | "eval" | "exec" | "exists" | "exit" | "fcntl" | "fileno" | "fork"
      | "gethostbyaddr" | "gethostbyname" | "getgrnam" | "getgrgid" | "getpwent" | "getpwnam" | "getpwuid" | "gmtime" | "goto" | "grep" | "hex"
      | "index" | "int" | "ioctl" | "join" | "keys" | "kill"
      | "last" | "lc" | "length" | "link" | "localtime" | "log" | "lstat"
      | "map" | "mkdir" | "next" | "oct" | "open" | "opendir" | "ord"
      | "pack" | "pipe" | "pop" | "print" | "printf" | "push" | "quotemeta" 
      | "rand" | "read" | "readdir" | "readlink" | "redo" | "ref" | "rename" | "require" | "return" | "reverse" | "rmdir"
      | "scalar" | "select" | "setpwent" | "shift" | "sleep" | "sort" | "splice" | "split" | "sprintf" | "stat" | "substr"
      | "symlink" | "sysopen" | "sysread" | "sysseek" | "system" | "syswrite" | "time" | "uc" | "umask" | "unpack" | "unshift"
      | "unlink" | "utime" | "values" | "vec" | "waitpid" | "wantarray" | "warn" | "write"
	  -> true

      | _ -> false)
  | _ -> false

let check_variable (context, var) vars = 
  match var with
  | Ident(None, ident, pos) when context <> I_func ->
      if is_my_declared vars (context, ident) || is_our_declared vars (context, ident) || 
         List.mem_assoc (context, ident) vars.imported || is_global_var context ident || is_global_var_declared vars (context, None, ident)
      then () 
      else warn_with_pos pos (sprintf "undeclared variable %s" (variable2s(context, ident)))
  | Ident(fq, name, pos) -> 
      if context = I_func && fq = None && is_global_var context name || 
         is_global_var_declared vars (context, fq, name)
      then ()
      else lpush vars.state.global_vars_used ((context, some_or fq vars.current_package, name), pos) 
  | _ -> ()

let declare_My vars (mys, pos) =
  let l_new = List.filter (fun (context, ident) ->
    if context = I_raw then
      if ident = "undef" then false else die_with_pos pos (sprintf "bad ident \"%s\" in my" ident)
    else true
  ) mys in
  let l_pre = List.hd vars.my_vars in
  List.iter (fun v ->
    if List.exists ((=) v) l_pre then warn_with_pos pos (sprintf "redeclared variable \"%s\"" (variable2s v))
  ) l_new ;
  { vars with my_vars = (l_new @ l_pre) :: List.tl vars.my_vars }

let declare_Our vars (ours, pos) =
  match vars.our_vars with
  | [] -> vars (* we're at the toplevel, already declared in global_vars_declared *)
  | l_pre :: other ->
      List.iter (fun v ->
	if List.exists ((=) v) l_pre then warn_with_pos pos (sprintf "redeclared variable \"%s\"" (variable2s v))
      ) ours ;
      { vars with our_vars = (ours @ l_pre) :: other }

let declare_My_our vars (my_or_our, l, pos) =
  match my_or_our with
  | "my"  -> declare_My  vars (l, pos)
  | "local"
  | "our" -> declare_Our vars (l, pos)
  | _ -> internal_error "declare_My_our"


let check_variables vars t = 
  let rec check_variables_ vars t = fold_tree check vars t
  and check vars = function
    | Block l ->
	let vars = { vars with my_vars = [] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars = List.fold_left check_variables_ vars l in
	let vars = { vars with my_vars = List.tl vars.my_vars ; our_vars = List.tl vars.our_vars } in
	Some vars
    | Call(Deref(I_func, Ident(None, "sort", _)), (Anonymous_sub(Block f) :: l)) ->
	let vars = List.fold_left check_variables_ vars l in
	let vars = { vars with my_vars = [ I_scalar, "a" ; I_scalar, "b" ] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars = List.fold_left check_variables_ vars f in
	let vars = { vars with my_vars = List.tl vars.my_vars ; our_vars = List.tl vars.our_vars } in
	Some vars

    | Call_op("foreach my", [my; expr; Block block]) ->
	let vars = check_variables_ vars expr in
	let vars = check_variables_ vars (Block (my :: block)) in
	Some vars
    | Call_op(op, cond :: Block first_bl :: other) when op = "if" || op = "while" || op = "unless" || op = "until" ->
	let vars = { vars with my_vars = [] :: vars.my_vars ; our_vars = [] :: vars.our_vars } in
	let vars = check_variables_ vars cond in
	let vars = List.fold_left check_variables_ vars first_bl in
	let vars = { vars with my_vars = List.tl vars.my_vars ; our_vars = List.tl vars.our_vars } in
	let vars = List.fold_left check_variables_ vars other in
	Some vars

    | Sub_declaration(Ident(fq, name, pos), _proto, body) ->
	let vars = declare_Our vars ([ I_func, (some_or fq vars.current_package) ^ "::" ^ name ], pos) in
	let vars = check_variables_ vars body in
	Some vars

    | Ident _ as var ->
	check_variable (I_star, var) vars ;
	Some vars

    | My_our(my_or_our, mys, pos) -> Some(declare_My_our vars (my_or_our, mys, pos))
    | Deref(context, (Ident _ as var)) -> 
	check_variable (context, var) vars ;
	Some vars
    | Deref_with(context, (Ident _ as var), para) -> 
	let vars = check_variables_ vars para in
	check_variable (context, var) vars ;
	Some vars

    | Call_op(op, [My_our(my_or_our, mys, pos); e]) ->
	if op = "=" then
	  (* check e first *)
	  let vars = check_variables_ vars e in
	  Some(declare_My_our vars (my_or_our, mys, pos))
	else
	  (warn_with_pos pos "weird" ; None)

    | _ -> None
  in
  let vars = List.fold_left check_variables_ { vars with my_vars = [[]] } t in
  vars

(*
let check_vars vars =
  List.iter (function 
    | I_func, (f, pos) -> 
	if not (is_our_declared vars (I_func, f)) then warn_with_pos pos ("unknown function " ^ f)
    | _ -> ()
  ) vars.global_vars_used
*)

let check_tree state package =
  let imports = get_imports state package in
  let vars = { my_vars = [[]]; our_vars = []; imported = imports; current_package = package.package_name; state = state } in
  let _vars = check_variables vars package.body in
  ()
