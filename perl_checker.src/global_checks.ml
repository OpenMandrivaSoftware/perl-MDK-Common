open Types
open Common
open Printf
open Config_file
open Parser_helper
open Tree

type state = {
    per_package : (string, per_package) Hashtbl.t ;
    methods : (string, (pos * bool ref) list) Hashtbl.t ;
    global_vars_declared : (context * string * string, pos) Hashtbl.t ;
    global_vars_used : ((context * string * string) * pos) list ref ;
  }

type vars = { 
    my_vars : ((context * string) * (pos * bool ref)) list list ;
    our_vars : ((context * string) * (pos * bool ref)) list list ;
    locally_imported : ((context * string) * (string * bool ref)) list ;
    required_vars : (context * string * string) list ;
    current_package : per_package ;
    state : state ;
  }


let rec get_imported state current_package (package_name, (imports, pos)) =
  try
    let package_used = Hashtbl.find state.per_package package_name in
    let exports = package_used.exports in
    let get_var_by_name var =
      let b =
	try snd (Hashtbl.find package_used.vars_declared var)
	with Not_found -> 
	  try
	    snd (List.assoc var (get_imports state package_used))
	  with Not_found ->
	    warn_with_pos pos (sprintf "name %s is not defined in package %s" (variable2s var) package_name) ;
	    ref true
      in
      var, (package_name, b)
    in
    match imports with
    | None ->
	let re = match exports.special_export with
	| Some Re_export_all -> get_imports state package_used
	| Some Fake_export_all -> 
	    (* HACK: if package exporting-all is ignored, ignore package importing *)
	    if List.mem package_name !ignored_packages then Tree.ignore_package current_package.package_name;
	      
	    Hashtbl.fold (fun var (_pos, b) l -> (var, (package_name, b)) :: l) package_used.vars_declared []
	| _ -> [] in
	let l = List.map get_var_by_name exports.export_auto in
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
	List.map get_var_by_name imports_vars
  with Not_found -> []

and get_imports state package =
  match !(package.imported) with
  | Some l -> l
  | None ->
      let l = collect (get_imported state package) package.uses in
      package.imported := Some l ;
      l


let is_my_declared vars t = 
  List.exists (fun l ->
    List.mem_assoc t l && (snd (List.assoc t l) := true ; true)
  ) vars.my_vars
let is_our_declared vars t = 
  List.exists (fun l ->
    List.mem_assoc t l && (snd (List.assoc t l) := true ; true)
  ) vars.our_vars

let is_var_declared_and_set state package var =
  try
    let (_pos, used) = Hashtbl.find package.vars_declared var in
    used := true ; 
    true
  with Not_found -> 
    try
      let (_pos, used) = List.assoc var (get_imports state package) in
      used := true ;
      true
    with Not_found ->
      false

let is_var_declared vars var = 
  List.mem_assoc var vars.locally_imported ||
  is_var_declared_and_set vars.state vars.current_package var

let is_global_var_declared vars (context, fq, name) =
  Hashtbl.mem vars.state.global_vars_declared (context, fq, name) ||
  (try
    let package = Hashtbl.find vars.state.per_package fq in
    is_var_declared_and_set vars.state package (context, name)
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
      | "abs" | "alarm" | "bless" 
      | "caller" | "chdir" | "chmod" | "chomp" | "chop" | "chown" | "chr" | "chroot" | "close" | "closedir" | "crypt"
      | "defined" | "delete" | "die"
      | "each" | "endpwent" | "eof" | "eval" | "exec" | "exists" | "exit"
      | "fcntl" | "fileno" | "flock" | "formline" | "fork"
      | "gethostbyaddr" | "gethostbyname" | "getgrnam" | "getgrgid" | "getppid" | "getpwent" | "getpwnam" | "getpwuid" | "gmtime" | "goto" | "grep" | "hex"
      | "index" | "int" | "ioctl" | "join" | "keys" | "kill"
      | "last" | "lc" | "lcfirst" | "length" | "link" | "localtime" | "log" | "lstat"
      | "map" | "mkdir" | "next" | "no" | "oct" | "open" | "opendir" | "ord"
      | "pack" | "pipe" | "pop" | "print" | "printf" | "push" | "quotemeta" 
      | "rand" | "read" | "readdir" | "readlink" | "redo" | "ref" | "rename" | "require" | "return" | "reverse" | "rindex" | "rmdir"
      | "scalar" | "seek" | "select" | "setpwent" | "shift" | "sleep" | "sort" | "splice" | "split" | "sprintf" | "stat" | "substr"
      | "symlink" | "syscall" | "sysopen" | "sysread" | "sysseek" | "system" | "syswrite" | "tie" | "time"
      | "uc" | "ucfirst" | "umask" | "undef" | "unlink" | "unpack" | "unshift" | "utime" | "values" | "vec" | "wait" | "waitpid" | "wantarray" | "warn" | "write"
	  -> true

      | _ -> false)
  | _ -> false

let check_variable (context, var) vars = 
  match var with
  | Ident(Some pkg, _, _) when uses_external_package pkg || List.mem pkg !ignored_packages -> ()
  | Ident(None, ident, pos) ->
      if is_my_declared vars (context, ident) || is_our_declared vars (context, ident) || is_var_declared vars (context, ident) || is_global_var context ident
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
    if not !used && s.[0] != '_' && not (List.mem s [ "BEGIN"; "END"; "DESTROY" ]) then warn_with_pos pos (sprintf "unused variable %s" (variable2s v))
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

    | Call(Deref(I_func, Ident(None, func, func_pos)), Anonymous_sub(Block f, pos) :: l) when func = "grep" || func = "map" || func = "substInFile" || func = "map_index" || func = "each_index" || func = "partition" || func = "find_index" || func = "grep_index" ->
	let vars = List.fold_left check_variables_ vars l in
	let vars' = { vars with my_vars = [] :: vars.my_vars ; our_vars = [(I_scalar, "_"), (pos, ref true)] :: vars.our_vars } in
	let vars' = List.fold_left check_variables_ vars' f in
	check_unused_local_variables vars' ;
	check_variable (I_func, Ident(None, func, func_pos)) vars ;
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
	let l = get_imported vars.state vars.current_package (package_name, (args, pos)) in
	let vars = { vars with locally_imported = l @ vars.locally_imported } in
	Some vars

    | Method_call(Raw_string(pkg, _), Raw_string(method_, pos), para) ->
	let vars = List.fold_left check_variables_ vars para in	
	let rec search pkg =
	  if is_global_var_declared vars (I_func, pkg, method_) then true
	  else
	    let package = Hashtbl.find vars.state.per_package pkg in
	    List.exists search (List.map fst (some_or package.isa []))
	in
	(try
	  if not (uses_external_package pkg || List.mem pkg !ignored_packages || search pkg || method_ = "bootstrap") then 
	    warn_with_pos pos (sprintf "unknown method %s starting in package %s" method_ pkg);
	with Not_found -> (* no warning, "can't find package" is already warned *)());
	Some vars

    | Method_call(o, Raw_string(method_, pos), para) ->
	let vars = check_variables_ vars o in
	let vars = List.fold_left check_variables_ vars para in	
	(try 
	  let l = Hashtbl.find vars.state.methods method_ in
	  List.iter (fun (_, used) -> used := true) l
	with Not_found -> 
	  if not (List.mem method_ [ "isa" ]) then
	    warn_with_pos pos ("unknown method " ^ method_)) ;
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
  let package =
    try
      let existing_package = Hashtbl.find state.per_package package.package_name in
      if existing_package.from_cache then raise Not_found;
      (* print_endline_flush (existing_package.file_name ^ " vs " ^ package.file_name); *)
      Hashtbl.iter (fun var pos -> Hashtbl.replace existing_package.vars_declared var pos) package.vars_declared ;
      { existing_package with
	body = existing_package.body @ package.body ;
	uses = existing_package.uses @ package.uses ;
	exports = { export_ok   = existing_package.exports.export_ok   @ package.exports.export_ok ;
		    export_auto = existing_package.exports.export_auto @ package.exports.export_auto ;
		    export_tags = existing_package.exports.export_tags @ package.exports.export_tags ;
		    special_export = None }
      }
    with Not_found -> package
  in
  Hashtbl.replace state.per_package package.package_name package

let check_unused_vars package =
  Hashtbl.iter (fun (context, name) (pos, is_used) ->
    if not (!is_used || List.mem name ["BEGIN"; "END"; "DESTROY"; "ISA"; "AUTOLOAD"; "EXPORT"; "EXPORT_OK"; "EXPORT_TAGS"]) then
      warn_with_pos pos (sprintf "unused %s%s::%s" (if context = I_func then "function " else "variable " ^ context2s context) package.package_name name)
  ) package.vars_declared

let arrange_global_vars_declared state =
  let h = Hashtbl.create 16 in
  Hashtbl.iter (fun (context, fq, name) pos ->
    try
      let package = Hashtbl.find state.per_package fq in
      if not (Hashtbl.mem package.vars_declared (context, name)) then
	Hashtbl.add package.vars_declared (context, name) (pos, ref false)
      (* otherwise dropping this second declaration *)
    with Not_found ->
      (* keeping it in global_vars_declared *)
      Hashtbl.add h (context, fq, name) pos
  ) state.global_vars_declared ;
  { state with global_vars_declared = h }

let get_methods_available state =
  let get_classes state =
    let l = hashtbl_collect (fun _ package ->
      match package.isa with
      | None ->
	  if Hashtbl.mem package.vars_declared (I_func, "new") then [package] else []
      | Some l ->
	  package :: List.map (fun (pkg, pos) -> 
	    try
	      Hashtbl.find state.per_package pkg
	    with Not_found -> die_with_pos pos ("bad package " ^ pkg)
	  ) l
    ) state.per_package in
    uniq l
  in
  List.iter (fun pkg ->
    Hashtbl.iter (fun (context, v) (pos, is_used) ->
      if context = I_func then
	let l = try Hashtbl.find state.methods v with Not_found -> [] in
	Hashtbl.replace state.methods v ((pos, is_used) :: l)
    ) pkg.vars_declared
  ) (get_classes state) ;
  state


let default_state() = { per_package = Hashtbl.create 16; methods = Hashtbl.create 256 ; global_vars_declared = Hashtbl.create 256; global_vars_used = ref [] }

let cache_cache = Hashtbl.create 16

let read_packages_from_cache state dir =
  if !Flags.no_cache || Hashtbl.mem cache_cache dir then () else
  try
    Hashtbl.add cache_cache dir ();
    let file = dir ^ "/.perl_checker.cache" in
    let fh = open_in file in
    let magic = input_line fh in
    if magic <> "perl_checker cache " ^ string_of_int Build.date then () else    
    let l = Marshal.from_channel fh in
    close_in fh ;

    let l = List.filter (fun pkg -> not (Hashtbl.mem state.per_package pkg.package_name)) l in

    if !Flags.verbose then print_endline_flush (sprintf "using cached packages %s from %s" (String.concat " " (List.map (fun pkg -> pkg.package_name) l)) file) ;

    List.iter (fun pkg -> 
      Info.add_a_file pkg.file_name pkg.lines_starts ;
      Hashtbl.add state.per_package pkg.package_name { pkg with from_cache = true }
    ) l
  with Sys_error _ -> ()

let write_packages_cache state dir =
  try
    let file = dir ^ "/.perl_checker.cache" in
    let fh = open_out file in
    output_string fh ("perl_checker cache " ^ string_of_int Build.date ^ "\n") ;
    let l = List.filter (fun pkg -> pkg.has_package_name) (List.map (fun pkg -> { pkg with imported = ref None }) (hashtbl_values state.per_package)) in
    Marshal.to_channel fh l [] ;
    close_out fh ;
    if !Flags.verbose then print_endline_flush ("saving cached packages in " ^ file)
  with Sys_error _ -> ()
