open Types
open Common
open Printf
open Config_file
open Parser_helper

type special_export = Re_export_all | Fake_export_all

type exports = { 
    export_ok : (context * string) list ;
    export_auto : (context * string) list ;
    export_tags : (string * (context * string) list) list ;
    special_export : special_export option ;
  }

type uses = (string * ((context * string) list option * pos)) list

type prototype = {
    proto_nb_min : int ;
    proto_nb_max : int option ;
  }

type per_package = {
    file_name : string ;
    package_name : string ; has_package_name : bool ;
    vars_declared : (context * string, pos * bool ref * prototype option) Hashtbl.t ;
    imported : ((context * string) * (string * bool ref * prototype option)) list option ref ;
    exports : exports ;
    uses : uses ;
    required_packages : (string * pos) list ;
    body : fromparser list ;
    isa : (string * pos) list option ;
    lines_starts : int list ;
    build_time : int ;
    from_cache : bool ;
    from_basedir : bool ;
  }

let anonymous_package_count = ref 0
let empty_exports = { export_ok = []; export_auto = []; export_tags = []; special_export = None }
let use_lib = ref (readlines (Unix.open_process_in "perl -le 'print foreach @INC'"))

let ignore_package pkg = 
  if !Flags.verbose then print_endline_flush ("ignoring package " ^ pkg);
  lpush ignored_packages pkg

let die_with_pos pos msg = failwith (Info.pos2sfull pos ^ msg)
let warn_with_pos pos msg = print_endline_flush (Info.pos2sfull pos ^ msg)

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

let from_qw_raw = function
  | Call_op("qw", [ Raw_string(s, pos)], _) -> 
      List.map (fun symbol -> symbol, pos) (words s)
  | String([s, List []], pos) -> [ s, pos ]
  | String(_, pos) ->
      warn_with_pos pos "not recognised yet" ;
      []
  | Raw_string(s, pos) ->
      [ s, pos ]
  | List [] -> []
  | List [ List l ] ->
      List.map (function
	| String([s, List []], pos)
	| Raw_string(s, pos) -> s, pos
	| Ident(_, _, pos) as ident -> string_of_Ident ident, pos
      ) l
  | _ -> internal_error "from_qw_raw"

let from_qw e =
  List.map (fun (s, pos) -> 
    let context, s' = s2context s in
    let context =
      match context with
      | I_raw -> if s'.[0] = ':' then I_raw else I_func
      | I_func -> warn_with_pos pos "weird, exported name with a function context especially given"; I_func
      | _ -> context 
    in context, s'
  ) (from_qw_raw e)

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
    | Perl_checker_comment("EXPORT-ALL",    _) -> { exports with special_export = Some Fake_export_all }

    | List [ Call_op("=", [ Deref(I_array, Ident(None, "EXPORT_OK", _)); v ], pos)]
    | List [ Call_op("=", [ My_our("our", [(I_array, "EXPORT_OK")], _);  v ], pos)] ->
	if exports.export_ok <> [] then warn_with_pos pos "weird, @EXPORT_OK set twice" ;
	(match v with
	| Call(Deref(I_func, Ident(None, "map", _)), 
	       [ Anonymous_sub(_, Block [List [Deref(I_array, Deref(I_scalar, Ident (None, "_", _)))]], _);
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

      (* $EXPORT_TAGS{all} = [ map { @$_ } values %EXPORT_TAGS ]; *)
    | List [Call_op("=", [
		    Deref_with(I_hash, I_scalar, Ident(None, "EXPORT_TAGS", _), Raw_string("all", _));
		    Ref(I_array,
			List[List[
			     Call(Deref(I_func, Ident(None, "map", _)),
				  [Anonymous_sub(_, Block [List [Deref(I_array, Deref(I_scalar, Ident(None, "_", _)))]], _);
				   Call(Deref(I_func, Ident(None, "values", _)), [Deref(I_hash, Ident(None, "EXPORT_TAGS", _))])])
			   ]])
		  ], _)] ->
		    { exports with export_tags = (":all", collect snd exports.export_tags) :: exports.export_tags }

    | List (My_our _ :: _) ->
	let _,_ = e,e in
	exports
    | _ -> exports
  ) empty_exports t

let uses_external_package = function
  | "vars" | "MDK::Common::Globals" | "Exporter" | "diagnostics" | "strict" | "lib" | "POSIX" | "Gtk" | "Storable"
  | "Config" | "Socket" | "IO::Socket" | "DynaLoader" | "Data::Dumper" | "Time::localtime" | "Expect" -> true
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
	let para = if l = [] then None else Some(collect from_qw l) in
	(package, (para, pos)) :: uses
    | _ -> uses
  ) [] t

let get_isa t =
  List.fold_left (fun (isa, exporter) e ->
    match e with
    | List [ Call_op("=", [ Deref(I_array, Ident(None, "ISA", pos)) ; classes ], _) ]
    | List [ Call_op("=", [ My_our("our", [ I_array, "ISA" ], pos) ; classes ], _) ] ->
	if isa <> None || exporter <> None then die_with_pos pos "@ISA set twice";
	let special, l = List.partition (fun (s, _) -> s = "DynaLoader" || s = "Exporter") (from_qw_raw classes) in
	let exporter = if List.mem_assoc "Exporter" special then Some pos else None in
	let isa = if l = [] && special <> [] then None else Some l in
	isa, exporter
    | _ -> isa, exporter
  ) (None, None) t

let read_xs_extension_from_c global_vars_declared package pos =
  try
    let cfile = Filename.chop_extension package.file_name ^ ".c" in
    let prefix = "newXS(\"" ^ package.package_name ^ "::" in
    ignore (fold_lines (fun in_bootstrap s ->
      if in_bootstrap then
	(try
	  let offset = strstr s prefix + String.length prefix in
	  let end_ = String.index_from s offset '"' in
	  let ident = String.sub s offset (end_ - offset) in
	  match split_name_or_fq_name ident with
	  | None, ident -> Hashtbl.replace package.vars_declared (I_func, ident) (pos, ref false, None)
	  | Some fq, ident -> 
	      let fq = package.package_name ^ "::" ^ fq in
	      Hashtbl.replace global_vars_declared (I_func, fq, ident) (pos, None)
	 with Not_found -> ());
      in_bootstrap || str_contains s "XS_VERSION_BOOTCHECK"
    ) false (open_in cfile));
    true
  with Invalid_argument _ | Sys_error _ -> false

let findfile dirs f = List.find (fun dir -> Sys.file_exists (dir ^ "/" ^ f)) dirs

let read_xs_extension_from_so global_vars_declared package pos =
  try
    let splitted = split_at2 ':'':' package.package_name in
    let rel_file = String.concat "/" ("auto" :: splitted @ [ last splitted ]) ^ ".so" in
    let so = (findfile !use_lib rel_file) ^ "/" ^ rel_file in
    let channel = Unix.open_process_in (Printf.sprintf "nm --defined-only -D \"%s\"" so) in
    fold_lines (fun () s ->
      let s = skip_n_char 11 s in
      if str_begins_with s "XS_" then
	let s = skip_n_char 3 s in
	let len = String.length s in
	let rec find_package_name accu i =
	  try
	    let i' = String.index_from s i '_' in
	    let accu = String.sub s i (i'-i) :: accu in
	    if i' + 1 < len && s.[i'+1] = '_' then
	      find_package_name accu (i' + 2)
	    else
	      List.rev accu, skip_n_char (i'+1) s
	  with Not_found -> List.rev accu, skip_n_char i s
	in
	let fq, name = find_package_name [] 0 in
	Hashtbl.replace global_vars_declared (I_func, String.concat "::" fq, name) (pos, None)
    ) () channel;
    let _ = Unix.close_process_in channel in
    true
  with Not_found -> false

let has_proto perl_proto body = 
  match perl_proto with
  | Some "" -> Some([], raw_pos2pos bpos, [body])
  | _ -> 
      match body with
      | Block [] ->
	  Some([ I_array, "_empty" ], raw_pos2pos bpos, [])
      | Block (List [Call_op ("=", [My_our ("my", mys, mys_pos); Deref(I_array, Ident(None, "_", _))], _pos)] :: body) ->
	  Some(mys, mys_pos, body)
      | _ -> None

let get_proto perl_proto body =
  map_option (fun (mys, pos, _) ->
    let scalars, others = break_at (fun (context, _) -> context <> I_scalar) mys in
    (match others with
    | (I_array, _) :: _ :: _ -> warn_with_pos pos "an array must be the last variable in a prototype"
    | (I_hash, _) :: _ :: _ -> warn_with_pos pos "an hash must be the last variable in a prototype"
    | _ -> ());
    let is_optional (_, s) = String.length s > 2 && (s.[0] = 'o' || s.[0] = 'b') && s.[1] = '_' in
    let must_have, optional = break_at is_optional scalars in
    if not (List.for_all is_optional optional) then
      warn_with_pos pos "an non-optional argument must not follow an optional argument";
    let min = List.length must_have in    
    { proto_nb_min = min; proto_nb_max = if others = [] then Some(min + List.length optional) else None }
  ) (has_proto perl_proto body)

let get_vars_declaration global_vars_declared package = 
  List.iter (function
    | Sub_declaration(Ident(None, name, pos), perl_proto, body, _) ->
	Hashtbl.replace package.vars_declared (I_func, name) (pos, ref false, get_proto perl_proto body)
    | Sub_declaration(Ident(Some fq, name, pos), perl_proto, body, _) ->
	Hashtbl.replace global_vars_declared (I_func, fq, name) (pos, get_proto perl_proto body)

    | List [ Call_op("=", [My_our("our", ours, pos); _], _) ]
    | List [ Call_op("=", [My_our("local", ([ I_scalar, "_" ] as ours), pos); _], _) ]
    | List [ My_our("our", ours, pos) ]
    | My_our("our", ours, pos) ->
	List.iter (fun (context, name) -> Hashtbl.replace package.vars_declared (context, name) (pos, ref false, None)) ours

    | Use(Ident(Some "MDK::Common", "Globals", pos), [ String _ ; ours ])
    | Use(Ident(None, "vars", pos), [ours]) -> 
	List.iter (fun (context, name) -> Hashtbl.replace package.vars_declared (context, name) (pos, ref false, None)) (from_qw ours)
    | Use(Ident(None, "vars", pos), _) -> 
	die_with_pos pos "usage: use vars qw($var func)"

    | List [ Method_call(Raw_string(pkg, pos), Raw_string("bootstrap", _), _) ] ->
	if pkg <> package.package_name then
	  warn_with_pos pos "strange bootstrap (the package name is not the same as the current package)"
	else
	  if not (read_xs_extension_from_c global_vars_declared package pos) then 
	    if not (read_xs_extension_from_so global_vars_declared package pos) then
	      ignore_package pkg
    | _ -> ()
  ) package.body

let rec fold_tree f env e = 
 match f env e with
  | Some env -> env
  | None ->
  match e with
  | Anonymous_sub(_, e', _)
  | Ref(_, e')
  | Deref(_, e')
    -> fold_tree f env e'

  | Diamond(e')
    -> fold_tree_option f env e'

  | String(l, _)
    -> List.fold_left (fun env (_, e) -> fold_tree f env e) env l

  | Sub_declaration(e1, _, e2, _)
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


let get_global_info_from_package from_basedir build_time t =
  let current_packages = get_current_package t in
  List.map (fun (current_package, t) ->
    let exports = get_exported t in
    let exporting_something() = exports.export_ok <> [] || exports.export_auto <> [] || exports.export_tags <> [] || exports.special_export = Some Re_export_all in

    let package_name =
      match current_package with
      | None -> 
	  if exporting_something() then
	    die_with_pos (!Info.current_file, 0, 0) "file with no package name wants to export!"
	  else
	    (incr anonymous_package_count ; sprintf "anonymous%d" !anonymous_package_count)
      | Some name -> name
    in
    let isa, exporter = get_isa t in
    (match exporter with
    | None ->
	if exporting_something() then warn_with_pos (!Info.current_file, 0, 0) "you must have \"@ISA = qw(Exporter)\" to EXPORT something"
    | Some pos ->
	if not (exporting_something()) then warn_with_pos pos "Inheritating from Exporter without EXPORTing anything");

    let uses = List.rev (get_uses t) in
    let required_packages = List.map (fun (s, (_, pos)) -> s, pos) uses in
    let required_packages = List.fold_left (fold_tree (fun l -> 
      function
	| Perl_checker_comment(s, pos) when str_begins_with s "require " ->
	    Some((skip_n_char 8 s, pos) :: l)
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
      required_packages = required_packages ;
      body = t ;
      isa = isa ;
      lines_starts = !Info.current_file_lines_starts ;
      build_time = build_time ;
      from_cache = false ;
      from_basedir = from_basedir ;
    }
  ) current_packages
