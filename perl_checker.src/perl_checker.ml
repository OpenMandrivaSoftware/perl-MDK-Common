open Types
open Common
open Tree
open Global_checks

let search_basedir file_name nb =
  let dir = Filename.dirname file_name in
  let config = Config_file.read dir in
  let nb = some_or config.Config_file.basedir nb in
  updir dir nb

let basedir = ref ""
let set_basedir state package =
  if !basedir = "" then
    let nb = List.length (split_at2 ':'':' package.package_name) - 1 in
    let dir = search_basedir package.file_name nb in
    lpush Tree.use_lib dir ;
    Config_file.read_any dir 1 ;
    read_packages_from_cache state dir ;
    if !Flags.verbose then print_endline_flush ("basedir is " ^ dir);
    basedir := dir

let mtime f = int_of_float ((Unix.stat f).Unix.st_mtime)

let rec parse_file from_basedir state file =
  try
    if !Flags.verbose then print_endline_flush ("checking " ^ file) ;
    let build_time = int_of_float (Unix.time()) in
    let command = 
      match !Flags.expand_tabs with
      | Some width -> "expand -t " ^ string_of_int width
      | None -> "cat" in
    let channel = Unix.open_process_in (Printf.sprintf "%s \"%s\"" command file) in
    let lexbuf = Lexing.from_channel channel in
    try
      Info.start_a_new_file file ;
      let tokens = Lexer.get_token Lexer.token lexbuf in
      let _ = Unix.close_process_in channel in
      let t = Parser_helper.parse_tokens Parser.prog tokens (Some lexbuf) in
      let packages = get_global_info_from_package from_basedir build_time t in
      let required_packages =
	collect (fun package ->
	  get_vars_declaration state.global_vars_declared package ;
	  Global_checks.add_package_to_state state package ;
	  set_basedir state package ;
	  package.required_packages
        ) packages in
      required_packages, state
    with Failure s -> (
      print_endline_flush s ;
      exit 1
     )
  with 
  | Not_found -> internal_error "runaway Not_found"

and parse_package_if_needed state (package_name, pos) =
  if List.mem package_name !Config_file.ignored_packages then [], state else
  let splitted = split_at2 ':'':' package_name in
  let rel_file = String.concat "/" splitted ^ ".pm" in

  (*print_endline_flush ("wondering about " ^ package_name) ;*)
  try
    let dir = findfile (Build.fake_packages_dir :: !use_lib) rel_file in
    let file = dir ^ "/" ^ rel_file in
    Config_file.read_any (Filename.dirname file) (List.length splitted) ;
    let already_done =
      try
	let pkg = Hashtbl.find state.per_package package_name in
	if pkg.from_cache then
	  if pkg.build_time > mtime file then (
	    Hashtbl.replace state.per_package package_name { pkg with from_cache = false };
	    (*print_endline_flush (package_name ^ " wants " ^ String.concat " " (List.map fst pkg.required_packages)) ; *)
	    Some pkg.required_packages
	  ) else (
	    if !Flags.verbose then print_endline_flush (Printf.sprintf "cached version of %s is outdated, re-parsing" file);
	    Hashtbl.remove state.per_package package_name ; (* so that check on file name below doesn't need to check from_cache *)
	    None
	  )
	else Some []
      with Not_found -> None in
    match already_done with
    | Some required_packages -> required_packages, state
    | None ->
	if hashtbl_exists (fun _ pkg -> pkg.file_name = file) state.per_package
	then [], state (* already seen, it happens when many files have the same package_name *)
	else parse_file (dir = !basedir) state file
  with Not_found -> 
    warn_with_pos pos (Printf.sprintf "can't find package %s" package_name) ;
    [], state

let rec parse_required_packages state = function
  | [] -> state
  | e :: l ->
      let el, state = parse_package_if_needed state e in
      parse_required_packages state (el @ l)


let parse_options =
  let args_r = ref [] in
  let restrict_to_files = ref false in

  let pot_file = ref "" in
  let generate_pot_chosen file =
    Flags.generate_pot := true ;
    Flags.expand_tabs := None ;
    pot_file := file
  in
  let options = [
    "-v", Arg.Set Flags.verbose, "  be verbose" ;
    "-q", Arg.Set Flags.quiet, "  be quiet" ;
    "-t", Arg.Int (fun i -> Flags.expand_tabs := Some i), "  set the tabulation width (default is 8)" ;
    "--check-unused", Arg.Set Flags.check_unused_global_vars, "  check unused global functions & variables" ;
    "--restrict-to-files", Arg.Set restrict_to_files, "  only display warnings concerning the file(s) given on command line" ;
    "--no-cache", Arg.Set Flags.no_cache, "  do not use cache" ;
    "--generate-pot", Arg.String generate_pot_chosen, "" ;
  ] in
  let usage = "Usage: perl_checker [-v] [-q] <files>\nOptions are:" in
  Arg.parse options (lpush args_r) usage;

  let files = if !args_r = [] then ["../t.pl"] else !args_r in

  let required_packages, state = collect_withenv (parse_file true) (default_state()) files in
  let required_packages = uniq_ (fun (a,_) (b,_) -> a = b) required_packages in

  if !Flags.generate_pot then Parser_helper.generate_pot !pot_file else (

  if !restrict_to_files then Common.print_endline_flush_quiet := true ;
  let state = parse_required_packages state required_packages in
  if !restrict_to_files then Common.print_endline_flush_quiet := false ;

  let state = arrange_global_vars_declared state in

  write_packages_cache state !basedir ;

  let state = Global_checks.get_methods_available state in

  let l = List.map snd (hashtbl_to_list state.per_package) in
  let l = List.filter (fun pkg -> not pkg.from_cache && pkg.from_basedir) l in
  (* HACK: skip ignored_packages. Some package may have appeared in ignored_packages due to the xs bootstrap hack *)
  let l = List.filter (fun pkg -> not (List.mem pkg.package_name !Config_file.ignored_packages)) l in

  let l = if !restrict_to_files then List.filter (fun pkg -> List.mem pkg.file_name files) l else l in

  List.iter (Global_checks.check_tree state) l;
  if !Flags.check_unused_global_vars then List.iter Global_checks.check_unused_vars l 
  )
