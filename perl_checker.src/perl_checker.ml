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
let set_basedir per_files file =
  if !basedir = "" then
    let nb = List.length (split_at2 ':'':' (List.hd file.packages).package_name) - 1 in
    let dir = search_basedir file.file_name nb in
    lpush Tree.use_lib dir ;
    Config_file.read_any dir 1 ;
    read_packages_from_cache per_files dir ;
    if !Flags.verbose then print_endline_flush ("basedir is " ^ dir);
    basedir := dir

let rec parse_file from_basedir require_name per_files file =
  try
    if !Flags.verbose then print_endline_flush_always ("parsing " ^ file) ;
    let build_time = Unix.time() in
    let command = 
      match !Flags.expand_tabs with
      | Some width -> "expand -t " ^ string_of_int width
      | None -> "cat" in
    let channel = Unix.open_process_in (Printf.sprintf "%s \"%s\"" command file) in
    let lexbuf = Lexing.from_channel channel in
    try
      Info.start_a_new_file file ;
      let tokens = Lexer.get_token Lexer.token lexbuf in
      if not Build.debugging then ignore (Unix.close_process_in channel) ;
      let t = Parser_helper.parse_tokens Parser.prog tokens (Some lexbuf) in
      let per_file = get_global_info_from_package from_basedir require_name build_time t in
      set_basedir per_files per_file ;
      Global_checks.add_file_to_files per_files per_file ;

      let required_packages = collect (fun package -> package.required_packages) per_file.packages in
      required_packages, per_files
    with Failure s -> (
      print_endline_flush_always s ;
      exit 1
     )
  with 
  | Not_found -> internal_error "runaway Not_found"

and parse_package_if_needed per_files (package_name, pos) =
  if List.mem package_name !Config_file.ignored_packages then [], per_files else
  let splitted = split_at2 ':'':' package_name in
  let rel_file = String.concat "/" splitted ^ ".pm" in

  (*print_endline_flush ("wondering about " ^ package_name) ;*)
  try
    let dir = findfile (Build.fake_packages_dir :: !use_lib) rel_file in
    let file = Info.file_to_absolute_file (dir ^ "/" ^ rel_file) in
    Config_file.read_any (Filename.dirname file) (List.length splitted) ;
    let already_done =
      try
	let per_file = Hashtbl.find per_files file in
	Some (collect (fun pkg -> pkg.required_packages) per_file.packages)
      with Not_found -> None in
    match already_done with
    | Some required_packages -> required_packages, per_files
    | None -> parse_file (dir = !basedir) (Some package_name) per_files file
  with Not_found -> 
    warn_with_pos_always pos (Printf.sprintf "can't find package %s" package_name) ;
    [], per_files

let rec parse_required_packages state already_done = function
  | [] -> state, already_done
  | e :: l ->
      if List.mem e already_done then
	parse_required_packages state already_done l
      else
	let el, state = parse_package_if_needed state e in
	parse_required_packages state (e :: already_done) (el @ l)


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

  let files = if !args_r = [] && Build.debugging then ["../t.pl"] else !args_r in
  let files = List.map Info.file_to_absolute_file files in

  let required_packages, per_files = collect_withenv (parse_file true None) (default_per_files()) files in
  let required_packages = uniq_ (fun (a,_) (b,_) -> a = b) required_packages in

  if !Flags.generate_pot then Parser_helper.generate_pot !pot_file else (

  if !restrict_to_files then Common.print_endline_flush_quiet := true ;
  let per_files, required_packages = parse_required_packages per_files [] required_packages in
  let l_required_packages = List.map fst required_packages in
  if !restrict_to_files then Common.print_endline_flush_quiet := false ;

  write_packages_cache per_files !basedir ;

  (* removing non needed files from per_files (those files come from the cache) *)
  List.iter (fun k ->
    let per_file = Hashtbl.find per_files k in
    if per_file.require_name <> None && not (List.mem (some per_file.require_name) l_required_packages) && not (List.mem per_file.file_name files) then
      Hashtbl.remove per_files k
  ) (hashtbl_keys per_files);

  let state = default_state per_files in

  Hashtbl.iter (fun _ per_file -> List.iter (add_package_to_state state) per_file.packages) per_files ;

  let state = 
    let global_vars_declared = Hashtbl.create 16 in
    let package_name_to_file_name = hashtbl_collect (fun _ per_file -> List.map (fun pkg -> pkg.package_name, per_file.file_name) per_file.packages) per_files in
    Hashtbl.iter (fun _ pkg -> 
      let file_name = List.assoc pkg.package_name package_name_to_file_name in
      get_vars_declaration global_vars_declared file_name pkg
    ) state.per_packages ;
    arrange_global_vars_declared global_vars_declared state
  in

  let state = Global_checks.get_methods_available state in

  let l = hashtbl_values per_files in
  let l = if !restrict_to_files then List.filter (fun file -> List.mem file.file_name files) l else l in

  let l = uniq (collect (fun file -> List.map (fun pkg -> pkg.package_name) file.packages) l) in
  let l = List.map (Hashtbl.find state.per_packages) l in
  
  (* HACK: skip ignored_packages. Some package may have appeared in ignored_packages due to the xs bootstrap hack *)
  let l = List.filter (fun pkg -> not (List.mem pkg.package_name !Config_file.ignored_packages)) l in

  List.iter (Global_checks.check_tree state) l;

  if !Flags.check_unused_global_vars then List.iter Global_checks.check_unused_vars l 

  )
