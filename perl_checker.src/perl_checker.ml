open Types
open Common
open Tree

let inc =
  let inc_ref = ref [] in
  let rec updir dir nb =
    if nb = 0 then dir else
    match dir with
    | "." -> String.concat "/" (times ".." nb)
    | _ -> updir (Filename.dirname dir) (nb-1)
  in
  fun file_name package_name has_package_name ->
    if !inc_ref = [] then (
      let reldir = if has_package_name then updir file_name (List.length(split_at2 ':'':' package_name)) else "." in
      let default = readlines (Unix.open_process_in "perl -le 'print foreach @INC'") in
      inc_ref := reldir :: default ;
      
      try
	ignored_packages := readlines (open_in (reldir ^ "/.perl_checker")) @ !ignored_packages
      with Sys_error _ -> ()
    );
    !inc_ref

let findfile dirs f = List.find Sys.file_exists (List.map (fun dir -> dir ^ "/" ^ f) dirs)

let rec parse_file state file =
  try
    if !Flags.verbose then print_endline_flush ("checking " ^ file) ;
    let channel = Unix.open_process_in (Printf.sprintf "expand \"%s\"" file) in
    let lexbuf = Lexing.from_channel channel in
    try
      Info.start_a_new_file file ;
      let tokens = Lexer.get_token Lexer.token lexbuf in
      let _ = Unix.close_process_in channel in
      let t = Parser_helper.parse_tokens Parser.prog tokens (Some lexbuf) in
      let packages, required_packages = get_global_info_from_package t in
      List.fold_left (fun (required_packages, state) package ->
	Tree.get_vars_declaration state package ;
	let state = Tree.add_package_to_state state package in
	List.map (fun (s, (_, pos)) -> s, pos) package.uses @ required_packages, state
      ) (required_packages, state) packages
    with Failure s -> (
      print_endline_flush s ;
      exit 1
     )
  with 
  | Not_found -> internal_error "runaway Not_found"

and parse_package_if_needed state (package_name, pos) =
  if List.mem_assoc package_name state.per_package then [], state else
  try
    let package = snd (List.hd state.per_package) in
    let inc = !Tree.use_lib @ inc package.file_name package.package_name package.has_package_name in
    if List.mem package_name !ignored_packages then [], state
    else
      let rel_file = String.concat "/" (split_at2 ':'':' package_name) ^ ".pm" in
      let file = findfile inc rel_file in
      if List.mem file state.files_parsed 
      then [], state (* already seen, it happens when many files have the same package_name *)
      else parse_file state file
  with Not_found -> 
    Tree.warn_with_pos pos (Printf.sprintf "can't find package %s" package_name) ;
    [], state

let rec parse_required_packages state = function
  | [] -> state
  | e :: l ->
      let el, state = parse_package_if_needed state e in
      parse_required_packages state (el @ l)

let parse_options =
  let args_r = ref [] in
  let restrict_to_files = ref false in
  let options = [
    "-v", Arg.Set Flags.verbose, "  be verbose" ;
    "-q", Arg.Set Flags.quiet, "  be quiet" ;
    "--restrict-to-files", Arg.Set restrict_to_files, "  only display warnings concerning the file(s) given on command line" ;
  ] in
  let usage = "Usage: perl_checker [-v] [-q] <files>\nOptions are:" in
  Arg.parse options (lpush args_r) usage;

  let files = if !args_r = [] then ["../t.pl"] else !args_r in
  let required_packages, state = collect_withenv parse_file default_state files in

  if !restrict_to_files then Common.print_endline_flush_quiet := true ;
  let state = parse_required_packages state required_packages in
  if !restrict_to_files then Common.print_endline_flush_quiet := false ;

  let l = List.map snd state.per_package in
  (* HACK: skip ignored_packages. Some package may have appeared in ignored_packages due to the xs bootstrap hack *)
  let l = List.filter (fun pkg -> not (List.mem pkg.package_name !ignored_packages)) l in

  let l = if !restrict_to_files then List.filter (fun pkg -> List.mem pkg.file_name files) l else l in

  List.iter (check_tree state) l
