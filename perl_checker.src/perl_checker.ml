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
	ignored_packages := readlines (open_in (reldir ^ "/.perl_checker"))
      with Sys_error _ -> ()
    );
    !inc_ref

let findfile dirs f = List.find Sys.file_exists (List.map (fun dir -> dir ^ "/" ^ f) dirs)

let rec parse_file state file =
  try
    if !Flags.verbose then prerr_endline ("checking " ^ file) ;
    let lexbuf = Lexing.from_channel (Unix.open_process_in (Printf.sprintf "expand \"%s\"" file)) in
    try
      Info.start_a_new_file file ;
      let tokens = Lexer.get_token Lexer.token lexbuf in
      let t = Parser_helper.parse_tokens Parser.prog tokens (Some lexbuf) in
      let package = get_global_info_from_package t in
      Tree.get_global_vars_declaration state package ;
      let state = { state with per_package = (package.package_name, package) :: state.per_package } in
      let state = List.fold_left parse_package_if_needed state package.uses in
      state
    with Failure s -> (
      prerr_endline s ;
      exit 1
     )
  with _ -> failwith ("bad file " ^ file)

and parse_package_if_needed state (package_name, (_, pos)) =
  if List.mem_assoc package_name state.per_package then state else
  try
    let package = snd (List.hd state.per_package) in
    let inc = inc package.file_name package.package_name package.has_package_name in
    if List.mem package_name !ignored_packages then state 
    else
      let file = String.concat "/" (split_at2 ':'':' package_name) ^ ".pm" in
      parse_file state (findfile inc file)
  with Not_found -> 
    Tree.warn_with_pos pos (Printf.sprintf "can't find package %s" package_name) ;
    state


let parse_options =
  let args_r = ref [] in
  let options = [
    "-v", Arg.Set Flags.verbose, "  be verbose" ;
    "-q", Arg.Set Flags.quiet, "  be quiet" ;
  ] in
  let usage = "Usage: perl_checker [-v] [-q] <files>\nOptions are:" in
  Arg.parse options (lpush args_r) usage;

  let args = if !args_r = [] then (Unix.chdir "/home/pixel/cooker/gi/perl-install" ; ["/home/pixel/cooker/gi/perl-install/t.pl"]) else !args_r in
  let state = List.fold_left parse_file default_state args in
  List.iter (check_tree state) (List.map snd state.per_package)
