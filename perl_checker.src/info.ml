open List
open Printf
open Common

let (lines_starts : (string, int list) Hashtbl.t) = Hashtbl.create 4
let current_file_lines_starts = ref []
let current_file_current_line = ref 0
let current_file = ref ""

let start_a_new_file file = 
  if !current_file <> "" then Hashtbl.add lines_starts !current_file !current_file_lines_starts ;
  current_file := file ;
  current_file_lines_starts := [0]

let add_a_file file file_lines_starts = Hashtbl.replace lines_starts file file_lines_starts

let get_lines_starts_for_file file =
  if file = !current_file then !current_file_lines_starts else Hashtbl.find lines_starts file

let cwd = expand_symlinks (Unix.getcwd())

let file_to_absolute_file file =
  let abs_file = 
    if file.[0] = '/' then file else 
    if file = "." then cwd else cwd ^ "/" ^ file
  in
  expand_symlinks abs_file

let absolute_file_to_file = 
  let s1 = Filename.dirname cwd in
  if String.length s1 < 4 then (fun x -> x) else
  let short_cwd = 
    let s2 = Filename.dirname s1 in
    if String.length s2 < 4 then s1 else 
    let s3 = Filename.dirname s2 in (* allow up to ../../../xxx *)
    if String.length s3 < 4 then s2 else s3 in
  memoize (fun abs_file ->
    if str_begins_with (short_cwd ^ "/") abs_file then
      let rec to_file rel cwd =
	if str_begins_with (cwd ^ "/") abs_file then
	  rel ^ skip_n_char_ (String.length cwd + 1) 0 abs_file
	else
	  to_file ("../" ^ rel) (Filename.dirname cwd)
      in
      to_file "" cwd  
    else
      abs_file)

let raw_pos2raw_line file a =
  let starts = map_index (fun a b -> a,b) (rev (get_lines_starts_for_file file)) in
  let ((offset, line), _) = find (fun (_,(e,_)) -> e > a) (combine starts (tl starts @ [999999999, 999999999])) in
  line, offset

let pos2line (file, a, b) = 
  let line, offset = raw_pos2raw_line file a in
  file, line, a - offset + 1, b - offset + 1

let pos2s (file, a, b) = sprintf "(%s, %d, %d)" file a b

let pos2sfull pos = 
  try
    let file, line, n1, n2 = pos2line pos in
    sprintf "File \"%s\", line %d, character %d-%d\n" (absolute_file_to_file file) (line + 1) n1 n2
  with Not_found -> failwith ("bad position " ^ pos2s pos)

let pos2s_for_po pos =
  let file, line, _, _ = pos2line pos in
  absolute_file_to_file file ^ ":" ^ string_of_int (line + 1)

let is_on_same_line file (a,b) =
  let line_a, _ = raw_pos2raw_line file a in
  let line_b, _ = raw_pos2raw_line file b in
  line_a = line_b

let is_on_same_line_current (a,b) = is_on_same_line !current_file (a,b)
let pos2sfull_current a b = pos2sfull (!current_file, a, b)
