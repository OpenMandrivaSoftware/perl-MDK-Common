open List
open Printf
open Common

let (lines_starts : (string * int list ref) list ref) = ref []
let current_file_lines_starts = ref []
let current_file_current_line = ref 0
let current_file = ref ""

let start_a_new_file file = 
  current_file := file ;
  current_file_lines_starts := [0] ;
  lines_starts := (file, current_file_lines_starts) :: !lines_starts

let raw_pos2raw_line file a =
  let starts = map_index (fun a b -> a,b) (rev !(assoc file !lines_starts)) in
  let ((offset, line), _) = find (fun (_,(e,_)) -> e > a) (combine starts (tl starts @ [999999999, 999999999])) in
  line, offset

let pos2line (file, a, b) = 
  let line, offset = raw_pos2raw_line file a in
  file, line, a - offset + 1, b - offset + 1

let pos2s (file, a, b) = sprintf "(%s, %d, %d)" file a b

let pos2sfull pos = 
  try
    let (file, line, n1,n2) = pos2line pos in
    sprintf "File \"%s\", line %d, character %d-%d\n" file (line + 1) n1 n2
  with Not_found -> failwith ("bad position " ^ pos2s pos)

let pos2sfull_current a b = pos2sfull (!current_file, a, b)
