open Types
open Common

let bpos = -1, -1


let not_complex = function
  | Call_op("?:", _) -> false
  | _ -> true

let not_simple = function
  | Num _ | Ident _ | Deref(_, Ident _) -> false
  | _ -> true

let string_of_Ident = function
  | Ident(None, s, _) -> s
  | Ident(Some fq, s, _) -> fq ^ "::" ^ s
  | _ -> internal_error "string_of_Ident"


let msg_with_pos (start, end_) msg = Info.pos2sfull_current start end_ ^ msg
let die_with_pos raw_pos msg = failwith      (msg_with_pos raw_pos msg)
let warn         raw_pos msg = prerr_endline (msg_with_pos raw_pos msg)

let die_rule msg = die_with_pos (Parsing.symbol_start(), Parsing.symbol_end()) msg
let debug msg = if false then prerr_endline msg

let raw_pos2pos(a, b) = !Info.current_file, a, b
let get_pos (_, (_, pos)) = raw_pos2pos pos

let warn_too_many_space start = warn (start, start) "you should have only one space here"
let warn_no_space	start = warn (start, start) "you should have a space here"
let warn_cr		start = warn (start, start) "you should not have a carriage-return (\\n) here"
let warn_space		start = warn (start, start) "you should not have a space here"


let sp_0(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0 -> ()
  | Space_1
  | Space_n -> warn_space start
  | Space_cr -> warn_cr start

let sp_0_or_cr(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0 -> ()
  | Space_1
  | Space_n -> warn_space start
  | Space_cr -> ()

let sp_1(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space start
  | Space_1 -> ()
  | Space_n -> warn_too_many_space start
  | Space_cr -> warn_cr start

let sp_n(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space start
  | Space_1 -> ()
  | Space_n -> ()
  | Space_cr -> warn_cr start

let sp_p(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space start
  | Space_1 -> ()
  | Space_n -> ()
  | Space_cr -> ()

let sp_cr(_, (spaces, (start, _))) =
  match spaces with
  | Space_none -> ()
  | Space_0
  | Space_1
  | Space_n -> warn (start, start) "you should have a carriage-return (\\n) here"
  | Space_cr -> ()

let sp_same (_, (spaces1, _) as ter1) (_, (spaces2, _) as ter2) =
  if spaces1 <> Space_0 then sp_p ter2
  else if spaces2 <> Space_0 then sp_p ter1

let op s (_, both) = ((), both), s
let op_p s e = sp_p e ; op s e

let call_op((prev_ter, op), ter, para) = 
  sp_same prev_ter ter ;
  Call_op(op, para)

let check_lines_after_BRACKET (l, both) =
  (match l with Semi_colon :: _ -> sp_0 | _ -> sp_p)(l, both)

let check_word_alone (word, _) =
  if string_of_Ident word = "time" then die_rule "please use time() instead of time";
  word

let check_parenthesized_first_argexpr word (e, (_, (start, _)) as ex) =
  let want_space = word.[0] = '-' in
  match e with
  | List[List[_]] :: l ->
      if want_space then
	if l = [] then sp_n(ex) else die_with_pos (start, start) "can't handle this nicely"
      else
	if l = [] then sp_0(ex) else die_with_pos (start, start) "you must not have a space here"
  | _ -> 
      if word = "time" then die_rule "please use time() instead of time";
      sp_p(ex)

let check_foreach (s, (_, pos)) = if s = "for"     then warn pos "write \"foreach\" instead of \"for\""
let check_for     (s, (_, pos)) = if s = "foreach" then warn pos "write \"for\" instead of \"foreach\""

let check_no_paren f_name (e, (_, pos)) =
  match e with
  | List[List[List[e]]] when not_complex e -> warn pos (Printf.sprintf "''... %s (...)'' can be written ''... %s ...''" f_name f_name)
  | _ -> ()

let to_Ident ((fq, name), (_, pos)) = Ident(fq, name, raw_pos2pos pos)
let to_String (s, (_, pos)) = String(s, raw_pos2pos pos)

let rec only_one (l, (spaces, pos)) =
  match l with
  | [List l'] -> only_one (l', (spaces, pos))
  | [e] -> e
  | [] -> die_with_pos pos "you must give one argument"
  | _  -> die_with_pos pos "you must give only one argument"

let only_one_in_List (e, both) =
  match e with
  | List l -> only_one(l, both)
  | _ -> e

let array_ident_to_hash_ident (e, (_, pos)) =
  match e with
  | Deref(I_array, e) -> Deref(I_hash, e)
  | _ -> die_with_pos pos "internal error (array_ident_to_hash_ident)"

let from_PATTERN ((s, opts), (_, pos)) = [ String(s, raw_pos2pos pos) ; String(opts, raw_pos2pos pos) ]
let from_PATTERN_SUBST ((s1, s2, opts), (_, pos)) = [ String(s1, raw_pos2pos pos) ; String(s2, raw_pos2pos pos) ; String(opts, raw_pos2pos pos) ]
  
let to_List = function
  | [e] -> e
  | l -> List l

let sub_declaration (name, proto) body = Sub_declaration(name, proto, body)

let var_dollar_ = Deref(I_scalar, Ident(None, "_", raw_pos2pos bpos))
let var_STDOUT = Deref(I_star, Ident(None, "STDOUT", raw_pos2pos bpos))
