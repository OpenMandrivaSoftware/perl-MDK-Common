open Types
open Common
open Printf

let bpos = -1, -1

let raw_pos2pos(a, b) = !Info.current_file, a, b
let pos_range (_, (_, (a1, b1))) (_, (_, (a2, b2))) = raw_pos2pos((if a1 = -1 then a2 else a1), (if b2 = -1 then b1 else b2))
let sp_pos_range (_, (space, (a1, b1))) (_, (_, (a2, b2))) = space, ((if a1 = -1 then a2 else a1), (if b2 = -1 then b1 else b2))
let get_pos (_, (_, pos)) = raw_pos2pos pos
let var_dollar_ = Deref(I_scalar, Ident(None, "_", raw_pos2pos bpos))
let var_STDOUT = Deref(I_star, Ident(None, "STDOUT", raw_pos2pos bpos))

let is_parenthesized = function
  | List[List[_]] -> true
  | _ -> false

let un_parenthesize = function
  | List[List[e]] -> e
  | _ -> internal_error "un_parenthesize"

let rec un_parenthesize_full = function
  | List[e] -> un_parenthesize_full e
  | e -> e

let not_complex e =
  if is_parenthesized e then true else
  let rec not_complex_ op = function
    | Call_op("?:", _) -> false
    | Call_op(op', l) -> op <> op' && List.for_all (not_complex_ op') l
    | e -> not (is_parenthesized e)
  in not_complex_ "" (un_parenthesize_full e)

let not_simple = function
  | Num _ | Ident _ | Deref(_, Ident _) -> false
  | _ -> true

let string_of_Ident = function
  | Ident(None, s, _) -> s
  | Ident(Some fq, s, _) -> fq ^ "::" ^ s
  | _ -> internal_error "string_of_Ident"

let from_scalar (e, _) =
  match e with
  | Deref(I_scalar, ident) -> ident
  | _ -> internal_error "from_scalar"

let from_array (e, _) =
  match e with
  | Deref(I_array, ident) -> ident
  | _ -> internal_error "from_array"

let msg_with_rawpos (start, end_) msg = Info.pos2sfull_current start end_ ^ msg
let die_with_rawpos raw_pos msg = failwith      (msg_with_rawpos raw_pos msg)
let warn         raw_pos msg = prerr_endline (msg_with_rawpos raw_pos msg)

let die_rule msg = die_with_rawpos (Parsing.symbol_start(), Parsing.symbol_end()) msg
let warn_rule msg = warn (Parsing.symbol_start(), Parsing.symbol_end()) msg
let debug msg = if true then prerr_endline msg

let warn_verb pos msg = if not !Flags.quiet then warn (pos, pos) msg
let warn_too_many_space start = warn_verb start "you should have only one space here"
let warn_no_space	start = warn_verb start "you should have a space here"
let warn_cr		start = warn_verb start "you should not have a carriage-return (\\n) here"
let warn_space		start = warn_verb start "you should not have a space here"

let rec prio_less = function
  | P_paren_wanted prio1, prio2
  | prio1, P_paren_wanted prio2 -> prio_less(prio1, prio2)

  | P_ternary, P_or -> false
  | P_ternary, P_and -> false

  | _, P_loose -> true
  | P_loose, _ -> false
  | _, P_or -> true
  | P_or, _ -> false

  | _, P_and -> true
  | P_and, _ -> false
  | _, P_comma -> true
  | P_comma, _ -> false
  | _, P_call_no_paren -> true
  | P_call_no_paren, _ -> false
  | _, P_assign -> true
  | P_assign, _ -> false
  | _, P_ternary -> true
  | P_ternary, _ -> false

  | _, P_tight_or -> true
  | P_tight_or, _ -> false
  | _, P_tight_and -> true
  | P_tight_and, _ -> false

  | _, P_expr -> true
  | P_expr, _ -> false

  | _, P_eq -> true
  | P_eq, _ -> false
  | _, P_cmp -> true
  | P_cmp, _ -> false
  | _, P_add -> true
  | P_add, _ -> false
  | _, P_mul -> true
  | P_mul, _ -> false
  | _, P_tight -> true
  | P_tight, _ -> false

  | _, P_paren _ -> true
  | P_paren _, _ -> true
  | P_tok, _ -> true

let prio_lo pri_out ((pri_in, e), (_, pos)) =
  if prio_less(pri_in, pri_out) then
    (match pri_in with
    | P_paren (P_paren_wanted _) -> ()
    | P_paren pri_in' ->
	if pri_in' <> pri_out && 
	   prio_less(pri_in', pri_out) && not_complex (un_parenthesize e) then 
	  warn pos "unneeded parentheses"
    | _ -> ())
  else warn pos "missing parentheses (needed for clarity)" ;
  e
    
let prio_lo_after pri_out ((pri_in, e), _ as para) =
  if pri_in = P_call_no_paren then e else prio_lo pri_out para

let prio_lo_concat ((pri_in, e), both) = prio_lo P_mul ((P_paren_wanted pri_in, e), both)

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
  | Space_n -> warn_verb start "you should have a carriage-return (\\n) here"
  | Space_cr -> ()

let sp_same (_, (spaces1, _) as ter1) (_, (spaces2, _) as ter2) =
  if spaces1 <> Space_0 then sp_p ter2
  else if spaces2 <> Space_0 then sp_p ter1

let check_word_alone (word, _) =
  (match word with
  | Ident(None, ("time" as f), _)
  | Ident(None, ("wantarray" as f), _) ->
      die_rule (sprintf "please use %s() instead of %s" f f)
  | _ -> ());
  word

let check_parenthesized_first_argexpr word ((_, e), (_, (start, _)) as ex) =
  let want_space = word.[0] = '-' in
  if word = "return" then () else
  match e with
  | [ Call_op(_, (e' :: l)) ]
  | e' :: l ->
      if is_parenthesized e' then
	if want_space then
	  if l = [] then sp_n(ex) else die_with_rawpos (start, start) "can't handle this nicely"
	else
	  if l = [] then sp_0(ex) else die_with_rawpos (start, start) "you must not have a space here"
  | _ -> 
      if word = "time" then die_rule "please use time() instead of time";
      sp_p(ex)

let check_foreach (s, (_, pos)) = if s = "for"     then warn pos "write \"foreach\" instead of \"for\""
let check_for     (s, (_, pos)) = if s = "foreach" then warn pos "write \"for\" instead of \"foreach\""
let check_MULT_is_x (s, _) = if s <> "x" then die_rule "syntax error"
let check_my (s, _) = if s <> "my" then die_rule "syntax error"

let check_my_our op para (_, pos) =
  match op, para with
  | "=", [List [My_our _]; Ident(None, "undef", _)] -> warn pos "no need to initialize variable, it's done by default"
  | "=", [List [My_our _]; List[]] -> 
      if Info.is_on_same_line_current pos then warn pos "no need to initialize variables, it's done by default"
  | _ -> ()

let check_block_sub (l, (_, (_, end_)) as ter_lines) (_, (space, _) as ter_BRACKET_END) =  
  if l = [] then
    sp_0_or_cr ter_BRACKET_END
  else (
    (if l <> [] && List.hd l = Semi_colon then sp_0 else sp_p) ter_lines ;
    sp_p ter_BRACKET_END ;

    if space <> Space_cr then
      (if l <> [] && last l = Semi_colon then warn_verb end_ "spurious \";\" before closing block")
  )

let check_block_ref (l, (_, (_, end_)) as ter_lines) (_, (space, _) as ter_BRACKET_END) =
  if l <> [] && List.hd l = Semi_colon 
  then (sp_0 ter_lines ; sp_p ter_BRACKET_END)
  else sp_same ter_lines ter_BRACKET_END ;

  if space <> Space_cr then
    (if l <> [] && last l = Semi_colon then warn_verb end_ "spurious \";\" before closing block")

let check_my_our_paren (((comma_closed, _), _), _) =
  if not comma_closed then die_rule "syntax error"

let rec only_one (l, (spaces, pos)) =
  match l with
  | [List l'] -> only_one (l', (spaces, pos))
  | [e] -> e
  | [] -> die_with_rawpos pos "you must give one argument"
  | _  -> die_with_rawpos pos "you must give only one argument"

let only_one_in_List ((_, e), both) =
  match e with
  | List l -> only_one(l, both)
  | _ -> e
  
let to_List = function
  | [e] -> e
  | l -> List l

let deref_arraylen e = Call(Ident(None, "int", raw_pos2pos bpos), [Deref(I_array, e)])
let to_Ident ((fq, name), (_, pos)) = Ident(fq, name, raw_pos2pos pos)
let to_Raw_string (s, (_, pos)) = Raw_string(s, raw_pos2pos pos)
let to_Local ((_, e), (_, pos)) =
  let l = 
    match e with
    | List[List l] -> l
    | _ -> [e]
  in
  let local_vars, local_exprs = fpartition (function
    | Deref(I_star, Ident(None, ident, _)) ->
	Some(I_star, ident)
    | Deref(I_scalar, Ident _)
    | Deref(I_array, Ident _)
    | Deref(I_star, Ident _)
    | Deref_with(I_hash, Ident _, _)
    | Deref_with(I_hash, Deref(I_scalar, _), _)
    | Deref_with(I_hash, Deref_with(I_hash, Ident _, _), _)
    | Deref_with(I_hash, Deref_with(I_hash, Deref(I_scalar, Ident _), _), _) ->
	None
    | _ -> die_with_rawpos pos "bad argument to \"local\""
  ) l in
  if local_vars = [] then Call_op("local", local_exprs)
  else if local_exprs = [] then My_our("local", local_vars, raw_pos2pos pos)
  else die_with_rawpos pos "bad argument to \"local\""

let op prio s (_, both) = prio, (((), both), s)
let op_p prio s e = sp_p e ; op prio s e

let call_op((prio, (prev_ter, op)), ter, para) = 
  sp_same prev_ter ter ;
  check_my_our op para (snd ter);
  prio, Call_op(op, para)

let sub_declaration (name, proto) body = Sub_declaration(name, proto, Block body)
let anonymous_sub body = Anonymous_sub (Block body)

let call(e, para) = 
  (match e with
  | Ident(None, "require", _) ->
      (match para with
      | [ Ident _ ] -> ()
      | [ String _ ] -> ()
      | [ Raw_string _ ] -> ()
      | _ -> die_rule "use either \"require PACKAGE\" or \"require 'PACKAGE.pm'\"")
  | Ident(None, "N", _) ->
      (match para with
      | [List(String _ :: _)] -> ()
      | _ -> die_rule "N() must be used with a string")
  | _ -> ());
  Call(e, para)

let call_one_scalar_para (e, (_, pos)) para =
  let pri =
    match e with
    | "defined" -> P_expr
    | _ -> P_add
  in
  pri, Call(Ident(None, e, raw_pos2pos pos), para)

let (current_lexbuf : Lexing.lexbuf option ref) = ref None


let rec list2tokens l =
  let rl = ref l in
  fun lexbuf ->
    match !rl with
    | [] -> internal_error "list2tokens"
    | ((start, end_), e) :: l -> 
	lexbuf.Lexing.lex_abs_pos <- 0 ;
	lexbuf.Lexing.lex_start_pos <- start ;
	lexbuf.Lexing.lex_curr_pos <- end_ ;
	rl := l ; e

let parse_tokens parse tokens lexbuf_opt =
  if lexbuf_opt <> None then current_lexbuf := lexbuf_opt ;
  if tokens = [] then [] else
  parse (list2tokens tokens) (some !current_lexbuf)

let parse_interpolated parse l = List.map (fun (s, tokens) -> s, to_List(parse_tokens parse tokens None)) l

let to_String parse (l, (_, pos)) = String(parse_interpolated parse l, raw_pos2pos pos)

let from_PATTERN parse ((s, opts), (_, pos)) = 
  [ String(parse_interpolated parse s, raw_pos2pos pos) ; 
    Raw_string(opts, raw_pos2pos pos) ]
let from_PATTERN_SUBST parse ((s1, s2, opts), (_, pos)) = 
  [ String(parse_interpolated parse s1, raw_pos2pos pos) ; 
    String(parse_interpolated parse s2, raw_pos2pos pos) ; 
    Raw_string(opts, raw_pos2pos pos) ]
