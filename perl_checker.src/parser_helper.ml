open Types
open Common
open Printf

let bpos = -1, -1

let raw_pos2pos(a, b) = !Info.current_file, a, b
let pos_range (_, (_, (a1, b1))) (_, (_, (a2, b2))) = raw_pos2pos((if a1 = -1 then a2 else a1), (if b2 = -1 then b1 else b2))
let sp_pos_range (_, (space, (a1, b1))) (_, (_, (a2, b2))) = space, ((if a1 = -1 then a2 else a1), (if b2 = -1 then b1 else b2))
let get_pos (_, (_, pos)) = raw_pos2pos pos
let var_dollar_ pos = Deref(I_scalar, Ident(None, "_", pos))
let var_STDOUT = Deref(I_star, Ident(None, "STDOUT", raw_pos2pos bpos))

let is_var_dollar_ = function
  | Deref(I_scalar, Ident(None, "_", _)) -> true
  | _ -> false
let is_var_number_match = function
  | Deref(I_scalar, Ident(None, s, _)) -> String.length s = 1 && s.[0] <> '0' && char_is_number s.[0]
  | _ -> false

let is_parenthesized = function
  | List[]
  | List[List _] -> true
  | _ -> false

let un_parenthesize = function
  | List[List[e]] -> e
  | List[e] -> e
  | _ -> internal_error "un_parenthesize"

let rec un_parenthesize_full = function
  | List[e] -> un_parenthesize_full e
  | e -> e

let not_complex e =
  if is_parenthesized e then true else
  let rec not_complex_ op = function
    | Call_op("?:", _, _) -> false
    | Call_op(op', l, _) -> op <> op' && List.for_all (not_complex_ op') l
    | e -> not (is_parenthesized e)
  in not_complex_ "" (un_parenthesize_full e)

let not_simple = function
  | Num _ | Ident _ | Deref(_, Ident _) -> false
  | _ -> true

let string_of_Ident = function
  | Ident(None, s, _) -> s
  | Ident(Some fq, s, _) -> fq ^ "::" ^ s
  | _ -> internal_error "string_of_Ident"
let context2s = function
  | I_scalar -> "$"
  | I_hash -> "%"
  | I_array -> "@"
  | I_func -> "&"
  | I_raw -> ""
  | I_star -> "*"
let variable2s(context, ident) = context2s context ^ ident

let non_scalar_context context = context = I_hash || context = I_array

let rec is_same_fromparser a b =
  match a, b with
  | Undef, Undef -> true
  | Ident(fq1, s1, _), Ident(fq2, s2, _) -> fq1 = fq2 && s1 = s2
  | Num(s1, _), Num(s2, _) 
  | Raw_string(s1, _), Raw_string(s2, _) -> s1 = s2

  | String(l1, _), String(l2, _) ->
      List.for_all2 (fun (s1, e1) (s2, e2) -> s1 = s2 && is_same_fromparser e1 e2) l1 l2

  | Ref(c1, e1), Ref(c2, e2)
  | Deref(c1, e1), Deref(c2, e2) -> c1 = c2 && is_same_fromparser e1 e2

  | Deref_with(c1, c_1, e1, e_1), Deref_with(c2, c_2, e2, e_2) -> c1 = c2 && c_1 = c_2 && is_same_fromparser e1 e2 && is_same_fromparser e_1 e_2

  | Diamond(None), Diamond(None) -> true
  | Diamond(Some e1), Diamond(Some e2) -> is_same_fromparser e1 e2

  | List(l1), List(l2) -> List.for_all2 is_same_fromparser l1 l2

  | Call_op(op1, l1, _), Call_op(op2, l2, _) -> op1 = op2 && List.for_all2 is_same_fromparser l1 l2
  | Call(e1, l1), Call(e2, l2) -> is_same_fromparser e1 e2 && List.for_all2 is_same_fromparser l1 l2

  | Method_call(e1, m1, l1), Method_call(e2, m2, l2) ->
      is_same_fromparser e1 e2 && is_same_fromparser m1 m2 && List.for_all2 is_same_fromparser l1 l2

  | _ -> false

let from_scalar (e, _) =
  match e with
  | Deref(I_scalar, ident) -> ident
  | _ -> internal_error "from_scalar"

let from_array (e, _) =
  match e with
  | Deref(I_array, ident) -> ident
  | _ -> internal_error "from_array"

let msg_with_rawpos (start, end_) msg = Info.pos2sfull_current start end_ ^ msg
let die_with_rawpos raw_pos msg = failwith (msg_with_rawpos raw_pos msg)
let warn raw_pos msg = print_endline_flush (msg_with_rawpos raw_pos msg)

let die_rule msg = die_with_rawpos (Parsing.symbol_start(), Parsing.symbol_end()) msg
let warn_rule msg = warn (Parsing.symbol_start(), Parsing.symbol_end()) msg
let debug msg = if true then print_endline_flush msg

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
  match word with
  | Ident(None, f, pos) ->
      (match f with
      | "length" | "stat" | "lstat" | "chop" | "chomp" | "quotemeta" | "lc" | "lcfirst" | "uc" | "ucfirst" ->
	  Call(Deref(I_func, word), [var_dollar_ pos])

      | "split" -> Call(Deref(I_func, word), [ Raw_string(" ", pos) ; var_dollar_ pos ])
      | "shift" -> Call(Deref(I_func, word), [ Deref(I_array,  Ident(None, "_", raw_pos2pos bpos)) ])
      | "die"   -> Call(Deref(I_func, word), [ Deref(I_scalar, Ident(None, "@", raw_pos2pos bpos)) ])
      | "return" | "eof" | "caller" 
      | "redo" | "next" | "last" -> 
	  Deref(I_func, word)

      | "hex" | "ref" -> 
	  warn_rule (sprintf "please use \"%s $_\" instead of \"%s\"" f f) ;
	  Call(Deref(I_func, word), [ Raw_string(" ", pos) ; var_dollar_ pos ])
      | "time" | "wantarray" | "fork" | "getppid" | "arch" -> 
	  warn_rule (sprintf "please use %s() instead of %s" f f) ;
	  Deref(I_func, word)
      | _ -> word)
  | _ -> word

let check_parenthesized_first_argexpr word ((_, e), (_, (start, _)) as ex) =
  let want_space = word.[0] = '-' in
  if word = "return" then () else
  match e with
  | [ Call_op(_, (e' :: l), _) ]
  | e' :: l ->
      if is_parenthesized e' then
	if l = [] then 
	  (if want_space then sp_n else sp_0) ex
	else die_with_rawpos (start, start) "can't handle this nicely"
      else
	sp_p(ex)
  | _ -> 
      if word = "time" then die_rule "please use time() instead of time";
      sp_p(ex)

let check_parenthesized_first_argexpr_with_Ident ident ((prio, e), _ as ex) =
  if prio = P_tok then ();
  (match ident with
  | Ident(Some _, _, _) ->
      (match e with
      | [e] when is_parenthesized e -> ()
      | _ -> warn_rule "use parentheses around argument (otherwise it might cause syntax errors if the package is \"require\"d and not \"use\"d")
  | Ident(None, word, _) when List.mem word ["ref"] ->
      if prio <> P_tok then warn_rule "use parentheses around argument"
  | _ -> ());
  check_parenthesized_first_argexpr (string_of_Ident ident) ex

let check_hash_subscript ((_, e), (_, pos)) =
  let can_be_raw_string = function
    | "" | "x" | "y" -> false (* special case for {'y'} otherwise the emacs mode goes wild, special case for {'x'} to have the same as {'y'} (since they usually go together) *)
    | s -> 
	char_is_alpha s.[0] && (String.length s = 1 || string_forall_with char_is_alphanumerical_ 1 s)
  in
  match e with
  | List [String ([(s, List [])], _)] when can_be_raw_string s -> warn pos (sprintf "{\"%s\"} can be written {%s}" s s)
  | List [Raw_string(s, _)] when can_be_raw_string s -> warn pos (sprintf "{'%s'} can be written {%s}" s s)
  | _ -> ()

let check_arrow_needed ((_, e), _) ter =
  match e with
  | Deref_with(I_array, I_scalar, List [List [Call _]], _) -> () (* "->" needed for (f())[0]->{XX} *)
  | Deref_with _ -> warn (sndsnd ter) "the arrow \"->\" is unneeded"
  | _ -> ()

let check_ternary_paras(cond, a, b) =
  let rec dont_need_short_circuit_rec = function
    | Num _
    | Raw_string _
    | String ([(_, List [])], _) 
    | Call_op("qw", _, _)
      -> true
    | Call(Deref(I_func, Ident(None, "N", _)), [ List(String _ :: l) ])
    | Call_op(".", l, _)
    | Ref(I_hash, List l)
    | List l -> List.for_all dont_need_short_circuit_rec l
    | _ -> false
  in
  let rec dont_need_short_circuit = function
    | Ref(_, Deref(_, Ident _))
    | Deref(_, Ident _) -> true
    | Ref(I_hash, List l)
    | List l -> List.for_all dont_need_short_circuit l
    | e -> dont_need_short_circuit_rec e
  in
  let check_ternary_para = function
    | List [] -> warn_rule "you may use if_() here\n  beware that the short-circuit semantic of ?: is not kept\n  if you want to keep the short-circuit behaviour, replace () with @{[]} and there will be no warning anymore"
    | _ -> ()
  in
  if dont_need_short_circuit a || is_same_fromparser cond a then check_ternary_para b;
  if dont_need_short_circuit b || is_same_fromparser cond b then check_ternary_para a;
  if is_same_fromparser cond a && dont_need_short_circuit b then warn_rule "you can replace \"$foo ? $foo : $bar\" with \"$foo || $bar\"";
  [ cond; a; b ]

let check_unneeded_var_dollar_    ((_, e), (_, pos)) =
  if is_var_dollar_ e then warn pos "\"$_ =~ /regexp/\" can be written \"/regexp/\"" else
  if is_var_number_match e then warn pos "do not use the result of a match (eg: $1) to match another pattern"
let check_unneeded_var_dollar_not ((_, e), (_, pos)) =
  if is_var_dollar_ e then warn pos "\"$_ !~ /regexp/\" can be written \"!/regexp/\"" else
  if is_var_number_match e then warn pos "do not use the result of a match (eg: $1) to match another pattern"
let check_unneeded_var_dollar_s   ((_, e), (_, pos)) = 
  if is_var_dollar_ e then warn pos "\"$_ =~ s/regexp/.../\" can be written \"s/regexp/.../\"" else
  if is_var_number_match e then die_with_rawpos pos "do not modify the result of a match (eg: $1)"

let check_MULT_is_x (s, _) = if s <> "x" then die_rule "syntax error"
let check_my (s, _) = if s <> "my" then die_rule "syntax error"
let check_foreach (s, (_, pos)) = if s = "for"     then warn pos "write \"foreach\" instead of \"for\""
let check_for     (s, (_, pos)) = if s = "foreach" then warn pos "write \"for\" instead of \"foreach\""
let check_for_foreach (s, (_, pos)) ((_, expr), _) =
  match expr with
  | List [ Deref(I_scalar, _) ] ->
      if s = "foreach" then warn pos "you are using the special fpons trick to locally set $_ with a value, for this please use \"for\" instead of \"foreach\""
  | List [ Deref_with(context, I_scalar, _, _) ] when context <> I_func -> 
      if s = "foreach" then warn pos "you are using the special fpons trick to locally set $_ with a value, for this please use \"for\" instead of \"foreach\""
  | _ -> 
      if s = "for" then warn pos "write \"foreach\" instead of \"for\""

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

let only_one_array_ref (l, (spaces, pos)) =
  let e = only_one (l, (spaces, pos)) in
  (match e with
  | Call_op("last_array_index", [Deref(I_array, e)], _) ->
      warn pos (sprintf "you can replace $#%s with -1" (string_of_Ident e))
  | _ -> ());
  e

let only_one_in_List ((_, e), both) =
  match e with
  | List l -> only_one(l, both)
  | _ -> e
  
let rec is_only_one_in_List = function
  | [List l] -> is_only_one_in_List l
  | [_] -> true
  | _ -> false
  
let is_not_a_scalar = function
  | Deref_with(_, context, _, _)
  | Deref(context, _) -> non_scalar_context context
  | _ -> false

let maybe_to_Raw_string = function
  | Ident(None, s, pos) -> Raw_string(s, pos)
  | Ident(Some fq, s, pos) -> Raw_string(fq ^ "::" ^ s, pos)
  | e -> e

let to_List = function
  | [e] -> e
  | l -> List l

let deref_arraylen e = Call_op("last_array_index", [Deref(I_array, e)], raw_pos2pos bpos)
let to_Ident ((fq, name), (_, pos)) = Ident(fq, name, raw_pos2pos pos)
let to_Raw_string (s, (_, pos)) = Raw_string(s, raw_pos2pos pos)
let to_Method_call (object_, method_, para) = 
  match method_ with
  | Ident(Some "SUPER", name, pos) -> Method_call(maybe_to_Raw_string object_, Raw_string(name, pos), para)
  | Ident(Some _, _, _) -> Call(Deref(I_func, method_), maybe_to_Raw_string object_ :: para)
  | _ -> Method_call(maybe_to_Raw_string object_, maybe_to_Raw_string method_, para)
let to_Deref_with(from_context, to_context, ref_, para) =
  if is_not_a_scalar ref_ then warn_rule "bad deref";
  Deref_with(from_context, to_context, ref_, para)

  
let to_Local ((_, e), (_, pos)) =
  let l = 
    match e with
    | List[List l] -> l
    | _ -> [e]
  in
  let local_vars, local_exprs = fpartition (function
    | Deref(I_star as context, Ident(None, ident, _))
    | Deref(I_scalar as context, Ident(None, ("_" as ident), _)) ->
	Some(context, ident)
    | Deref(I_scalar, Ident _)
    | Deref(I_array, Ident _)
    | Deref(I_star, Ident _)
    | Deref_with(I_hash, I_scalar, Ident _, _)
    | Deref_with(I_hash, I_scalar, Deref(I_scalar, _), _)
    | Deref_with(I_hash, I_scalar, Deref_with(I_hash, I_scalar, Ident _, _), _)
    | Deref_with(I_hash, I_scalar, Deref_with(I_hash, I_scalar, Deref(I_scalar, Ident _), _), _) ->
	None
    | _ -> die_with_rawpos pos "bad argument to \"local\""
  ) l in
  if local_vars = [] then Call_op("local", local_exprs, raw_pos2pos pos)
  else if local_exprs = [] then My_our("local", local_vars, raw_pos2pos pos)
  else die_with_rawpos pos "bad argument to \"local\""

let op prio s (_, both) = prio, (((), both), s)
let op_p prio s e = sp_p e ; op prio s e

let sub_declaration (name, proto) body = Sub_declaration(name, proto, Block body)
let anonymous_sub (body, (_, pos)) = Anonymous_sub (Block body, raw_pos2pos pos)

let cook_call_op(op, para, pos) =
  let call = Call_op(op, para, raw_pos2pos pos) in
  match op, para with
  | "=", [My_our _; Ident(None, "undef", _)] -> 
      warn pos "no need to initialize variable, it's done by default" ;
      call
  | "=", [My_our _; List[]] -> 
      if Info.is_on_same_line_current pos then warn pos "no need to initialize variables, it's done by default" ;
      call

  | "=", [ Deref(I_star, String ([(sf1, List [])], _)); _ ] ->
      warn_rule (sprintf "write *{'%s'} instead of *{\"%s\"}" sf1 sf1) ;
      call

  | "=", [ Deref(I_star, (Ident _ as f1)); Deref(I_star, (Ident _ as f2)) ] ->
      let s1, s2 = string_of_Ident f1, string_of_Ident f2 in
      warn pos (sprintf "\"*%s = *%s\" is better written \"*%s = \\&%s\"" s1 s2 s1 s2) ;
      sub_declaration (f1, "") [ Deref(I_func, f2) ]
  | "=", [ Deref(I_star, Raw_string(sf1, pos_f1)); Deref(I_star, (Ident _ as f2)) ] ->
      let s2 = string_of_Ident f2 in
      warn pos (sprintf "\"*{'%s'} = *%s\" is better written \"*{'%s'} = \\&%s\"" sf1 s2 sf1 s2) ;
      sub_declaration (Ident(None, sf1, pos_f1), "") [ Deref(I_func, f2) ]

  | "=", [ Deref(I_star, (Ident _ as f1)); Ref(I_scalar, Deref(I_func, (Ident _ as f2))) ] ->
      sub_declaration (f1, "") [ Deref(I_func, f2) ]
  | "=", [ Deref(I_star, Raw_string(sf1, pos_f1)); Ref(I_scalar, Deref(I_func, (Ident _ as f2))) ] ->
      sub_declaration (Ident(None, sf1, pos_f1), "") [ Deref(I_func, f2) ]

  | _ -> 
      call

let call_op_((prio, (prev_ter, op)), ter, para) (sp, pos) = 
  sp_same prev_ter ter ;
  (prio, cook_call_op(op, para, pos)), (sp, pos)

let to_Call_op(op, para) (sp, pos) = Call_op(op, para, raw_pos2pos pos), (sp, pos)
let to_Call_op_(prio, op, para) (sp, pos) = (prio, Call_op(op, para, raw_pos2pos pos)), (sp, pos)

let followed_by_comma ((_,e), _) (true_comma, _) =
  if true_comma then e else
    match split_last e with
    | l, Ident(None, s, pos) -> l @ [Raw_string(s, pos)]
    | _ -> e


let pot_strings = ref []
let pot_strings_and_file = Hashtbl.create 16
let po_comments = ref []
let po_comment (s, _) = lpush po_comments s

let check_format_a_la_printf s pos =
  let rec check_format_a_la_printf_ i =
    try
      let i' = String.index_from s i '%' in
      try
	(match s.[i' + 1] with
	| '%' | 'd' | 's' | 'c' -> ()
	| c -> warn (pos + i', pos + i') (sprintf "invalid command %%%c" c));
	check_format_a_la_printf_ (i' + 2)
      with Invalid_argument _ -> warn (pos + i', pos + i') "invalid command %"
    with Not_found -> ()
  in check_format_a_la_printf_ 0
  
let generate_pot file = 
  let fd = open_out file in
  output_string fd 
"# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"POT-Creation-Date: 2002-12-05 19:52+0100\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: 8-bit\\n\"

" ;

  let rec print_formatted_char = function
    | '"'  -> output_char fd '\\'; output_char fd '"'
    | '\t' -> output_char fd '\\'; output_char fd 't'
    | '\\' -> output_char fd '\\'; output_char fd '\\'
    | '\n' -> output_string fd "\\n\"\n\""
    | c -> output_char fd c
  in
  List.iter (fun (s, po_comments) ->
    match Hashtbl.find_all pot_strings_and_file s with
    | [] -> ()
    | l ->
	List.iter (fun po_comment -> output_string fd ("#. " ^ po_comment ^ "\n")) po_comments;

	List.iter (fun _ -> Hashtbl.remove pot_strings_and_file s) l ;
	fprintf fd "#: %s\n" (String.concat " " (List.map (fun s -> s ^ ":1") l)) ;
	output_string fd "#, c-format\n" ;

	output_string fd (if String.contains s '\n' then "msgid \"\"\n\"" else "msgid \"") ;
	String.iter print_formatted_char s ;
	output_string fd "\"\n" ;
	output_string fd "msgstr \"\"\n\n"
  ) !pot_strings ;      
  close_out fd

let call_func is_a_func (e, para) =
  match e with
  | Deref(I_func, Ident(None, f, _)) ->
      let para' = match f with
      | "no" ->
	  (match para with
	  | [ Ident(_, _, pos) as s ] -> Some [ Raw_string(string_of_Ident s, pos) ]
	  | [ Call(Deref(I_func, (Ident(_, _, pos) as s)), l) ] -> Some(Raw_string(string_of_Ident s, pos) :: l)
	  | _ -> die_rule "use \"no PACKAGE <para>\"")
      | "undef" ->
	  (match para with
	  | [ Deref(I_star, ident) ] -> Some [ Deref(I_func, ident) ]
	  | _ -> None)

      | "N" | "N_" ->
	  (match para with
	  | [ List(String([ s, List [] ], (file, pos_a, _)) :: _) ] -> 
	      if !Flags.generate_pot then (
		lpush pot_strings (s, !po_comments) ;
		po_comments := [] ;
		Hashtbl.add pot_strings_and_file s file ;
	      ) ;
	      check_format_a_la_printf s pos_a ;
	      (*if String.contains s '\t' then warn_rule "tabulation in translated string must be written \\\\t";*)
	      (*if count_matching_char s '\n' > 10 then warn_rule "long string";*)
	      None
	  | [ List(String _ :: _) ] -> die_rule "don't use interpolated translated string, use %s or %d instead"
	  |  _ -> die_rule (sprintf "%s() must be used with a string" f))

      | "goto" ->
	  (match para with
	  | [ Ident(None, s, pos) ] -> Some [ Raw_string(s, pos) ]
	  | _ -> None)

      | "last" | "next" | "redo" when not is_a_func ->
	  (match para with
	  | [ Ident(None, s, pos) ] -> Some [ Raw_string(s, pos) ]
	  | _ -> die_rule (sprintf "%s must be used with a raw string" f))

      | "length" ->
	  if para = [] then warn_rule "length() with no parameter !?" else
	  if is_not_a_scalar (List.hd para) then warn_rule "never use \"length @l\", it returns the length of the string int(@l)" ;
	  None

      | "split" ->
	  (match para with
	  | [ List(Call_op("m//", Deref(I_scalar, Ident(None, "_", _)) :: pattern, pos) :: l) ]
	  | Call_op("m//", Deref(I_scalar, Ident(None, "_", _)) :: pattern, pos) :: l ->
	      Some(Call_op("qr//", pattern, pos) :: l)
	  | _ -> None)
	    
      | _ -> None
      in Call(e, some_or para' para)
  | _ -> Call(e, para)

let call(e, para) = call_func false (e, para)


let call_one_scalar_para (e, (_, pos)) para =
  let para =
    match para with
    | [] ->
	  warn_rule (sprintf "please use \"%s $_\" instead of \"%s\"" e e) ;
	  [var_dollar_ (raw_pos2pos pos)]
    | _ -> para
  in
  let pri =
    match e with
    | "defined" -> P_expr
    | _ -> P_add
  in
  pri, call(Deref(I_func, Ident(None, e, raw_pos2pos pos)), para)


let call_op_if_infix left right (sp, pos) =
  (match left, right with
  | List [Call_op("=", [Deref(context, _); _], _)], _ when non_scalar_context context -> ()
  | List [Call_op("=", [v; _], _)],
    List [Call_op("not", [v'], _)] when is_same_fromparser v v' ->
      warn_rule "\"$foo = ... if !$foo\" can be written \"$foo ||= ...\""
  | _ -> ());
  Call_op("if infix", [ left ; right], raw_pos2pos pos), (sp, pos)

let call_op_unless_infix left right (sp, pos) =
  (match left, right with
  | List [Call_op("=", [Deref(context, _); _], _)], _ when non_scalar_context context -> ()
  | List [Call_op("=", [v; _], _)], List [v'] when is_same_fromparser v v' ->
      warn_rule "\"$foo = ... unless $foo\" can be written \"$foo ||= ...\""
  | _ -> ());
  (match right with
  | List [Call_op(op, _, _)] ->
      (match op with
      | "&&" | "||" | "not" | "ne" | "?:" -> warn_rule "don't use \"unless\" when the condition is complex, use \"if\" instead"
      | _ -> ());
  | _ -> ());
  Call_op("unless infix", [ left ; right], raw_pos2pos pos), (sp, pos)


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

let parse_interpolated parse l = 
  let l' = List.map (fun (s, tokens) -> s, to_List(parse_tokens parse tokens None)) l in
  match split_last l' with
  | pl, ("", List []) -> pl
  | _ -> l'

let to_String parse strict (l, (_, pos)) = 
  let l' = parse_interpolated parse l in
  (match l' with
  | [ "", List [Deref(I_scalar, Ident(None, ident, _))]] -> 
      if strict then warn pos (sprintf "%s is better written without the double quotes" (variable2s(I_scalar, ident)))
  | [ "", List [Deref(I_hash, _)]] -> 
      warn pos "don't use a hash in string context"
  | [ "", List [Deref(I_array, _)]] -> 
      ()
  | [("", _)] -> 
      if strict then warn pos "double quotes are unneeded"
  | _ -> ());
  String(l', raw_pos2pos pos)

let from_PATTERN parse ((s, opts), (_, pos)) = 
  [ String(parse_interpolated parse s, raw_pos2pos pos) ; 
    Raw_string(opts, raw_pos2pos pos) ]
let from_PATTERN_SUBST parse ((s1, s2, opts), (_, pos)) = 
  [ String(parse_interpolated parse s1, raw_pos2pos pos) ; 
    String(parse_interpolated parse s2, raw_pos2pos pos) ; 
    Raw_string(opts, raw_pos2pos pos) ]
