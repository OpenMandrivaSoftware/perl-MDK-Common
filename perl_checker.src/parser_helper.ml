open Types
open Common
open Printf

let bpos = -1, -1

let raw_pos2pos(a, b) = !Info.current_file, a, b
let raw_pos_range { pos = (a1, b1) } { pos = (a2, b2) } = (if a1 = -1 then a2 else a1), (if b2 = -1 then b1 else b2)
let pos_range esp1 esp2 = raw_pos2pos (raw_pos_range esp1 esp2)
let get_pos pesp = raw_pos2pos pesp.pos
let get_pos_start { pos = (start, _) } = start
let get_pos_end { pos = (_, end_) } = end_
let var_dollar_ pos = Deref(I_scalar, Ident(None, "_", pos))
let var_STDOUT = Deref(I_star, Ident(None, "STDOUT", raw_pos2pos bpos))

let new_any mcontext any spaces pos = { mcontext = mcontext ; any = any ; spaces = spaces ; pos = pos }
let new_any_ any spaces pos = new_any M_unknown any spaces pos
let new_esp mcontext e esp_start esp_end = new_any mcontext e esp_start.spaces (raw_pos_range esp_start esp_end)
let new_1esp e esp = new_any esp.mcontext e esp.spaces esp.pos
let new_pesp mcontext prio e esp_start esp_end = new_any mcontext { priority = prio ; expr = e } esp_start.spaces (raw_pos_range esp_start esp_end)
let new_1pesp prio e esp = new_any esp.mcontext { priority = prio ; expr = e } esp.spaces esp.pos
let default_esp e = new_any M_unknown e Space_none bpos
let default_pesp prio e = new_any M_unknown { priority = prio ; expr = e } Space_none bpos

let split_name_or_fq_name full_ident =
  match split_at2 ':'':' full_ident with
  | [] -> internal_error "split_ident"
  | [ident] -> None, ident
  | l ->
      let fql, name = split_last l in
      let fq = String.concat "::" fql in
      Some fq, name

let is_var_dollar_ = function
  | Deref(I_scalar, Ident(None, "_", _)) -> true
  | _ -> false
let is_var_number_match = function
  | Deref(I_scalar, Ident(None, s, _)) -> String.length s = 1 && s.[0] <> '0' && char_is_number s.[0]
  | _ -> false

let non_scalar_context context = context = I_hash || context = I_array
let is_scalar_context context = context = I_scalar
  
let rec is_not_a_scalar = function
  | Deref_with(_, context, _, _)
  | Deref(context, _) -> non_scalar_context context
  | List []
  | List(_ :: _ :: _) -> true
  | Call(Deref(I_func, Ident(None, "map", _)), _)
  | Call(Deref(I_func, Ident(None, "grep", _)), _) -> true
  | Call_op("?:", [ _cond ; a; b ], _) -> is_not_a_scalar a || is_not_a_scalar b
  | _ -> false

let is_a_scalar = function
  | Ref _
  | Num _
  | Raw_string _
  | String _
  | Call(Deref(I_func, Ident(None, "N", _)), _) -> true
  | My_our(_, [ context, _ ], _)
  | Deref_with(_, context, _, _)
  | Deref(context, _) -> is_scalar_context context
  | _ -> false

let is_a_string = function
  | String _ | Raw_string _ -> true
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

let rec un_parenthesize_full_l = function
  | [ List l ] -> un_parenthesize_full_l l
  | l -> l

let is_always_true = function
  | Num(n, _) -> float_of_string n <> 0.
  | Raw_string(s, _) -> s <> ""
  | String(l, _) -> l <> []
  | Ref _ -> true
  | _ -> false

let is_always_false = function
  | Num(n, _) -> float_of_string n = 0.
  | Raw_string(s, _) -> s = ""
  | String(l, _) -> l = []
  | List [] -> true
  | Ident(None, "undef", _) -> true
  | _ -> false

let rec is_lvalue = function
  | Call(Deref(I_func, Ident(None, f, _)), _) -> List.mem f [ "substr" ]

  | Call_op("?:", [ _ ; a ; b ], _) -> is_lvalue a && is_lvalue b

  | Call_op("local", l, _)
  | List [ List l ] 
    -> List.for_all is_lvalue l

  | My_our _
  | Deref(_, _)
  | Deref_with(_, _, _, _)
  | Ident(None, "undef", _)
    -> true

  | _ -> false

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

let context2s = function
  | I_scalar -> "$"
  | I_hash -> "%"
  | I_array -> "@"
  | I_func -> "&"
  | I_raw -> ""
  | I_star -> "*"
let variable2s(context, ident) = context2s context ^ ident

let rec string_of_fromparser = function
  | Semi_colon -> ";"
  | Undef -> "undef"
  | Num(num, _) -> num

  | Raw_string(s, _) -> "\"" ^ s ^ "\""
  | String(l, _) -> 
      let l' = List.map (fun (s, e) -> 
	s ^ if e = List[] then "" else string_of_fromparser e
      ) l in
      "\"" ^ String.concat "" l' ^ "\""

  | Ident(None, s, _) -> s
  | Ident(Some fq, s, _) -> fq ^ "::" ^ s
  | My_our(myour, l, _) -> myour ^ "(" ^ String.concat "," (List.map (fun (context, s) -> context2s context ^ s) l) ^ ")"

  | Anonymous_sub(_, e, _) -> "sub { " ^ string_of_fromparser e ^ " }"
  | Ref(_, e) -> "\\" ^ string_of_fromparser e
  | Deref(context, e) -> context2s context ^ string_of_fromparser e

  | Diamond(None) -> "<>"
  | Diamond(Some e) -> "<" ^ string_of_fromparser e ^ ">"

  | Sub_declaration(name, _prototype, body, Real_sub_declaration) ->
      "sub " ^ string_of_fromparser name ^ " { " ^ string_of_fromparser body ^ " }"

  | Sub_declaration(name, _prototype, body, Glob_assign) ->
      "*" ^ string_of_fromparser name ^ " = sub { " ^ string_of_fromparser body ^ " };"

  | Deref_with(_, _, _e1, _e2) ->
      internal_error "todo"

  | Package(p) -> "package " ^ string_of_fromparser p

  | Use(e, []) -> "use " ^ string_of_fromparser e
  | Use(e, l) -> "use " ^ string_of_fromparser e ^ "(" ^ lstring_of_fromparser l

  | List l -> lstring_of_fromparser_parentheses l
  | Block l -> "{ " ^ lstring_of_fromparser l ^ " }"
  | Call_op(op, l, _) -> op ^ lstring_of_fromparser_parentheses l

  | Call(e, l) -> string_of_fromparser e ^ lstring_of_fromparser l

  | Method_call(obj, meth, l) -> 
      let para = if l = [] then "" else lstring_of_fromparser_parentheses l in
      string_of_fromparser obj ^ "->" ^ string_of_fromparser meth ^ para

  | Label(e) -> e ^ ": "

  | Perl_checker_comment _ -> ""
  | Too_complex -> "XXX"

and lstring_of_fromparser l = String.concat ", " (List.map string_of_fromparser l)
and lstring_of_fromparser_parentheses l = "(" ^ lstring_of_fromparser l ^ ")"

let rec is_same_fromparser a b =
  match a, b with
  | Undef, Undef -> true
  | Ident(fq1, s1, _), Ident(fq2, s2, _) -> fq1 = fq2 && s1 = s2
  | Num(s1, _), Num(s2, _) 
  | Raw_string(s1, _), Raw_string(s2, _) -> s1 = s2

  | String(l1, _), String(l2, _) ->
      for_all2_ (fun (s1, e1) (s2, e2) -> s1 = s2 && is_same_fromparser e1 e2) l1 l2

  | Ref(c1, e1), Ref(c2, e2)
  | Deref(c1, e1), Deref(c2, e2) -> c1 = c2 && is_same_fromparser e1 e2

  | Deref_with(c1, c_1, e1, e_1), Deref_with(c2, c_2, e2, e_2) -> c1 = c2 && c_1 = c_2 && is_same_fromparser e1 e2 && is_same_fromparser e_1 e_2

  | Diamond(None), Diamond(None) -> true
  | Diamond(Some e1), Diamond(Some e2) -> is_same_fromparser e1 e2

  | List(l1), List(l2) -> for_all2_ is_same_fromparser l1 l2

  | Call_op(op1, l1, _), Call_op(op2, l2, _) -> op1 = op2 && for_all2_ is_same_fromparser l1 l2
  | Call(e1, l1), Call(e2, l2) -> is_same_fromparser e1 e2 && for_all2_ is_same_fromparser l1 l2

  | Method_call(e1, m1, l1), Method_call(e2, m2, l2) ->
      is_same_fromparser e1 e2 && is_same_fromparser m1 m2 && for_all2_ is_same_fromparser l1 l2

  | _ -> false

let from_scalar esp =
  match esp.any with
  | Deref(I_scalar, ident) -> ident
  | _ -> internal_error "from_scalar"

let from_array esp =
  match esp.any with
  | Deref(I_array, ident) -> ident
  | _ -> internal_error "from_array"

let rec get_pos_from_expr = function
  | Anonymous_sub(_, _, pos)
  | String(_, pos)
  | Call_op(_, _, pos)
  | Perl_checker_comment(_, pos)
  | My_our(_, _, pos)
  | Raw_string(_, pos)
  | Num(_, pos)
  | Ident(_, _, pos)
      -> pos

  | Package e
  | Ref(_, e)
  | Deref(_, e)
  | Sub_declaration(e, _, _, _)
  | Deref_with(_, _, e, _)
  | Use(e, _)
  | Call(e, _)
  | Method_call(_, e, _)
      -> get_pos_from_expr e

  | Diamond(option_e) 
      -> if option_e = None then raw_pos2pos bpos else get_pos_from_expr (some option_e)
      
  | List l
  | Block l
      -> if l = [] then raw_pos2pos bpos else get_pos_from_expr (List.hd l)

  | Semi_colon
  | Too_complex
  | Undef
  | Label _
      -> raw_pos2pos bpos

let msg_with_rawpos (start, end_) msg = Info.pos2sfull_current start end_ ^ msg
let die_with_rawpos raw_pos msg = failwith (msg_with_rawpos raw_pos msg)
let warn warn_types raw_pos msg = if Flags.are_warning_types_set warn_types then print_endline_flush (msg_with_rawpos raw_pos msg)

let die_rule msg = die_with_rawpos (Parsing.symbol_start(), Parsing.symbol_end()) msg
let warn_rule warn_types msg = warn warn_types (Parsing.symbol_start(), Parsing.symbol_end()) msg

let warn_verb warn_types pos msg = if not !Flags.quiet then warn warn_types (pos, pos) msg
let warn_too_many_space start = warn_verb [Warn_white_space] start "you should have only one space here"
let warn_no_space	start = warn_verb [Warn_white_space] start "you should have a space here"
let warn_cr		start = warn_verb [Warn_white_space] start "you should not have a carriage-return (\\n) here"
let warn_space		start = warn_verb [Warn_white_space] start "you should not have a space here"

let rec prio_less = function
  | P_none, _ | _, P_none -> internal_error "prio_less"

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
  | _, P_call_no_paren -> true
  | P_call_no_paren, _ -> false
  | _, P_comma -> true
  | P_comma, _ -> false
  | _, P_assign -> true
  | P_assign, _ -> false
  | _, P_ternary -> true
  | P_ternary, _ -> false

  | _, P_tight_or -> true
  | P_tight_or, _ -> false
  | _, P_tight_and -> true
  | P_tight_and, _ -> false

  | P_bit, P_bit -> true
  | P_bit, _ -> false

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

let prio_lo_check pri_out pri_in pos expr =
  if prio_less(pri_in, pri_out) then
    (match pri_in with
    | P_paren (P_paren_wanted _) -> ()
    | P_paren pri_in' ->
	if pri_in' <> pri_out && 
	   prio_less(pri_in', pri_out) && not_complex (un_parenthesize expr) then 
	  warn [Warn_suggest_simpler] pos "unneeded parentheses"
    | _ -> ())
  else 
    (match expr with
    | Call_op ("print", [Deref (I_star, Ident (None, "STDOUT", _)); (Deref(I_scalar, _) as ident)], _) -> 
	 warn [Warn_traps] pos (sprintf "use parentheses: replace \"print %s ...\" with \"print(%s ...)\"" (string_of_fromparser ident) (string_of_fromparser ident))
    | _ -> warn [Warn_traps] pos "missing parentheses (needed for clarity)")

let prio_lo pri_out in_ = prio_lo_check pri_out in_.any.priority in_.pos in_.any.expr ; in_.any.expr

let prio_lo_after pri_out in_ =
  if in_.any.priority = P_call_no_paren then in_.any.expr else prio_lo pri_out in_

let prio_lo_concat esp = prio_lo P_mul { esp with any = { esp.any with priority = P_paren_wanted esp.any.priority } }

let hash_ref esp = Ref(I_hash, prio_lo P_loose esp)

let sp_0 esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0 -> ()
  | Space_1
  | Space_n -> warn_space (get_pos_start esp)
  | Space_cr -> warn_cr (get_pos_start esp)

let sp_0_or_cr esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0 -> ()
  | Space_1
  | Space_n -> warn_space (get_pos_start esp)
  | Space_cr -> ()

let sp_1 esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space (get_pos_start esp)
  | Space_1 -> ()
  | Space_n -> warn_too_many_space (get_pos_start esp)
  | Space_cr -> warn_cr (get_pos_start esp)

let sp_n esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space (get_pos_start esp)
  | Space_1 -> ()
  | Space_n -> ()
  | Space_cr -> warn_cr (get_pos_start esp)

let sp_p esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0 -> warn_no_space (get_pos_start esp)
  | Space_1 -> ()
  | Space_n -> ()
  | Space_cr -> ()

let sp_cr esp =
  match esp.spaces with
  | Space_none -> ()
  | Space_0
  | Space_1
  | Space_n -> warn_verb [Warn_white_space] (get_pos_start esp) "you should have a carriage-return (\\n) here"
  | Space_cr -> ()

let sp_same esp1 esp2 =
  if esp1.spaces <> Space_0 then sp_p esp2
  else if esp2.spaces <> Space_0 then sp_p esp1

let function_to_context word_alone = function
  | "map" | "grep" | "grep_index" | "map_index" | "uniq" | "uniq_" -> M_array
  | "partition" -> M_tuple [ M_ref M_array ; M_ref M_array ]
  | "find" -> M_unknown_scalar
  | "any" | "every" -> M_bool
  | "find_index" -> M_int
  | "each_index" -> M_none
  | "N" | "N_" -> M_string

  | "chop" | "chomp" | "push" | "unshift" -> M_none	
  | "hex" | "length" | "time" | "fork" | "getppid" -> M_int
  | "eof" | "wantarray" -> M_int
  | "stat" | "lstat" -> M_list
  | "arch" | "quotemeta" | "join" | "lc" | "lcfirst" | "uc" | "ucfirst" -> M_string
	    
  | "split" -> M_array
  | "shift" | "pop" -> M_unknown_scalar
  | "die" | "return" | "redo" | "next" | "last" -> M_unknown
  | "caller" -> M_mixed [M_string ; M_list]
	
  | "ref" -> M_ref M_unknown_scalar
  | "undef" -> if word_alone then M_undef else M_none
  | _ -> M_unknown

let word_alone esp =
  let word = esp.any in
  let mcontext, e = match word with
  | Ident(None, f, pos) ->
      let e = match f with
      | "length" | "stat" | "lstat" | "chop" | "chomp" | "quotemeta" | "lc" | "lcfirst" | "uc" | "ucfirst" ->
	  Call(Deref(I_func, word), [var_dollar_ pos])
	    
      | "split" -> Call(Deref(I_func, word), [ Raw_string(" ", pos) ; var_dollar_ pos ])
      | "die"   -> Call(Deref(I_func, word), [ Deref(I_scalar, Ident(None, "@", raw_pos2pos bpos)) ])
      | "return" | "eof" | "caller" 
      | "redo" | "next" | "last" -> 
	  Deref(I_func, word)
	    
      | "hex" | "ref" -> 
	  warn_rule [Warn_complex_expressions] (sprintf "please use \"%s $_\" instead of \"%s\"" f f) ;
	  Call(Deref(I_func, word), [ Raw_string(" ", pos) ; var_dollar_ pos ])
      | "time" | "wantarray" | "fork" | "getppid" | "arch" -> 
	  warn_rule [Warn_complex_expressions] (sprintf "please use %s() instead of %s" f f) ;
	  Deref(I_func, word)
      | _ -> word
      in
      function_to_context true f, e
  | _ -> M_unknown, word
  in
  new_pesp mcontext P_tok e esp esp

let check_parenthesized_first_argexpr word esp =
  let want_space = word.[0] = '-' in
  if word = "return" then () else
  match esp.any.expr with
  | [ Call_op(_, (e' :: l), _) ]
  | e' :: l ->
      if is_parenthesized e' then
	if l = [] then 
	  (if want_space then sp_n else sp_0) esp
	else 
	  (* eg: join (" ", @l) . "\n" *)
	  die_with_rawpos (get_pos_start esp, get_pos_start esp) "please remove the space before the function call"
      else
	sp_p esp
  | _ -> 
      if word = "time" then die_rule "please use time() instead of time";
      sp_p esp

let check_parenthesized_first_argexpr_with_Ident ident esp =
  if esp.any.priority = P_tok then ();
  (match ident with
  | Ident(Some _, _, _) ->
      (match esp.any.expr with
      | [e] when is_parenthesized e -> ()
      | _ -> warn_rule [Warn_suggest_simpler] "use parentheses around argument (otherwise it might cause syntax errors if the package is \"require\"d and not \"use\"d")
  | Ident(None, word, _) when List.mem word ["ref" ; "readlink"] ->
      if esp.any.priority <> P_tok then warn_rule [Warn_complex_expressions] "use parentheses around argument"
  | _ -> ());
  check_parenthesized_first_argexpr (string_of_fromparser ident) esp

let check_hash_subscript esp =
  let can_be_raw_string = function
    | "" | "x" | "y" -> false (* special case for {'y'} otherwise the emacs mode goes wild, special case for {'x'} to have the same as {'y'} (since they usually go together) *)
    | s -> 
	char_is_alpha s.[0] && (String.length s = 1 || string_forall_with char_is_alphanumerical_ 1 s)
  in
  match esp.any.expr with
  | List [String ([(s, List [])], _)] when can_be_raw_string s -> warn [Warn_suggest_simpler] esp.pos (sprintf "{\"%s\"} can be written {%s}" s s)
  | List [Raw_string(s, _)] when can_be_raw_string s -> warn [Warn_suggest_simpler] esp.pos (sprintf "{'%s'} can be written {%s}" s s)
  | _ -> ()

let check_arrow_needed esp1 esp2 =
  match esp1.any.expr with
  | Deref_with(I_array, I_scalar, List [List [Call _]], _) -> () (* "->" needed for (f())[0]->{XX} *)
  | Deref_with _ -> warn [Warn_suggest_simpler] esp2.pos "the arrow \"->\" is unneeded"
  | _ -> ()

let check_scalar_subscripted esp =
  match esp.any with
  | Deref(I_scalar, Deref _) -> warn_rule [Warn_complex_expressions] "for complex dereferencing, use \"->\""
  | _ -> ()

let negatable_ops = collect (fun (a, b) -> [ a, b ; b, a ]) [
  "==", "!=" ; 
  "eq", "ne" ;
]

let check_negatable_expr esp =
  match un_parenthesize_full esp.any.expr with
  | Call_op("m//", var :: _, _) when not (is_var_dollar_ var) ->
      warn_rule [Warn_suggest_simpler] "!($var =~ /.../) is better written $var !~ /.../"
  | Call_op("!m//", var :: _, _) when not (is_var_dollar_ var) ->
      warn_rule [Warn_suggest_simpler] "!($var !~ /.../) is better written $var =~ /.../"
  | Call_op(op, _, _) ->
      (try
	let neg_op = List.assoc op negatable_ops in
        warn_rule [Warn_suggest_simpler] (Printf.sprintf "!($foo %s $bar) is better written $foo %s $bar" op neg_op)
      with Not_found -> ())
  | _ -> ()

let check_ternary_paras(cond, a, b) =
  let rec dont_need_short_circuit_rec = function
    | Num _
    | Raw_string _
    | String ([(_, List [])], _) 
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
    | List [] -> warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you may use if_() here\n  beware that the short-circuit semantic of ?: is not kept\n  if you want to keep the short-circuit behaviour, replace () with @{[]} and there will be no warning anymore"
    | _ -> ()
  in
  if dont_need_short_circuit a || is_same_fromparser cond a then check_ternary_para b;
  if dont_need_short_circuit b || is_same_fromparser cond b then check_ternary_para a;
  if is_same_fromparser cond a && is_a_scalar a && is_a_scalar b then warn_rule [Warn_suggest_simpler] "you can replace \"$foo ? $foo : $bar\" with \"$foo || $bar\"";
  [ cond; a; b ]

let check_unneeded_var_dollar_ esp =
  if is_var_dollar_ esp.any.expr then warn [Warn_suggest_simpler] esp.pos "\"$_ =~ /regexp/\" can be written \"/regexp/\"" else
  if is_var_number_match esp.any.expr then warn [Warn_complex_expressions] esp.pos "do not use the result of a match (eg: $1) to match another pattern"
let check_unneeded_var_dollar_not esp =
  if is_var_dollar_ esp.any.expr then warn [Warn_suggest_simpler] esp.pos "\"$_ !~ /regexp/\" can be written \"!/regexp/\"" else
  if is_var_number_match esp.any.expr then warn [Warn_complex_expressions] esp.pos "do not use the result of a match (eg: $1) to match another pattern"
let check_unneeded_var_dollar_s esp = 
  let expr = esp.any.expr in
  if is_var_dollar_ expr then warn [Warn_suggest_simpler] esp.pos "\"$_ =~ s/regexp/.../\" can be written \"s/regexp/.../\"" else
  if is_var_number_match expr then warn [Warn_traps] esp.pos "do not modify the result of a match (eg: $1)" else
  let expr = match expr with
  | List [List [Call_op("=", [ expr; _], _)]] -> expr (* check $xx in ($xx = ...) =~ ... *)
  | _ -> expr in
  if is_a_string expr || not (is_a_scalar expr) then warn [Warn_complex_expressions] esp.pos "you can only use s/// on a variable"

let check_my esp = if esp.any <> "my" then die_rule "syntax error"
let check_foreach esp = if esp.any = "for"     then warn [Warn_normalized_expressions] esp.pos "write \"foreach\" instead of \"for\""
let check_for     esp = if esp.any = "foreach" then warn [Warn_normalized_expressions] esp.pos "write \"for\" instead of \"foreach\""
let check_for_foreach esp arg =
  match arg.any.expr with
  | List [ Deref(I_scalar, _) ] ->
      if esp.any = "foreach" then warn [Warn_normalized_expressions] esp.pos "you are using the special trick to locally set $_ with a value, for this please use \"for\" instead of \"foreach\""
  | List [ Deref_with(context, I_scalar, _, _) ] when context <> I_func -> 
      if esp.any = "foreach" then warn [Warn_normalized_expressions] esp.pos "you are using the special trick to locally set $_ with a value, for this please use \"for\" instead of \"foreach\""
  | List [ Deref(I_hash, _) ] ->
      warn [Warn_traps] esp.pos "foreach with a hash is usually an error"
  | _ -> 
      if esp.any = "for" then warn [Warn_normalized_expressions] esp.pos "write \"foreach\" instead of \"for\""

let check_block_sub esp_lines esp_BRACKET_END =
  match fst esp_lines.any with
  | [] -> 
      sp_0_or_cr esp_BRACKET_END
  | l ->
      (if List.hd l = Semi_colon then sp_0 else sp_p) esp_lines ;
      sp_p esp_BRACKET_END ;

      if esp_BRACKET_END.spaces = Space_cr then
	(if not (snd esp_lines.any) then warn_verb [Warn_white_space] (get_pos_end esp_lines) "missing \";\"")
      else
	(if last l = Semi_colon then warn_verb [Warn_white_space] (get_pos_end esp_lines) "spurious \";\" before closing block")

let check_block_ref esp_lines esp_BRACKET_END =
  let l = esp_lines.any in
  if l <> [] && List.hd l = Semi_colon 
  then (sp_0 esp_lines ; sp_p esp_BRACKET_END)
  else sp_same esp_lines esp_BRACKET_END ;

  if esp_BRACKET_END.spaces <> Space_cr then
    (if l <> [] && last l = Semi_colon then warn_verb [Warn_white_space] (get_pos_end esp_lines) "spurious \";\" before closing block")

let check_unless_else elsif else_ =
  if elsif.any <> [] then warn [Warn_complex_expressions] elsif.pos "don't use \"elsif\" with \"unless\" (replace \"unless\" with \"if\")";
  if else_.any <> [] then warn [Warn_complex_expressions] else_.pos "don't use \"else\" with \"unless\" (replace \"unless\" with \"if\")"

let check_my_our_paren { any = ((comma_closed, _), l) } after_esp = 
  (if l = [] then sp_0 else sp_1) after_esp ; 
  if not comma_closed then die_rule "syntax error"

let check_simple_pattern = function
  | [ String([ st, List [] ], _); Raw_string("", _) ] ->
      if String.length st > 2 &&
	st.[0] = '^' && st.[String.length st - 1] = '$' then
	let st = skip_n_char_ 1 1 st in
	if string_forall_with char_is_alphanumerical_ 0 st then
	  warn_rule [Warn_suggest_simpler] (sprintf "\"... =~ /^%s$/\" is better written \"... eq '%s'\"" st st)
  | _ -> ()

let rec only_one esp =
  match esp.any with
  | [List l'] -> only_one { esp with any = l' }
  | [e] -> e
  | [] -> die_with_rawpos esp.pos "you must give one argument"
  | _  -> die_with_rawpos esp.pos "you must give only one argument"

let only_one_array_ref esp =
  let e = only_one esp in
  (match e with
  | Call_op("last_array_index", [Deref(I_array, e)], _) ->
      warn [Warn_suggest_simpler] esp.pos (sprintf "you can replace $#%s with -1" (string_of_fromparser e))
  | _ -> ());
  e

let only_one_in_List esp =
  match esp.any.expr with
  | List l -> only_one { esp with any = l }
  | e -> e
  
let rec is_only_one_in_List = function
  | [List l] -> is_only_one_in_List l
  | [_] -> true
  | _ -> false

let maybe_to_Raw_string = function
  | Ident(None, s, pos) -> Raw_string(s, pos)
  | Ident(Some fq, s, pos) -> Raw_string(fq ^ "::" ^ s, pos)
  | e -> e

let to_List = function
  | [e] -> e
  | l -> List l

let deref_arraylen e = Call_op("last_array_index", [Deref(I_array, e)], raw_pos2pos bpos)
let deref_raw context e = 
  let e = match e with
  | Raw_string(s, pos) -> 
      let fq, ident = split_name_or_fq_name s in
      Ident(fq, ident, pos)
  | Deref(I_scalar, (Ident _ as ident)) ->
      warn_rule [Warn_suggest_simpler] (sprintf "%s{$%s} can be written %s$%s" (context2s context) (string_of_fromparser ident) (context2s context) (string_of_fromparser ident));
      e
  | _ -> e
  in Deref(context, e)

let to_Ident { any = (fq, name); pos = pos } = Ident(fq, name, raw_pos2pos pos)
let to_Raw_string { any = s; pos = pos } = Raw_string(s, raw_pos2pos pos)
let to_Method_call (object_, method_, para) = 
  match method_ with
  | Ident(Some "SUPER", name, pos) -> Method_call(maybe_to_Raw_string object_, Raw_string(name, pos), para)
  | Ident(Some _, _, _) -> Call(Deref(I_func, method_), maybe_to_Raw_string object_ :: para)
  | _ -> Method_call(maybe_to_Raw_string object_, maybe_to_Raw_string method_, para)
let to_Deref_with(from_context, to_context, ref_, para) =
  if is_not_a_scalar ref_ then warn_rule [] "bad deref";
  Deref_with(from_context, to_context, ref_, para)

let lines_to_Block esp_lines esp_BRACKET_END =
  check_block_sub esp_lines esp_BRACKET_END;
  Block (fst esp_lines.any)
  
let to_Local esp =
  let l = 
    match esp.any.expr with
    | List[List l] -> l
    | e -> [e]
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
    | _ -> die_with_rawpos esp.pos "bad argument to \"local\""
  ) l in
  if local_vars = [] then Call_op("local", local_exprs, raw_pos2pos esp.pos)
  else if local_exprs = [] then My_our("local", local_vars, raw_pos2pos esp.pos)
  else die_with_rawpos esp.pos "bad argument to \"local\""

let sub_declaration (name, proto) body sub_kind = Sub_declaration(name, proto, Block body, sub_kind)
let anonymous_sub proto lines bracket_end = Anonymous_sub (proto, lines_to_Block lines bracket_end, raw_pos2pos lines.pos)
let call_with_same_para_special f = Call(f, [Deref(I_star, (Ident(None, "_", raw_pos2pos bpos)))])
let remove_call_with_same_para_special = function
  | Call(f, [Deref(I_star, (Ident(None, "_", _)))]) -> f
  | e -> e

let check_My_under_condition msg = function
  | List [ My_our("my", _, _) ] ->
      warn_rule [Warn_traps] "this is stupid"
  | List [ Call_op("=", [ My_our("my", _, _); _ ], _) ] ->
      warn_rule [Warn_traps] msg
  | _ -> ()

let cook_call_op op para pos =
  (match op with
  | "le" | "ge" | "eq" | "ne" | "gt" | "lt" | "cmp" ->
      if List.exists (function Num _ -> true | _ -> false) para then
	warn_rule [Warn_traps] (sprintf "you should use a number operator, not the string operator \"%s\" (or replace the number with a string)" op)
  | "." ->
      if List.exists (function Call(Deref(I_func, Ident(None, "N_", _)), _) -> true | _ -> false) para then
	warn_rule [Warn_MDK_Common; Warn_traps] "N_(\"xxx\") . \"yyy\" is dumb since the string \"xxx\" will never get translated"
  | _ -> ());

  (match op, para with
  | "if", List [Call_op ("=", [ _; e ], _)] :: _ when is_always_true e || is_always_false e -> 
      warn_rule [Warn_traps] "are you sure you did not mean \"==\" instead of \"=\"?"

  | "foreach", [ _; Block [ expr ; Semi_colon ] ]
  | "foreach", [ _; Block [ expr ] ] -> 
      (match expr with
      | Call_op("if infix", [ List [ Call(Deref(I_func, Ident(None, "push", _)), [ Deref(I_array, Ident _) as l ; Deref(I_scalar, Ident(None, "_", _)) ]) ] ; _ ], _) ->
	  let l = string_of_fromparser l in
	  warn_rule [Warn_suggest_functional] (sprintf "use \"push %s, grep { ... } ...\" instead of \"foreach (...) { push %s, $_ if ... }\"\n  or sometimes \"%s = grep { ... } ...\"" l l l)
      | Call_op("if infix", [ List [ Call(Deref(I_func, Ident(None, "push", _)), [ Deref(I_array, Ident _) as l; _ ]) ] ; _ ], _) ->
	  let l = string_of_fromparser l in
	  warn_rule [Warn_suggest_functional] (sprintf "use \"push %s, map { ... ? ... : () } ...\" instead of \"foreach (...) { push %s, ... if ... }\"\n  or sometimes \"%s = map { ... ? ... : () } ...\"\n  or sometimes \"%s = map { if_(..., ...) } ...\"" l l l l)
      | List [ Call(Deref(I_func, Ident(None, "push", _)), [ Deref(I_array, Ident _) as l; _ ]) ] ->
	  let l = string_of_fromparser l in
	  warn_rule [Warn_suggest_functional] (sprintf "use \"push %s, map { ... } ...\" instead of \"foreach (...) { push %s, ... }\"\n  or sometimes \"%s = map { ... } ...\"" l l l)
      | _ -> ())

  | "=", [My_our _; Ident(None, "undef", _)] -> 
      warn [Warn_suggest_simpler] pos "no need to initialize variable, it's done by default"
  | "=", [My_our _; List[]] -> 
      if Info.is_on_same_line_current pos then warn [Warn_suggest_simpler] pos "no need to initialize variables, it's done by default"

  | "=", [ Deref_with(I_array, I_scalar, id, Deref(I_array, id_)); _ ] when is_same_fromparser id id_ ->
      warn_rule [Warn_suggest_simpler] "\"$a[@a] = ...\" is better written \"push @a, ...\""

  | "=", [ Deref(I_star, String ([(sf1, List [])], _)); _ ] ->
      warn_rule [Warn_help_perl_checker] (sprintf "write *{'%s'} instead of *{\"%s\"}" sf1 sf1)

  | "||=", List [ List _ ] :: _
  | "&&=", List [ List _ ] :: _ -> warn_rule [Warn_complex_expressions] "remove the parentheses"
  | "||=", e :: _
  | "&&=", e :: _ -> if is_not_a_scalar e then warn_rule [Warn_traps] (sprintf "\"%s\" is only useful with a scalar" op)

  | "==", [Call_op("last_array_index", _, _); Num(n, _)] ->
      warn_rule [Warn_suggest_simpler] (sprintf "$#x == %s is better written @x == %d" n (1 + int_of_string n))
  | "==", [Call_op("last_array_index", _, _); Call_op("- unary", [Num (n, _)], _)] ->
      warn_rule [Warn_suggest_simpler] (sprintf "$#x == -%s is better written @x == %d" n (1 - int_of_string n))


  | "||", e :: _ when is_always_true  e -> warn_rule [Warn_strange] "<constant> || ... is the same as <constant>"
  | "&&", e :: _ when is_always_false e -> warn_rule [Warn_strange] "<constant> && ... is the same as <constant>"
  | "||", e :: _ when is_always_false e -> warn_rule [Warn_strange] "<constant> || ... is the same as ..."
  | "&&", e :: _ when is_always_true  e -> warn_rule [Warn_strange] "<constant> && ... is the same as ..."

  | "or",  e :: _ when is_always_true  (un_parenthesize_full e) -> warn_rule [Warn_strange] "<constant> or ... is the same as <constant>"
  | "and", e :: _ when is_always_false (un_parenthesize_full e) -> warn_rule [Warn_strange] "<constant> and ... is the same as <constant>"
  | "or",  e :: _ when is_always_false (un_parenthesize_full e) -> warn_rule [Warn_strange] "<constant> or ... is the same as ..."
  | "and", e :: _ when is_always_true  (un_parenthesize_full e) -> warn_rule [Warn_strange] "<constant> and ... is the same as ..."

  | "or", [ List [ Deref(I_scalar, id) ]; List [ Call_op("=", [ Deref(I_scalar, id_); _], _) ] ] when is_same_fromparser id id_ ->
      warn_rule [Warn_suggest_simpler] "\"$foo or $foo = ...\" can be written \"$foo ||= ...\""

  | "and", [ _cond ; expr ] -> check_My_under_condition "replace \"<cond> and my $foo = ...\" with \"my $foo = <cond> && ...\"" expr
  | "or",  [ _cond ; expr ] -> check_My_under_condition "replace \"<cond> or my $foo = ...\" with \"my $foo = !<cond> && ...\"" expr
      
  | _ -> ());

  match op, para with
  | "=", [ Deref(I_star, (Ident _ as f1)); Deref(I_star, (Ident _ as f2)) ] ->
      let s1, s2 = string_of_fromparser f1, string_of_fromparser f2 in
      warn [Warn_complex_expressions] pos (sprintf "\"*%s = *%s\" is better written \"*%s = \\&%s\"" s1 s2 s1 s2) ;
      sub_declaration (f1, None) [ call_with_same_para_special(Deref(I_func, f2)) ] Glob_assign
  | "=", [ Deref(I_star, Raw_string(sf1, pos_f1)); Deref(I_star, (Ident _ as f2)) ] ->
      let s2 = string_of_fromparser f2 in
      warn [Warn_help_perl_checker] pos (sprintf "\"*{'%s'} = *%s\" is better written \"*{'%s'} = \\&%s\"" sf1 s2 sf1 s2) ;
      sub_declaration (Ident(None, sf1, pos_f1), None) [ call_with_same_para_special(Deref(I_func, f2)) ] Glob_assign

  | "=", [ Deref(I_star, (Ident _ as f1)); Ref(I_scalar, Deref(I_func, (Ident _ as f2))) ] ->
      sub_declaration (f1, None) [ call_with_same_para_special(Deref(I_func, f2)) ] Glob_assign
  | "=", [ Deref(I_star, Raw_string(sf1, pos_f1)); Ref(I_scalar, Deref(I_func, (Ident _ as f2))) ] ->
      sub_declaration (Ident(None, sf1, pos_f1), None) [ call_with_same_para_special(Deref(I_func, f2)) ] Glob_assign

  | "=", [ Deref(I_star, (Ident _ as f1)); (Anonymous_sub(proto, sub, _)) ] ->
      sub_declaration (f1, proto) [ sub ] Glob_assign

  | _ -> Call_op(op, para, raw_pos2pos pos)

let to_Call_op mcontext op para esp_start esp_end = 
  let pos = raw_pos_range esp_start esp_end in
  new_any mcontext (cook_call_op op para pos) esp_start.spaces pos
let to_Call_op_ mcontext prio op para esp_start esp_end = 
  let pos = raw_pos_range esp_start esp_end in
  new_any mcontext { priority = prio ; expr = cook_call_op op para pos } esp_start.spaces pos
let to_Call_assign_op_ mcontext prio op left right esp_left esp_end = 
  if not (is_lvalue left) then warn [Warn_strange] esp_left.pos "invalid lvalue";
  to_Call_op_ mcontext prio op [ left ; right ] esp_left esp_end

let followed_by_comma expr true_comma =
  if true_comma then expr else
    match split_last expr with
    | l, Ident(None, s, pos) -> l @ [Raw_string(s, pos)]
    | _ -> expr


let pot_strings = Hashtbl.create 16
let po_comments = ref []
let po_comment esp = lpush po_comments esp.any

let check_format_a_la_printf s pos =
  let rec check_format_a_la_printf_ contexts i =
    try
      let i' = String.index_from s i '%' in
      try
	let contexts = 
	  match s.[i' + 1] with
	  | '%' -> contexts
	  | 'd' -> M_int :: contexts
	  | 's' | 'c' -> M_string :: contexts
	  | c -> warn [Warn_strange] (pos + i', pos + i') (sprintf "invalid command %%%c" c); contexts
	in
	check_format_a_la_printf_ contexts (i' + 2)
      with Invalid_argument _ -> warn [Warn_strange] (pos + i', pos + i') "invalid command %" ; contexts
    with Not_found -> contexts
  in check_format_a_la_printf_ [] 0
  
let generate_pot file = 
  let fd = open_out file in
  output_string fd 
("# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR Free Software Foundation, Inc.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"POT-Creation-Date: " ^ input_line (Unix.open_process_in "date '+%Y-%m-%d %H:%M%z'") ^ "\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: 8-bit\\n\"

") ;

  let rec print_formatted_char = function
    | '"'  -> output_char fd '\\'; output_char fd '"'
    | '\t' -> output_char fd '\\'; output_char fd 't'
    | '\\' -> output_char fd '\\'; output_char fd '\\'
    | '\n' -> output_string fd "\\n\"\n\""
    | c -> output_char fd c
  in
  let sorted_pot_strings = List.sort (fun (_, pos_a) (_, pos_b) -> compare pos_a pos_b)
      (Hashtbl.fold (fun k (v, _) l -> (k,v) :: l) pot_strings [] ) in
  List.iter (fun (s, _) ->
    match Hashtbl.find_all pot_strings s with
    | [] -> ()
    | l ->
	List.iter (fun _ -> Hashtbl.remove pot_strings s) l ;

	List.iter (fun po_comment -> output_string fd ("#. " ^ po_comment ^ "\n")) (collect snd l);

	let pos_l = List.sort compare (List.map fst l) in
	fprintf fd "#: %s\n" (String.concat " " (List.map Info.pos2s_for_po pos_l)) ;
	output_string fd "#, c-format\n" ;

	output_string fd (if String.contains s '\n' then "msgid \"\"\n\"" else "msgid \"") ;
	String.iter print_formatted_char s ;
	output_string fd "\"\n" ;
	output_string fd "msgstr \"\"\n\n"
  ) sorted_pot_strings ;      
  close_out fd

let check_system_call = function
  | "mkdir" :: l ->
      let has_p = List.exists (str_begins_with "-p") l in
      let has_m = List.exists (str_begins_with "-m") l in
      if has_p && has_m then ()
      else if has_p then warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace system(\"mkdir -p ...\") with mkdir_p(...)"
      else if has_m then warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace system(\"mkdir -m <mode> ...\") with mkdir(..., <mode>)"
      else warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace system(\"mkdir ...\") with mkdir(...)"
  | _ -> ()

let call_raw force_non_builtin_func (e, para) =
  let check_anonymous_block f = function
  | [ Anonymous_sub _ ; Deref (I_hash, _) ] ->
      warn_rule [Warn_strange] ("a hash is not a valid parameter to function " ^ f)

  | Anonymous_sub _ :: _ -> ()
  | _ -> warn_rule [Warn_complex_expressions] (sprintf "always use \"%s\" with a block (eg: %s { ... } @list)" f f)
  in

  match e with
  | Deref(I_func, Ident(None, f, _)) ->
      (match f with
      | "join" ->
	  (match un_parenthesize_full_l para with
	  | e :: _ when not (is_a_scalar e) -> warn_rule [Warn_traps] "first argument of join() must be a scalar";
	  | [_] -> warn_rule [Warn_traps] "not enough parameters"
	  | [_; e] when is_a_scalar e -> warn_rule [Warn_traps] "join('...', $foo) is the same as $foo"
	  | _ -> ())

      | "length" ->
	  if para = [] then warn_rule [Warn_complex_expressions] (sprintf "please use \"%s $_\" instead of \"%s\"" f f) else
	  if is_not_a_scalar (List.hd para) then warn_rule [Warn_traps] "never use \"length @l\", it returns the length of the string int(@l)" ;

      | "open" ->
	  (match para with
	  | [ List(Ident(None, name, _) :: _) ]
	  | Ident(None, name, _) :: _ ->
	      if not (List.mem name [ "STDIN" ; "STDOUT" ; "STDERR" ]) then
		warn_rule [Warn_complex_expressions] (sprintf "use a scalar instead of a bareword (eg: occurrences of %s with $%s)" name name)
	  | _ -> ())

      | "N" | "N_" ->
	  (match para with
	  | [ List(String([ s, List [] ], (_, pos_offset, _ as pos)) :: para) ] -> 
	      if !Flags.generate_pot then (
		Hashtbl.add pot_strings s (pos, !po_comments) ;
		po_comments := []
	      ) ;
	      let contexts = check_format_a_la_printf s pos_offset in
	      if f = "N" then
		if List.length para < List.length contexts then
		  warn_rule [Warn_traps; Warn_MDK_Common] "not enough parameters"
		else if List.length para > List.length contexts then
		  warn_rule [Warn_traps; Warn_MDK_Common] "too many parameters" ;
	      (*if String.contains s '\t' then warn_rule "tabulation in translated string must be written \\\\t";*)
	      (*if count_matching_char s '\n' > 10 then warn_rule "long string";*)
	  | [ List(String _ :: _) ] -> die_rule "don't use interpolated translated string, use %s or %d instead"
	  |  _ -> die_rule (sprintf "%s() must be used with a string" f))

      | "if_" ->
	  (match para with
	  | [ List [ _ ] ] -> warn_rule [Warn_traps; Warn_MDK_Common] "not enough parameters";
	  | _ -> ())

      | "map" ->
	  (match para with

	  | Anonymous_sub(None, Block [ List [ Call(Deref(I_func, Ident(None, "if_", _)),
						    [ List [ _ ; Deref(I_scalar, Ident(None, "_", _)) ] ]) ] ], _) :: _ ->
						      warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace \"map { if_(..., $_) }\" with \"grep { ... }\""
	  | _ -> check_anonymous_block f para)

      | "grep" ->
	  (match para with
	  | [ Anonymous_sub(None, Block [ List [ Call_op("not", [ Call(Deref(I_func, Ident(None, "member", _)), [ List(Deref(I_scalar, Ident(None, "_", _)) :: _) ]) ], _) ] ], _); _ ] ->
	      warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace \"grep { !member($_, ...) } @l\" with \"difference2([ @l ], [ ... ])\""
	  | [ Anonymous_sub(None, Block [ List [ Call(Deref(I_func, Ident(None, "member", _)), [ List(Deref(I_scalar, Ident(None, "_", _)) :: _) ]) ] ], _); _ ] ->
	      warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace \"grep { member($_, ...) } @l\" with \"intersection([ @l ], [ ... ])\""
	  | _ -> check_anonymous_block f para)

      | "any" ->
	  (match para with
	    [Anonymous_sub (None, Block 
			      [ List [ Call_op("eq", [Deref(I_scalar, Ident(None, "_", _)); _ ], _) ] ], 
			    _); _ ] ->
	      warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace \"any { $_ eq ... } @l\" with \"member(..., @l)\""
	  | _ -> check_anonymous_block f para)		

      | "grep_index" | "map_index" | "partition" | "uniq_"
      | "find"
      | "every"
      | "find_index"
      | "each_index" -> check_anonymous_block f para

      | "member" ->
	  (match para with
	    [ List [ _; Call(Deref(I_func, Ident(None, "keys", _)), _) ] ] ->
	      warn_rule [Warn_suggest_simpler; Warn_MDK_Common] "you can replace \"member($xxx, keys %yyy)\" with \"exists $yyy{$xxx}\""
	  | _ -> ())

      | "pop" | "shift" ->
	  (match para with
	  | [] 
	  | [ Deref(I_array, _) ] 
	  | [ List [ Deref(I_array, _) ] ] -> ()
	  | _ -> warn_rule [Warn_traps] (f ^ " is expecting an array and nothing else"))

      | "push" | "unshift" ->
	  (match para with
	  | Deref(I_array, _) :: l 
	  | [ List (Deref(I_array, _) :: l) ] -> 
	      if l = [] then warn_rule [Warn_traps] ("you must give some arguments to " ^ f)
	  | _ -> warn_rule [Warn_traps] (f ^ " is expecting an array"))

      | "system" ->
	  let fake_string_option_from_expr = function
	    | String(l, _) -> Some(String.concat "" (List.map fst l))
	    | Raw_string(s, _) -> Some s
	    | _ -> None
	  in
	  (match un_parenthesize_full_l para with
	  | [ e ] ->
	      (match fake_string_option_from_expr e with
	      | Some s ->
		  if List.exists (String.contains s) [ '\'' ; char_quote ] &&
	            not (List.exists (String.contains s) [ '<' ; '>' ; '&' ; ';']) then
		    warn_rule [Warn_complex_expressions] "instead of quoting parameters you should give a list of arguments";
		  check_system_call (split_at ' ' s)
	      | None -> ())
	  | l -> 
	      let l' = filter_some_with fake_string_option_from_expr l in
	      check_system_call l')
      | _ -> ()
      );

      let para' = match f with
      | "no" ->
	  (match para with
	  | [ Ident(_, _, pos) as s ] -> Some [ Raw_string(string_of_fromparser s, pos) ]
	  | [ Call(Deref(I_func, (Ident(_, _, pos) as s)), l) ] -> Some(Raw_string(string_of_fromparser s, pos) :: l)
	  | _ -> die_rule "use \"no PACKAGE <para>\"")
      | "undef" ->
	  (match para with
	  | [ Deref(I_star, ident) ] -> Some [ Deref(I_func, ident) ]
	  | _ -> None)

      | "goto" ->
	  (match para with
	  | [ Ident(None, s, pos) ] -> Some [ Raw_string(s, pos) ]
	  | _ -> None)

      | "last" | "next" | "redo" when not force_non_builtin_func ->
	  (match para with
	  | [ Ident(None, s, pos) ] -> Some [ Raw_string(s, pos) ]
	  | _ -> die_rule (sprintf "%s must be used with a raw string" f))

      | "split" ->
	  (match para with
	  | [ List(Call_op("m//", Deref(I_scalar, Ident(None, "_", _)) :: pattern, pos) :: l) ]
	  | Call_op("m//", Deref(I_scalar, Ident(None, "_", _)) :: pattern, pos) :: l ->
	      Some(Call_op("qr//", pattern, pos) :: l)
	  | _ -> None)
	    
      | _ -> None
      in Call(e, some_or para' para)
  | _ -> Call(e, para)

let call(e, para) = call_raw false (e, para)

let check_return esp_func esp_para =
  match esp_func.any with
  | Ident(None, "return", _) -> 
      prio_lo_check P_call_no_paren esp_para.any.priority esp_para.pos (List esp_para.any.expr)
  | _ -> ()

let call_and_context(e, para) force_non_builtin_func priority esp_start esp_end =
  let context = 
    match e with
    | Deref(I_func, Ident(None, f, _)) -> function_to_context false f
    | _ -> M_unknown
  in
  new_pesp context priority (call_raw force_non_builtin_func (e, para)) esp_start esp_end

let call_no_paren   esp_func esp_para = check_return esp_func esp_para; call_and_context(Deref(I_func, esp_func.any), esp_para.any.expr) false P_call_no_paren esp_func esp_para
let call_with_paren esp_func esp_para = check_return esp_func esp_para; call_and_context (Deref(I_func, esp_func.any), esp_para.any.expr) false P_tok esp_func esp_para

let call_func esp_func esp_para = 
  call_and_context(esp_func.any, esp_para.any.expr) true P_tok esp_func esp_para

let call_one_scalar_para { any = e ; pos = pos } para esp_start esp_end =
  let para =
    match para with
    | [] ->
	  if e = "shift" || e = "pop" then 
	    [] (* can't decide here *)
	  else
	    (if not (List.mem e [ "length" ]) then warn_rule [Warn_complex_expressions] (sprintf "please use \"%s $_\" instead of \"%s\"" e e) ;
	     [var_dollar_ (raw_pos2pos pos)])
    | _ -> para
  in
  new_pesp M_unknown P_mul (call(Deref(I_func, Ident(None, e, raw_pos2pos pos)), para)) esp_start esp_end


let (current_lexbuf : Lexing.lexbuf option ref) = ref None

let rec list2tokens l =
  let rl = ref l in
  fun lexbuf ->
    match !rl with
    | [] -> internal_error "list2tokens"
    | ((start, end_), e) :: l -> 
	(* HACK: fake a normal lexbuf *)
	lexbuf.Lexing.lex_start_p <- { Lexing.dummy_pos with Lexing.pos_cnum = start } ;
	lexbuf.Lexing.lex_curr_p <- { Lexing.dummy_pos with Lexing.pos_cnum = end_ } ;
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

let to_String parse strict { any = l ; pos = pos } = 
  let l' = parse_interpolated parse l in
  (match l' with
  | [ "", List [Deref(I_scalar, Ident(None, ident, _))]] -> 
      if ident <> "!" && strict then warn [Warn_suggest_simpler] pos (sprintf "%s is better written without the double quotes" (variable2s(I_scalar, ident)))
  | [ "", List [Deref(I_hash, _)]] -> 
      warn [Warn_traps] pos "don't use a hash in string context"
  | [ "", List [Deref(I_array, _)]] -> 
      ()
  | [("", _)] -> 
      if strict then warn [Warn_suggest_simpler] pos "double quotes are unneeded"
  | _ -> ());
  String(l', raw_pos2pos pos)

let from_PATTERN parse { any = (s, opts) ; pos = pos } = 
  let re = parse_interpolated parse s in
  (match List.rev re with
  | (s, List []) :: _ ->
      if str_ends_with s ".*" then
	warn_rule [Warn_suggest_simpler] (sprintf "you can remove \"%s\" at the end of your regexp" ".*")
      else if str_ends_with s ".*$" then
	warn_rule [Warn_suggest_simpler] (sprintf "you can remove \"%s\" at the end of your regexp" ".*$")
  | _ -> ());
  let pattern = [ String(re, raw_pos2pos pos) ; Raw_string(opts, raw_pos2pos pos) ] in
  check_simple_pattern pattern;
  pattern

let from_PATTERN_SUBST parse { any = (s1, s2, opts) ; pos = pos } = 
  [ String(parse_interpolated parse s1, raw_pos2pos pos) ; 
    String(parse_interpolated parse s2, raw_pos2pos pos) ; 
    Raw_string(opts, raw_pos2pos pos) ]


let rec mcontext2s = function
  | M_none -> "()"

  | M_bool -> "bool"

  | M_int -> "int"
  | M_float -> "float"
  | M_string -> "string"
  | M_ref c -> "ref(" ^ mcontext2s c ^ ")"
  | M_revision -> "revision"
  | M_undef -> "undef"
  | M_sub -> "sub"
  | M_unknown_scalar -> "scalar"

  | M_tuple l -> "tuple(" ^ String.concat ", " (List.map mcontext2s l) ^ ")"
  | M_list -> "list"
  | M_array -> "array"
  | M_hash -> "hash"

  | M_special -> "special"
  | M_unknown -> "unknown"
  | M_mixed l -> String.concat " | " (List.map mcontext2s l)

let rec mcontext_lower c1 c2 =
  match c1, c2 with
  | M_special, _ | _, M_special -> internal_error "M_special in mcontext_compare"

  | M_unknown, _
  | _, M_unknown -> true

  | M_mixed l, c -> List.exists (fun a -> mcontext_lower a c) l
  | c, M_mixed l -> List.exists (mcontext_lower c) l

  | M_none, M_none | M_sub, M_sub | M_hash, M_hash | M_hash, M_bool -> true
  | M_none, _      | M_sub, _     | M_hash, _      -> false

  | _, M_list -> true

  | M_list, M_bool
  | M_list, M_tuple _

    (* M_unknown_scalar is M_mixed [ M_int ; M_float ; M_string ; M_bool ; M_ref _ ; M_revision ; M_undef ] *)
  | M_unknown_scalar, M_int | M_unknown_scalar, M_float | M_unknown_scalar, M_string | M_unknown_scalar, M_bool 
  | M_unknown_scalar, M_ref _ | M_unknown_scalar, M_revision | M_unknown_scalar, M_undef | M_unknown_scalar, M_unknown_scalar

  | M_array, M_array | M_array, M_int | M_array, M_float | M_array, M_bool | M_array, M_unknown_scalar | M_array, M_tuple _
  | M_int, M_int | M_int, M_float | M_int, M_string | M_int, M_bool | M_int, M_unknown_scalar
  | M_float, M_float | M_float, M_string | M_float, M_bool | M_float, M_unknown_scalar
  | M_string, M_string | M_string, M_bool | M_string, M_unknown_scalar
  | M_bool, M_bool | M_bool, M_unknown_scalar

  | M_ref _, M_unknown_scalar
  | M_revision, M_revision | M_revision, M_unknown_scalar
  | M_undef, M_undef | M_undef, M_unknown_scalar

    -> true

  | M_tuple t1, M_tuple t2 -> 
      List.length t1 = List.length t2 && for_all2_true mcontext_lower t1 t2

  | M_tuple [c], M_int | M_tuple [c], M_float | M_tuple [c], M_string | M_tuple [c], M_bool 
  | M_tuple [c], M_ref _ | M_tuple [c], M_revision | M_tuple [c], M_undef | M_tuple [c], M_unknown_scalar 
    -> mcontext_lower c c2

(*  | M_ref a, M_ref b -> mcontext_lower a b *)

  | _ -> false

let mcontext_is_scalar = function
  | M_unknown -> false
  | c -> mcontext_lower c M_unknown_scalar

let mcontext_to_scalar = function
  | M_array -> M_int
  | c -> if mcontext_is_scalar c then c else M_unknown_scalar

let mcontext_merge_raw c1 c2 =
  match c1, c2 with
  | M_unknown, _ | _, M_unknown -> Some M_unknown
  | M_unknown_scalar, c when mcontext_is_scalar c -> Some M_unknown_scalar
  | c, M_unknown_scalar when mcontext_is_scalar c -> Some M_unknown_scalar
  | M_mixed _, _ | _, M_mixed _ -> internal_error "mcontext_merge_raw"
  | _ -> 
      if mcontext_lower c1 c2 then Some c2 else
      if mcontext_lower c2 c1 then Some c1 else
      if c1 = c2 then Some c1 else
      None

let rec mcontext_lmerge_add l = function
  | M_mixed l2 -> List.fold_left mcontext_lmerge_add [] (l2 @ l)
  | c ->
      let rec add_to = function
	| [] -> [c]
	| M_mixed subl :: l -> add_to (subl @ l)
	| c2 :: l ->
	    match mcontext_merge_raw c c2 with
	    | Some c' -> c' :: l
	    | None -> c2 :: add_to l
      in add_to l

let mcontext_lmerge l =
  match List.fold_left mcontext_lmerge_add [] l with
  | [] -> internal_error "mcontext_lmerge"
  | [c] -> c
  | l -> M_mixed l

let mcontext_merge c1 c2 = mcontext_lmerge [ c1 ; c2 ]

let mcontext_lmaybe esp = if esp.any = [] then [] else [esp.mcontext]

let mcontext_check_raw wanted_mcontext mcontext =
  if not (mcontext_lower mcontext wanted_mcontext) then
    warn_rule [Warn_context] (sprintf "context %s is not compatible with context %s" (mcontext2s mcontext) (mcontext2s wanted_mcontext))

let mcontext_check wanted_mcontext esp =
  (match wanted_mcontext with
  | M_list | M_array | M_float | M_mixed [M_array; M_none] | M_tuple _ -> ()
  | _ ->
    match un_parenthesize_full esp.any.expr with
    | Call(Deref(I_func, Ident(None, "grep", _)), _) -> 
	warn_rule [Warn_suggest_simpler; Warn_help_perl_checker] (if wanted_mcontext = M_bool then
	  "in boolean context, use \"any\" instead of \"grep\"" else
	  "you may use \"find\" instead of \"grep\"")
    | _ -> ());
  mcontext_check_raw wanted_mcontext esp.mcontext

let mcontext_check_unop_l wanted_mcontext esp = 
  mcontext_check wanted_mcontext { esp with any = { esp.any with expr = List esp.any.expr  } }

let mcontext_check_non_none esp =
  if esp.mcontext = M_none then warn_rule [Warn_context] "() context not accepted here"

let mcontext_check_none msg expr esp =
  let rec mcontext_check_none_rec msg expr = function
    | M_none | M_unknown -> ()
    | M_mixed l when List.exists (fun c -> c = M_none) l -> ()
    | M_tuple l ->
	(match expr with
	| [List l_expr]
	| [List l_expr ; Semi_colon] ->
	    let rec iter = function
	      | e::l_expr, mcontext::l ->
		  mcontext_check_none_rec (if l = [] then msg else "value is dropped") [e] mcontext ;
		  iter (l_expr, l)
	      | [], [] -> ()
	      | _ -> internal_error "mcontext_check_none"
	    in iter (un_parenthesize_full_l l_expr, l)
	| _ -> internal_error "mcontext_check_none")
    | _ -> 
	match expr with
	| [List [Num("1", _)]; Semi_colon] -> () (* allow "1;" for package return value. It would be much better to check we are at toplevel, but hell i don't want to wire this information up to here *)
	| [List [Call_op ("<>", [Ident (None, "STDIN", _)], _)]; Semi_colon] -> () (* allow <STDIN> to ask "press return" *)
	| [List [Call(Deref(I_func, Ident(None, "map", _)), _)]; Semi_colon] -> warn_rule [Warn_void] "if you don't use the return value, use \"foreach\" instead of \"map\""
	| _ -> warn [Warn_void] esp.pos msg
  in
  mcontext_check_none_rec msg expr esp.mcontext

(* only returns M_float when there is at least one float *)
let mcontext_float_or_int l = 
  List.iter (mcontext_check_raw M_float) l;
  if List.mem M_float l then M_float else M_int 

let mcontext_op_assign left right =
  mcontext_check_non_none right;

  let left_mcontext =
    match left.mcontext with
    | M_mixed [ c ; M_none ] -> c
    | c -> c
  in

  let wanted_mcontext = match left_mcontext with
  | M_array -> M_list
  | M_hash -> M_mixed [ M_hash ; M_list ]
  | m -> m
  in
  mcontext_check wanted_mcontext right;

  let return_mcontext =
    match left_mcontext with
    | M_tuple _ -> M_array
    | c -> c
  in
  mcontext_merge return_mcontext M_none

let mtuple_context_concat c1 c2 =
  match c1, c2 with
  | M_array, _ | _, M_array
  | M_hash, _ | _, M_hash -> M_list
  | M_tuple l, _ -> M_tuple (l @ [c2])
  | _ -> M_tuple [c1 ; c2]

let call_op_if_infix left right esp_start esp_end =
  (match left, right with
  | List [Call_op("=", [Deref(context, _); _], _)], _ when non_scalar_context context -> ()
  | List [Call_op("=", [v; _], _)],
    List [Call_op("not", [v'], _)] when is_same_fromparser v v' ->
      warn_rule [Warn_suggest_simpler] "\"$foo = ... if !$foo\" can be written \"$foo ||= ...\""
  | _ -> ());

  mcontext_check_none "value is dropped" [left] esp_start;
  (match right with 
  | List [ Num("0", _)] -> () (* allow my $x if 0 *)
  | _ -> check_My_under_condition "replace \"my $foo = ... if <cond>\" with \"my $foo = <cond> && ...\"" left);

  let pos = raw_pos_range esp_start esp_end in
  new_any M_none (Call_op("if infix", [ left ; right], raw_pos2pos pos)) esp_start.spaces pos

let call_op_unless_infix left right esp_start esp_end =
  (match left, right with
  | List [Call_op("=", [Deref(context, _); _], _)], _ when non_scalar_context context -> ()
  | List [Call_op("=", [v; _], _)], List [v'] when is_same_fromparser v v' ->
      warn_rule [Warn_suggest_simpler] "\"$foo = ... unless $foo\" can be written \"$foo ||= ...\""
  | _ -> ());
  (match right with
  | List [Call_op(op, _, _)] ->
      (match op with
      | "&&" | "||" | "not" | "ne" | "?:" -> warn_rule [Warn_complex_expressions] "don't use \"unless\" when the condition is complex, use \"if\" instead"
      | _ -> ());
  | _ -> ());

  mcontext_check_none "value is dropped" [left] esp_start;
  check_My_under_condition "replace \"my $foo = ... unless <cond>\" with \"my $foo = !<cond> && ...\"" left;

  let pos = raw_pos_range esp_start esp_end in
  new_any M_none (Call_op("unless infix", [ left ; right], raw_pos2pos pos)) esp_start.spaces pos

let symops pri para_context return_context op_str left op right =
  sp_same op right;
  let skip_context_check =
    (op_str = "==" || op_str = "!=") && (match left.any.expr, right.any.expr with
    | Deref(I_array, _), List [] -> true (* allow @l == () and @l != () *)
    | _ -> false)
  in
  if op_str <> "==" && op_str <> "!=" && para_context = M_float then
    (match un_parenthesize_full left.any.expr with
    | Call_op("last_array_index", _, _) -> warn_rule [Warn_complex_expressions] "change your expression to use @xxx instead of $#xxx"
    | _ -> ());

  if not skip_context_check then
    (mcontext_check para_context left ; mcontext_check para_context right) ;
  to_Call_op_ return_context pri op_str [prio_lo pri left; prio_lo_after pri right] left right
