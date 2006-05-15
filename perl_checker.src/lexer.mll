{ (* -*- caml -*- *)
open Common
open Types
open Lexing
open Info

let bpos = -1,-1

type raw_token =
  | EOF of raw_pos
  | SPACE of int
  | CR
  | INT of (string * raw_pos)
  | FLOAT of (string * raw_pos)
  | RAW_STRING of (string * raw_pos)
  | STRING of (raw_interpolated_string * raw_pos)
  | PATTERN of (raw_interpolated_string * string * raw_pos)
  | QR_PATTERN of (raw_interpolated_string * string * raw_pos)
  | PATTERN_SUBST of (raw_interpolated_string * raw_interpolated_string * string * raw_pos)
  | BAREWORD of (string * raw_pos)
  | BAREWORD_PAREN of (string * raw_pos)
  | REVISION of (string * raw_pos)
  | PERL_CHECKER_COMMENT of (string * raw_pos)
  | PO_COMMENT of (string * raw_pos)
  | POD of (string * raw_pos)
  | LABEL of (string * raw_pos)
  | COMMAND_STRING of (raw_interpolated_string * raw_pos)
  | PRINT_TO_STAR of ((string * string) * raw_pos)
  | PRINT_TO_SCALAR of ((string * string) * raw_pos)
  | QUOTEWORDS of (string * raw_pos)
  | COMPACT_HASH_SUBSCRIPT of (string * raw_pos)
  | RAW_HERE_DOC of ((string * raw_pos) ref * raw_pos)
  | HERE_DOC of (raw_interpolated_string * raw_pos) ref * raw_pos
  | FORMAT of (raw_interpolated_string * raw_pos) ref * raw_pos
  | SCALAR_IDENT of (string option * string * raw_pos)
  | ARRAY_IDENT of (string option * string * raw_pos)
  | HASH_IDENT of (string option * string * raw_pos)
  | FUNC_IDENT of (string option * string * raw_pos)
  | STAR_IDENT of (string option * string * raw_pos)
  | RAW_IDENT of (string option * string * raw_pos)
  | RAW_IDENT_PAREN of (string option * string * raw_pos)
  | ARRAYLEN_IDENT of (string option * string * raw_pos)
  | SUB_WITH_PROTO of (string * raw_pos)
  | FUNC_DECL_WITH_PROTO of (string option * string * string * raw_pos)

  | IF of raw_pos | ELSIF of raw_pos | ELSE of raw_pos | UNLESS of raw_pos | DO of raw_pos | WHILE of raw_pos | UNTIL of raw_pos | MY_OUR of (string * raw_pos) | CONTINUE of raw_pos | SUB of raw_pos
  | LOCAL of raw_pos | FOR of (string * raw_pos) | USE of raw_pos | PACKAGE of raw_pos | BEGIN of raw_pos | END of raw_pos | PRINT of (string * raw_pos) 
  | NEW of (raw_pos) | AT of raw_pos | DOLLAR of raw_pos | PERCENT of raw_pos | AMPERSAND of raw_pos
  | STAR of raw_pos | ARRAYLEN of raw_pos | SEMI_COLON of raw_pos | PKG_SCOPE of raw_pos | PAREN of raw_pos | PAREN_END of raw_pos | BRACKET of raw_pos
  | BRACKET_END of raw_pos | BRACKET_HASHREF of raw_pos | ARRAYREF of raw_pos | ARRAYREF_END of raw_pos | ARROW of raw_pos | INCR of raw_pos | DECR of raw_pos
  | CONCAT of raw_pos | POWER of raw_pos | TIGHT_NOT of raw_pos | BIT_NEG of raw_pos | REF of raw_pos | ONE_SCALAR_PARA of (string * raw_pos) | PATTERN_MATCH of raw_pos | PATTERN_MATCH_NOT of raw_pos | MULT of (string * raw_pos) | MULT_L_STR of raw_pos
  | PLUS of (string * raw_pos) | BIT_SHIFT of (string * raw_pos)
  | LT of raw_pos | GT of raw_pos | COMPARE_OP of (string * raw_pos) | COMPARE_OP_STR of (string * raw_pos) | EQ_OP of (string * raw_pos) | EQ_OP_STR of (string * raw_pos)
  | BIT_AND of raw_pos | BIT_OR of raw_pos | BIT_XOR of raw_pos | AND_TIGHT of raw_pos | OR_TIGHT of raw_pos | DOTDOT of (string * raw_pos)
  | QUESTION_MARK of raw_pos | COLON of raw_pos | ASSIGN of (string * raw_pos) | COMMA of raw_pos | RIGHT_ARROW of raw_pos | NOT of raw_pos | AND of raw_pos | OR of raw_pos | XOR of raw_pos

and raw_interpolated_string = (string * raw_token list) list

let new_any mcontext any spaces pos = { mcontext = mcontext ; any = any ; spaces = spaces ; pos = pos }

let pos lexbuf = lexeme_start lexbuf, lexeme_end lexbuf
let pos2sfull_with start end_ = Info.pos2sfull (!current_file, start, end_)
let pos2sfull lexbuf = pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf)

let warn_with_pos warn_types (start, end_) err = if Flags.are_warning_types_set warn_types then print_endline_flush (pos2sfull_with start end_ ^ err)
let warn warn_types lexbuf err = warn_with_pos warn_types (pos lexbuf) err
let die lexbuf err = failwith (pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf) ^ err)

let rec concat_bareword_paren accu = function
  | PRINT(s, pos1) :: PAREN(pos2) :: l
  | BAREWORD(s, pos1) :: PAREN(pos2) :: l ->
      concat_bareword_paren (PAREN(pos2) :: BAREWORD_PAREN(s, pos1) :: accu) l
  | RAW_IDENT(kind, ident, pos1) :: PAREN(pos2) :: l -> 
      concat_bareword_paren (PAREN(pos2) :: RAW_IDENT_PAREN(kind, ident, pos1) :: accu) l	
  | PO_COMMENT(_, pos) as e :: l ->
      let l = drop_while (function CR | SPACE _ -> true | _ -> false) l in
      (match l with
      | PO_COMMENT _ :: _
	  (* the check will be done on this PO_COMMENT *)
      | BAREWORD("N", _) :: PAREN(_) :: _
      | BAREWORD("N_", _) :: PAREN(_) :: _ ->
	  concat_bareword_paren (e :: accu) l
      | _ -> 
	  warn_with_pos [Warn_MDK_Common] pos "N(...) must follow the #-PO: comment, with nothing in between" ;
	  concat_bareword_paren accu l)
  | [] -> List.rev accu
  | e :: l -> 
      concat_bareword_paren (e :: accu) l

let rec bracket_bareword_is_hashref accu = function
  | (pos, Parser.BRACKET bracket) :: (_, Parser.BAREWORD _ as bareword) :: (_, Parser.RIGHT_ARROW _ as right_arrow) :: l ->
      bracket_bareword_is_hashref (right_arrow :: bareword :: (pos, Parser.BRACKET_HASHREF bracket) :: accu) l
  | [] -> List.rev accu
  | e :: l -> 
      bracket_bareword_is_hashref (e :: accu) l


let rec raw_token_to_pos_and_token spaces = function
  | INT(s, pos) -> pos, Parser.NUM(new_any M_int s spaces pos)
  | FLOAT(s, pos) -> pos, Parser.NUM(new_any M_float s spaces pos)
  | RAW_STRING(s, pos) -> pos, Parser.RAW_STRING(new_any M_string s spaces pos)
  | RAW_HERE_DOC(r, pos) -> pos, Parser.RAW_HERE_DOC(new_any M_string !r spaces pos)
  | STRING(l, pos) -> pos, Parser.STRING(new_any M_string (raw_interpolated_string_to_tokens l) spaces pos)
  | COMMAND_STRING(l, pos) -> pos, Parser.COMMAND_STRING(new_any (M_mixed [M_string; M_array]) (raw_interpolated_string_to_tokens l) spaces pos)
  | QR_PATTERN(s, opts, pos) -> pos, Parser.QR_PATTERN(new_any M_special (raw_interpolated_string_to_tokens s, opts) spaces pos)
  | PATTERN(s, opts, pos) -> pos, Parser.PATTERN(new_any M_special (raw_interpolated_string_to_tokens s, opts) spaces pos)
  | PATTERN_SUBST(from, to_, opts, pos) -> pos, Parser.PATTERN_SUBST(new_any M_special (raw_interpolated_string_to_tokens from, raw_interpolated_string_to_tokens to_, opts) spaces pos)
  | HERE_DOC(l, pos) -> pos, Parser.HERE_DOC(new_any M_string (raw_interpolated_string_to_tokens (fst !l), snd !l) spaces pos)
  | FORMAT(l, pos) -> pos, Parser.FORMAT(new_any M_string (raw_interpolated_string_to_tokens (fst !l), snd !l) spaces pos)
  | BAREWORD(s, pos) -> pos, Parser.BAREWORD(new_any M_special s spaces pos)
  | BAREWORD_PAREN(s, pos) -> pos, Parser.BAREWORD_PAREN(new_any M_special s spaces pos)
  | REVISION(s, pos) -> pos, Parser.REVISION(new_any M_revision s spaces pos)
  | PERL_CHECKER_COMMENT(s, pos) -> pos, Parser.PERL_CHECKER_COMMENT(new_any M_none s spaces pos)
  | PO_COMMENT(s, pos) -> pos, Parser.PO_COMMENT(new_any M_special s spaces pos)
  | POD(s, pos) -> pos, Parser.POD(new_any M_special s spaces pos)
  | LABEL(s, pos) -> pos, Parser.LABEL(new_any M_none s spaces pos)
  | PRINT(s, pos) -> pos, Parser.PRINT(new_any M_special s spaces pos)
  | PRINT_TO_STAR(s, pos) -> pos, Parser.PRINT_TO_STAR(new_any M_special s spaces pos)
  | PRINT_TO_SCALAR(s, pos) -> pos, Parser.PRINT_TO_SCALAR(new_any M_special s spaces pos)
  | QUOTEWORDS(s, pos) -> pos, Parser.QUOTEWORDS(new_any M_array s spaces pos)
  | COMPACT_HASH_SUBSCRIPT(s, pos) -> pos, Parser.COMPACT_HASH_SUBSCRIPT(new_any M_special s spaces pos)
  | SCALAR_IDENT(kind, name, pos) -> pos, Parser.SCALAR_IDENT(new_any M_special (kind, name) spaces pos)
  | ARRAY_IDENT(kind, name, pos) -> pos, Parser.ARRAY_IDENT(new_any M_special (kind, name) spaces pos)
  | HASH_IDENT(kind, name, pos) -> pos, Parser.HASH_IDENT(new_any M_special (kind, name) spaces pos)
  | FUNC_IDENT(kind, name, pos) -> pos, Parser.FUNC_IDENT(new_any M_special (kind, name) spaces pos)
  | STAR_IDENT(kind, name, pos) -> pos, Parser.STAR_IDENT(new_any M_special (kind, name) spaces pos)
  | RAW_IDENT(kind, name, pos) -> pos, Parser.RAW_IDENT(new_any M_special (kind, name) spaces pos)
  | RAW_IDENT_PAREN(kind, name, pos) -> pos, Parser.RAW_IDENT_PAREN(new_any M_special (kind, name) spaces pos)
  | ARRAYLEN_IDENT(kind, name, pos) -> pos, Parser.ARRAYLEN_IDENT(new_any M_special (kind, name) spaces pos)
  | SUB_WITH_PROTO(proto, pos) -> pos, Parser.SUB_WITH_PROTO(new_any M_special proto spaces pos)
  | FUNC_DECL_WITH_PROTO(fq, name, proto, pos) -> pos, Parser.FUNC_DECL_WITH_PROTO(new_any M_special (fq, name, proto) spaces pos)

  | NEW(pos) -> pos, Parser.NEW(new_any M_special () spaces pos)
  | COMPARE_OP(s, pos) -> pos, Parser.COMPARE_OP(new_any M_special s spaces pos)
  | COMPARE_OP_STR(s, pos) -> pos, Parser.COMPARE_OP_STR(new_any M_special s spaces pos)
  | EQ_OP(s, pos) -> pos, Parser.EQ_OP(new_any M_special s spaces pos)
  | EQ_OP_STR(s, pos) -> pos, Parser.EQ_OP_STR(new_any M_special s spaces pos)
  | ASSIGN(s, pos) -> pos, Parser.ASSIGN(new_any M_special s spaces pos)
  | FOR(s, pos) -> pos, Parser.FOR(new_any M_special s spaces pos)

  | DOTDOT(s, pos) -> pos, Parser.DOTDOT(new_any M_special s spaces pos)
  | MULT(s, pos) -> pos, Parser.MULT(new_any M_special s spaces pos)
  | BIT_SHIFT(s, pos) -> pos, Parser.BIT_SHIFT(new_any M_special s spaces pos)
  | PLUS(s, pos) -> pos, Parser.PLUS(new_any M_special s spaces pos)
  | ONE_SCALAR_PARA(s, pos) -> pos, Parser.ONE_SCALAR_PARA(new_any M_special s spaces pos)
  | MY_OUR(s, pos) -> pos, Parser.MY_OUR(new_any M_special s spaces pos)

  | EOF              (pos) -> pos, Parser.EOF              (new_any M_special () spaces pos)
  | IF               (pos) -> pos, Parser.IF               (new_any M_special () spaces pos)
  | ELSIF            (pos) -> pos, Parser.ELSIF            (new_any M_special () spaces pos)
  | ELSE             (pos) -> pos, Parser.ELSE             (new_any M_special () spaces pos)
  | UNLESS           (pos) -> pos, Parser.UNLESS           (new_any M_special () spaces pos)
  | DO               (pos) -> pos, Parser.DO               (new_any M_special () spaces pos)
  | WHILE            (pos) -> pos, Parser.WHILE            (new_any M_special () spaces pos)
  | UNTIL            (pos) -> pos, Parser.UNTIL            (new_any M_special () spaces pos)
  | CONTINUE         (pos) -> pos, Parser.CONTINUE         (new_any M_special () spaces pos)
  | SUB              (pos) -> pos, Parser.SUB              (new_any M_special () spaces pos)
  | LOCAL            (pos) -> pos, Parser.LOCAL            (new_any M_special () spaces pos)
  | USE              (pos) -> pos, Parser.USE              (new_any M_special () spaces pos)
  | PACKAGE          (pos) -> pos, Parser.PACKAGE          (new_any M_special () spaces pos)
  | BEGIN            (pos) -> pos, Parser.BEGIN            (new_any M_special () spaces pos)
  | END              (pos) -> pos, Parser.END              (new_any M_special () spaces pos)
  | AT               (pos) -> pos, Parser.AT               (new_any M_special () spaces pos)
  | DOLLAR           (pos) -> pos, Parser.DOLLAR           (new_any M_special () spaces pos)
  | PERCENT          (pos) -> pos, Parser.PERCENT          (new_any M_special () spaces pos)
  | AMPERSAND        (pos) -> pos, Parser.AMPERSAND        (new_any M_special () spaces pos)
  | STAR             (pos) -> pos, Parser.STAR             (new_any M_special () spaces pos)
  | ARRAYLEN         (pos) -> pos, Parser.ARRAYLEN         (new_any M_special () spaces pos)
  | SEMI_COLON       (pos) -> pos, Parser.SEMI_COLON       (new_any M_none    () spaces pos)
  | PKG_SCOPE        (pos) -> pos, Parser.PKG_SCOPE        (new_any M_special () spaces pos)
  | PAREN            (pos) -> pos, Parser.PAREN            (new_any M_special () spaces pos)
  | PAREN_END        (pos) -> pos, Parser.PAREN_END        (new_any M_special () spaces pos)
  | BRACKET          (pos) -> pos, Parser.BRACKET          (new_any M_special () spaces pos)
  | BRACKET_END      (pos) -> pos, Parser.BRACKET_END      (new_any M_special () spaces pos)
  | BRACKET_HASHREF  (pos) -> pos, Parser.BRACKET_HASHREF  (new_any M_special () spaces pos)
  | ARRAYREF         (pos) -> pos, Parser.ARRAYREF         (new_any M_special () spaces pos)
  | ARRAYREF_END     (pos) -> pos, Parser.ARRAYREF_END     (new_any M_special () spaces pos)
  | ARROW            (pos) -> pos, Parser.ARROW            (new_any M_special () spaces pos)
  | INCR             (pos) -> pos, Parser.INCR             (new_any M_special () spaces pos)
  | DECR             (pos) -> pos, Parser.DECR             (new_any M_special () spaces pos)
  | POWER            (pos) -> pos, Parser.POWER            (new_any M_special () spaces pos)
  | TIGHT_NOT        (pos) -> pos, Parser.TIGHT_NOT        (new_any M_special () spaces pos)
  | BIT_NEG          (pos) -> pos, Parser.BIT_NEG          (new_any M_special () spaces pos)
  | REF              (pos) -> pos, Parser.REF              (new_any M_special () spaces pos)
  | PATTERN_MATCH    (pos) -> pos, Parser.PATTERN_MATCH    (new_any M_special () spaces pos)
  | PATTERN_MATCH_NOT(pos) -> pos, Parser.PATTERN_MATCH_NOT(new_any M_special () spaces pos)
  | LT               (pos) -> pos, Parser.LT               (new_any M_special () spaces pos)
  | GT               (pos) -> pos, Parser.GT               (new_any M_special () spaces pos)
  | BIT_AND          (pos) -> pos, Parser.BIT_AND          (new_any M_special () spaces pos)
  | BIT_OR           (pos) -> pos, Parser.BIT_OR           (new_any M_special () spaces pos)
  | BIT_XOR          (pos) -> pos, Parser.BIT_XOR          (new_any M_special () spaces pos)
  | AND_TIGHT        (pos) -> pos, Parser.AND_TIGHT        (new_any M_special () spaces pos)
  | OR_TIGHT         (pos) -> pos, Parser.OR_TIGHT         (new_any M_special () spaces pos)
  | QUESTION_MARK    (pos) -> pos, Parser.QUESTION_MARK    (new_any M_special () spaces pos)
  | COLON            (pos) -> pos, Parser.COLON            (new_any M_special () spaces pos)
  | COMMA            (pos) -> pos, Parser.COMMA            (new_any M_special () spaces pos)
  | CONCAT           (pos) -> pos, Parser.CONCAT           (new_any M_special () spaces pos)
  | MULT_L_STR       (pos) -> pos, Parser.MULT_L_STR       (new_any M_special () spaces pos)
  | RIGHT_ARROW      (pos) -> pos, Parser.RIGHT_ARROW      (new_any M_special () spaces pos)
  | NOT              (pos) -> pos, Parser.NOT              (new_any M_special () spaces pos)
  | AND              (pos) -> pos, Parser.AND              (new_any M_special () spaces pos)
  | OR               (pos) -> pos, Parser.OR               (new_any M_special () spaces pos)
  | XOR              (pos) -> pos, Parser.XOR              (new_any M_special () spaces pos)

  | SPACE _ | CR -> internal_error "raw_token_to_token"

and raw_token_to_token spaces raw_token = 
  let _, token = raw_token_to_pos_and_token spaces raw_token in
  token

and raw_interpolated_string_to_tokens l =
  List.map (fun (s, rtok) -> s, concat_spaces [] Space_0 rtok) l

and concat_spaces ret spaces = function
  | CR :: l -> concat_spaces ret Space_cr l
  | SPACE n :: l ->
      let spaces' = 
	match spaces with
	| Space_cr -> Space_cr
	| Space_0 -> if n = 1 then Space_1 else Space_n
	| _ -> Space_n
      in
      concat_spaces ret spaces' l
  | [] -> List.rev ret
  | token :: l -> concat_spaces (raw_token_to_pos_and_token spaces token :: ret) Space_0 l

let rec lexbuf2list accu t lexbuf =
  match t lexbuf with
  | EOF pos -> List.rev (EOF pos :: accu)
  | e -> lexbuf2list (e :: accu) t lexbuf

let get_token token lexbuf = 
  let tokens = lexbuf2list [] token lexbuf in
  let tokens = concat_bareword_paren [] tokens in
  let tokens = concat_spaces [] Space_0 tokens in
  let tokens = bracket_bareword_is_hashref [] tokens in
  tokens

let next_rule = Stack.create()


let putback lexbuf nb = lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - nb

let add_a_new_line raw_pos = 
   incr current_file_current_line ;
   lpush current_file_lines_starts raw_pos

let here_docs = Queue.create()
let raw_here_docs = Queue.create()
let current_here_doc_mark = ref ""

let here_doc_next_line mark =
  let here_doc_ref = ref([], bpos) in
  Queue.push (mark, here_doc_ref) here_docs ;
  here_doc_ref
let raw_here_doc_next_line mark =
  let here_doc_ref = ref("", bpos) in
  Queue.push (mark, here_doc_ref) raw_here_docs ;
  here_doc_ref

let delimit_char = ref '/'
let delimit_char_open = ref '('
let delimit_char_close = ref ')'
type string_escape_kinds = Double_quote | Qq | Delimited | Here_doc
let string_escape_kind = ref Double_quote
let string_quote_escape = ref false
let string_escape_useful = ref (Left false)
let not_ok_for_match = ref (-1)
let string_nestness = ref 0
let string_is_i18n = ref false

let building_current_interpolated_string = Stack.create()
let building_current_string = Stack.create()
let current_string_start_pos = ref 0
let current_string_start_line = ref 0

let die_in_string lexbuf err = failwith (pos2sfull_with !current_string_start_pos (lexeme_end lexbuf) ^ err)
let warn_escape_unneeded lexbuf c = 
  let s = String.make 1 c in warn [Warn_suggest_simpler] lexbuf ("you can replace \\" ^ s ^ " with " ^ s)
let next_interpolated toks =
  let r = Stack.top building_current_string in
  Queue.push (!r, toks) (Stack.top building_current_interpolated_string) ;
  r := ""

let raw_ins t lexbuf = 
  Stack.push (ref "") building_current_string; 
  current_string_start_pos := lexeme_start lexbuf;
  t lexbuf ;
  !(Stack.pop building_current_string), (!current_string_start_pos, lexeme_end lexbuf)

let ins t lexbuf =
  Stack.push (Queue.create()) building_current_interpolated_string ;
  Stack.push (ref "") building_current_string; 
  current_string_start_pos := lexeme_start lexbuf;
  t lexbuf ;
  next_interpolated [] ;
  let _ = Stack.pop building_current_string in
  queue2list (Stack.pop building_current_interpolated_string), (!current_string_start_pos, lexeme_end lexbuf)

let raw_ins_to_string t lexbuf =
  let s, pos = raw_ins t lexbuf in
  not_ok_for_match := lexeme_end lexbuf; 
  RAW_STRING(s, pos)
let ins_to_string t lexbuf =
  string_escape_useful := Left false ;
  string_quote_escape := false ;
  let s, pos = ins t lexbuf in

  if not !string_is_i18n then
    (match !string_escape_useful, s with
    | Right c, [ _, [] ] ->
	let s = String.make 1 c in
	warn_with_pos [Warn_suggest_simpler] pos ("you can replace \"xxx\\" ^ s ^ "xxx\" with 'xxx" ^ s ^ "xxx', that way you don't need to escape <" ^ s ^ ">")
    | _ -> 
	if !string_quote_escape then
	  let full_s = String.concat "" (List.map fst s) in
	  let nb = string_fold_left (fun nb c ->
	    if nb < 0 then nb else
	    if c = '(' then nb + 1 else
	    if c = ')' then nb - 1 else nb
	  ) 0 full_s in
	  if nb = 0 then
	    warn_with_pos [Warn_suggest_simpler] pos "you can replace \"xxx\\\"xxx\" with qq(xxx\"xxx), that way you don't need to escape <\">"
    );

  not_ok_for_match := lexeme_end lexbuf; 
  string_is_i18n := false ;
  STRING(s, pos)

let next_s s t lexbuf =
  let r = Stack.top building_current_string in r := !r ^ s ;
  t lexbuf
let next t lexbuf = next_s (lexeme lexbuf) t lexbuf

let ins_re re_delimited_string lexbuf =
  let s, pos = ins re_delimited_string lexbuf in
  List.iter (fun (s, _) ->
    if str_contains s "[^\\s]" then warn [Warn_suggest_simpler] lexbuf "you can replace [^\\s] with \\S";
    if str_contains s "[^\\w]" then warn [Warn_suggest_simpler] lexbuf "you can replace [^\\w] with \\W"
  ) s ;
  s, pos

let string_interpolate token pre lexbuf =
   let s = lexeme lexbuf in
   let local_lexbuf = Lexing.from_string (pre ^ s ^ " ") in (* add a space to help tokenizing "xxx$$" *)
   local_lexbuf.lex_start_p <- lexbuf.lex_start_p ;
   local_lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
   local_lexbuf.lex_abs_pos <- lexeme_start lexbuf ;
   let l = lexbuf2list [] token local_lexbuf in
   let l = concat_bareword_paren [] l in
   next_interpolated l; 
   (Stack.pop next_rule) lexbuf

let ident_type_from_char fq name lexbuf c = 
  not_ok_for_match := lexeme_end lexbuf; 
  match c with
  | '$' -> SCALAR_IDENT(fq, name, pos lexbuf)
  | '@' -> ARRAY_IDENT (fq, name, pos lexbuf)
  | '%' -> HASH_IDENT  (fq, name, pos lexbuf)
  | '&' -> FUNC_IDENT  (fq, name, pos lexbuf)
  | '*' -> STAR_IDENT  (fq, name, pos lexbuf)
  | _ -> internal_error "ident_type_from_char"

let split_at_two_colons s =
  let i_fq = String.rindex s ':' in
  String.sub s 0 (i_fq - 1), skip_n_char (i_fq + 1) s

let ident_from_lexbuf lexbuf = 
  let fq, name = split_at_two_colons (lexeme lexbuf) in
  RAW_IDENT(Some fq, name, pos lexbuf)
  
let typed_ident_from_lexbuf lexbuf = 
  let s = lexeme lexbuf in
  ident_type_from_char None (skip_n_char 1 s) lexbuf s.[0]

let typed_fqident_from_lexbuf lexbuf = 
  let s = lexeme lexbuf in
  let fq, name = split_at_two_colons (skip_n_char 1 s) in
  ident_type_from_char (Some fq) name lexbuf s.[0]
  
let arraylen_ident_from_lexbuf lexbuf = 
  not_ok_for_match := lexeme_end lexbuf; 
  let s = lexeme lexbuf in
  ARRAYLEN_IDENT(None, skip_n_char 2 s, pos lexbuf)

let arraylen_fqident_from_lexbuf lexbuf = 
  let s = lexeme lexbuf in
  let fq, name = split_at_two_colons (skip_n_char 2 s) in
  ARRAYLEN_IDENT(Some fq, name, pos lexbuf)

let check_multi_line_delimited_string opts (start, end_) =
   let check =
     match opts with
     | None -> true
     | Some s -> not (String.contains s 'x') in
   if check then
     if !current_file_current_line <> !current_string_start_line then
     failwith (pos2sfull_with start end_ ^ "multi-line patterns are not allowed (or use /x modifier)")

let hex_in_string lexbuf next_rule s =
  let i = 
    try int_of_string ("0x" ^ s)
    with Failure("int_of_string") -> die_in_string lexbuf ("Bad_hex_in_string \"" ^ lexeme lexbuf ^ "\"")
  in
  let s =
    if i < 256 then
      String.make 1 (Char.chr i)
    else
      "\\x{" ^ s ^ "}" in
  next_s s (Stack.pop next_rule) lexbuf 

let set_delimit_char lexbuf op = 
  let c = lexeme_char lexbuf (String.length op) in
  delimit_char := c;
  match c with
  | '@' -> warn [Warn_complex_expressions] lexbuf ("don't use " ^ op ^ "@...@, replace @ with / ! , or |")
  | ':' -> warn [Warn_complex_expressions] lexbuf ("don't use " ^ op ^ ":...:, replace : with / ! , or |")
  | _ -> ()

let set_delimit_char_open lexbuf op =
  let char_open = lexeme_char lexbuf (String.length op) in
  let char_close =
    match char_open with
    | '(' -> ')'
    | '{' -> '}'
    | _ -> internal_error "set_delimit_char_open"
  in
  if op = "qx" then 
    warn [Warn_complex_expressions] lexbuf (Printf.sprintf "don't use qx%c...%c, use `...` instead" char_open char_close)
  else if char_open = '{' then
    warn [Warn_complex_expressions] lexbuf ("don't use " ^ op ^ "{...}, use " ^ op ^ "(...) instead");
  delimit_char_open := char_open;
  delimit_char_close := char_close
}

let stash = [ '$' '@' '%' '&' '*' ]
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident = ident_start ['0'-'9' 'A'-'Z' 'a'-'z' '_'] *
let pattern_separator = [ '/' '!' ',' '|' '@' ':' ] 
let pattern_open = [ '(' '{' ]
let pattern_close = [ ')' '}' ]

let in_string_expr = (ident | (ident? ("::" ident)+)) "->"? (('{' [^ '{' '}' '\n']* '}') | ('[' [^ '[' ']' '\n']* ']'))*

rule token = parse
| [' ' '\t']+ { 
    (* propagate not_ok_for_match when it was set by the previous token *)
    if lexeme_start lexbuf = !not_ok_for_match then not_ok_for_match := lexeme_end lexbuf; 
    SPACE(lexeme_end lexbuf - lexeme_start lexbuf)
  }
| "# perl_checker: " [^ '\n']* { PERL_CHECKER_COMMENT(skip_n_char 16 (lexeme lexbuf), pos lexbuf) }
| "#-PO: " [^ '\n']* { PO_COMMENT(skip_n_char 1 (lexeme lexbuf), pos lexbuf) }
| '#' [^ '\n']* { SPACE(1) }

| "\n=" { 
    add_a_new_line(lexeme_end lexbuf - 1);
    let _ = ins pod_command lexbuf in token lexbuf 
  }

| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    (try
      let (mark, r) = Queue.pop here_docs in
      current_here_doc_mark := mark ;
      r := ins here_doc lexbuf
    with Queue.Empty -> 
     try
      let (mark, r) = Queue.pop raw_here_docs in
      current_here_doc_mark := mark ;
      r := raw_ins raw_here_doc lexbuf
    with Queue.Empty -> ());
    CR
  }
| "->" { ARROW(pos lexbuf) }
| "++" { INCR(pos lexbuf) }
| "--" { DECR(pos lexbuf) }
| "**" { POWER(pos lexbuf) }
| "!" { TIGHT_NOT(pos lexbuf) }
| "~" { BIT_NEG(pos lexbuf) }
| "=~" { PATTERN_MATCH(pos lexbuf) }
| "!~" { PATTERN_MATCH_NOT(pos lexbuf) }
| "*" { MULT(lexeme lexbuf, pos lexbuf) }
| "%" { MULT(lexeme lexbuf, pos lexbuf) }
| "x" { MULT_L_STR(pos lexbuf) }
| "+" { PLUS(lexeme lexbuf, pos lexbuf) }
| "-" { PLUS(lexeme lexbuf, pos lexbuf) }
| "." { CONCAT(pos lexbuf) }
| "<<" { BIT_SHIFT(lexeme lexbuf, pos lexbuf) }
| ">>" { BIT_SHIFT(lexeme lexbuf, pos lexbuf) }
| "<" { LT(pos lexbuf) }
| ">" { GT(pos lexbuf) }
| "<=" | ">="  { COMPARE_OP(lexeme lexbuf, pos lexbuf) }
| "lt" | "gt" | "le" | "ge" { COMPARE_OP_STR(lexeme lexbuf, pos lexbuf) }
| "==" | "!=" | "<=>" { EQ_OP(lexeme lexbuf, pos lexbuf) }
| "eq" | "ne" | "cmp" { EQ_OP_STR(lexeme lexbuf, pos lexbuf) }
| "&" { BIT_AND(pos lexbuf) }
| "|" { BIT_OR(pos lexbuf) }
| "^" { BIT_XOR(pos lexbuf) }
| "&&" { AND_TIGHT(pos lexbuf) }
| "||" { OR_TIGHT(pos lexbuf) }
| ".." { DOTDOT(lexeme lexbuf, pos lexbuf) }
| "..." { DOTDOT(lexeme lexbuf, pos lexbuf) }
| "?" { QUESTION_MARK(pos lexbuf) }
| ":" { COLON(pos lexbuf) }
| "::" { PKG_SCOPE(pos lexbuf) }

| "=" | "+=" | "-=" | "*=" | ".=" | "|=" | "&=" | "^=" | "||=" | "&&=" { ASSIGN(lexeme lexbuf, pos lexbuf) }

| "<<=" | ">>=" | "**=" { 
    warn [Warn_complex_expressions] lexbuf (Printf.sprintf "don't use \"%s\", use the expanded version instead" (lexeme lexbuf)) ;
    ASSIGN(lexeme lexbuf, pos lexbuf) 
  }

| "," { COMMA(pos lexbuf) }
| "=>" { RIGHT_ARROW(pos lexbuf) }
| "not" { NOT(pos lexbuf) }
| "and" { AND(pos lexbuf) }
| "or" { OR(pos lexbuf) }
| "xor" { XOR(pos lexbuf) }

| "if"       { IF(pos lexbuf) }
| "else"     { ELSE(pos lexbuf) }
| "elsif"    { ELSIF(pos lexbuf) }
| "unless"   { UNLESS(pos lexbuf) }
| "do"       { DO(pos lexbuf) }
| "while"    { WHILE(pos lexbuf) }
| "until"    { UNTIL(pos lexbuf) }
| "foreach"  { FOR(lexeme lexbuf, pos lexbuf) }
| "for"      { FOR(lexeme lexbuf, pos lexbuf) }
| "my"       { MY_OUR(lexeme lexbuf, pos lexbuf) }
| "our"      { MY_OUR(lexeme lexbuf, pos lexbuf) }
| "local"    { LOCAL(pos lexbuf) }
| "continue" { CONTINUE(pos lexbuf) }
| "sub"      { SUB(pos lexbuf) }
| "package"  { PACKAGE(pos lexbuf) }
| "use"      { USE(pos lexbuf) }
| "BEGIN"    { BEGIN(pos lexbuf) }
| "END"      { END(pos lexbuf) }
| "print"    { PRINT(lexeme lexbuf, pos lexbuf) }
| "printf"   { PRINT(lexeme lexbuf, pos lexbuf) }
| "new"      { NEW(pos lexbuf) }
| "format"   { let pos = pos lexbuf in FORMAT(here_doc_next_line ".", pos) }
| "delete"
| "defined"
| "length" 
| "keys" 
| "exists" 
| "shift"
| "eval"
| "ref"      { ONE_SCALAR_PARA(lexeme lexbuf, pos lexbuf) }

| "split"
| "grep"  { (* ok_for_match! *) BAREWORD(lexeme lexbuf, pos lexbuf) }

| "print " ['A'-'Z'] ['A'-'Z' '0'-'9']* ['\n' ' '] { 
    putback lexbuf 1;
    PRINT_TO_STAR(("print", skip_n_char 6 (lexeme lexbuf)), pos lexbuf)
  }
| "print $" ident ['\n' ' '] { 
    putback lexbuf 1; 
    PRINT_TO_SCALAR(("print", skip_n_char 7 (lexeme lexbuf)), pos lexbuf);
  }
| "printf " ['A'-'Z'] ['A'-'Z' '0'-'9']* ['\n' ' '] { 
    putback lexbuf 1;
    PRINT_TO_STAR(("printf", skip_n_char 7 (lexeme lexbuf)), pos lexbuf)
  }
| "printf $" ident ['\n' ' '] { 
    putback lexbuf 1; 
    PRINT_TO_SCALAR(("printf", skip_n_char 8 (lexeme lexbuf)), pos lexbuf);
  }

| ident ' '* "=>" { (* needed so that (if => 1) works *)
    let s = lexeme lexbuf in
    let end_ = String.length s - 1 in
    let ident_end = non_rindex_from s (end_ - 2) ' ' in
    putback lexbuf (end_ - ident_end); 
    BAREWORD(String.sub s 0 (ident_end+1), pos lexbuf)
  }

| "{" ident "}" { (* needed so that $h{if} works *)
    not_ok_for_match := lexeme_end lexbuf;
    COMPACT_HASH_SUBSCRIPT(skip_n_char_ 1 1 (lexeme lexbuf), pos lexbuf)
  }

| '@' { AT(pos lexbuf) }
| '$' { DOLLAR(pos lexbuf) }
| '$' '#' { ARRAYLEN(pos lexbuf) }
| '%' ['$' '{'] { putback lexbuf 1; PERCENT(pos lexbuf) }
| '&' ['$' '{'] { putback lexbuf 1; AMPERSAND(pos lexbuf) }
| '*' ['$' '{'] { putback lexbuf 1; if lexeme_start lexbuf = !not_ok_for_match then MULT("*", pos lexbuf) else STAR(pos lexbuf) }


| ';' { SEMI_COLON(pos lexbuf) }
| '(' { PAREN(pos lexbuf) }
| '{' { BRACKET(pos lexbuf) }
| "+{"{ BRACKET_HASHREF(pos lexbuf) }
| '[' { ARRAYREF(pos lexbuf) }
| ')' { not_ok_for_match := lexeme_end lexbuf; PAREN_END(pos lexbuf) }
| '}' { not_ok_for_match := lexeme_end lexbuf; BRACKET_END(pos lexbuf) }
| ']' { not_ok_for_match := lexeme_end lexbuf; ARRAYREF_END(pos lexbuf) }

| "/" {
    if lexeme_start lexbuf = !not_ok_for_match then MULT("/", pos lexbuf)
    else (
      delimit_char := '/' ;
      current_string_start_line := !current_file_current_line;
      let s, pos = ins_re re_delimited_string lexbuf in
      let opts, _ = raw_ins pattern_options lexbuf in
      check_multi_line_delimited_string (Some opts) pos ;
      PATTERN(s, opts, pos)
    ) 
  }

| "/=" {
    if lexeme_start lexbuf = !not_ok_for_match then ASSIGN(lexeme lexbuf, pos lexbuf)
    else (
      putback lexbuf 1 ;
      delimit_char := '/' ;
      let s, pos = ins_re re_delimited_string lexbuf in
      let opts, _ = raw_ins pattern_options lexbuf in
      PATTERN(s, opts, pos)
    ) 
  }

| "m" pattern_separator {
  set_delimit_char lexbuf "m" ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins_re re_delimited_string lexbuf in
  let opts, _ = raw_ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN(s, opts, pos)
}

| "qr" pattern_separator {
  set_delimit_char lexbuf "qr" ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins_re re_delimited_string lexbuf in
  let opts, _ = raw_ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  QR_PATTERN(s, opts, pos)
}

| "qw" pattern_separator {
  set_delimit_char lexbuf "qw" ;
  current_string_start_line := !current_file_current_line;
  let s, pos = raw_ins delimited_string lexbuf in
  warn_with_pos [Warn_complex_expressions] pos (Printf.sprintf "don't use qw%c...%c, use qw(...) instead" !delimit_char !delimit_char) ;
  QUOTEWORDS(s, pos)
}

| "s" pattern_separator {
  set_delimit_char lexbuf "s" ;
  current_string_start_line := !current_file_current_line;
  let s1, (start, _) = ins_re re_delimited_string lexbuf in 
  let s2, (_, end_)  = ins delimited_string lexbuf in 
  let opts, _ = raw_ins pattern_options lexbuf in
  let pos = start, end_ in
  if String.contains opts 'e' && sum (List.map (fun (s, _) -> count_chars_in_string s '"') s2) > 2 then
    die lexbuf ("do not write so complicated things in the eval part of s///,\n" ^
		"i generate wrong warnings for things like s/xxx/die \"yyy \\\"zzz\\\" \"/") ;
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}

| "tr" pattern_separator {
  set_delimit_char lexbuf "tr" ;
  current_string_start_line := !current_file_current_line;
  let s1, (start, _) = ins delimited_string lexbuf in 
  let s2, (_, end_)  = ins delimited_string lexbuf in 
  let opts, _ = raw_ins pattern_options lexbuf in
  let pos = start, end_ in
  check_multi_line_delimited_string None pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}
    
| "<<" ident { 
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char 2 (lexeme lexbuf)), pos lexbuf)
  }
| "<<\"" ident "\"" { 
    warn_with_pos [Warn_suggest_simpler] (lexeme_start lexbuf + 2, lexeme_end lexbuf) "Don't use <<\"MARK\", use <<MARK instead" ;
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char_ 3 1 (lexeme lexbuf)), pos lexbuf)
  }
| "<<'" ident "'" { 
    not_ok_for_match := lexeme_end lexbuf; 
    RAW_HERE_DOC(raw_here_doc_next_line (skip_n_char_ 3 1 (lexeme lexbuf)), pos lexbuf)
  }
| "<<" ' '+ "'"
| "<<" ' '+ ident
| "<<" ' '* '"' { 
    failwith (pos2sfull_with (lexeme_start lexbuf + 2) (lexeme_end lexbuf) ^ "No space allowed between \"<<\" and the marker")
  }

| "\\"+ stash
| "\\" ['0'-'9' 'A'-'Z' 'a'-'z']
| "\\" ' '* '('
    { lexbuf.lex_curr_pos <- lexbuf.lex_start_pos + 1; REF(pos lexbuf) }

| "sub(" [ '$' '@' '\\' '&' ';' '%' ]* ')' {
    SUB_WITH_PROTO(skip_n_char_ 4 1 (lexeme lexbuf), pos lexbuf)
  }

| "sub" ' '+ ident ' '* '(' [ '$' '@' '\\' '&' ';' '%' ]* ')' {
    (* bloody prototypes, must be caught especially otherwise "($)" is badly tokenized *)
    (* and alas "($@)" is both valid as an expression and a prototype *)
    let s = lexeme lexbuf in
    let ident_start = non_index_from s 3 ' ' in

    let proto_start = String.index_from s ident_start '(' in
    let ident_end = non_rindex_from s (proto_start-1) ' ' in
    let ident = String.sub s ident_start (ident_end - ident_start + 1) in
    let prototype = skip_n_char_ (proto_start + 1) 1 s in

    FUNC_DECL_WITH_PROTO(None, ident, prototype, pos lexbuf)
  }

| "sub" ' '+ ident ("::" ident)+ ' '* '(' [ '$' '@' '\\' '&' ';' '%' ]* ')' {
    (* bloody prototypes, must be caught especially otherwise "($)" is badly tokenized *)
    (* and alas "($@)" is both valid as an expression and a prototype *)
    let s = lexeme lexbuf in
    let ident_start = non_index_from s 3 ' ' in

    let proto_start = String.index_from s ident_start '(' in
    let ident_end = non_rindex_from s (proto_start-1) ' ' in
    let ident = String.sub s ident_start (ident_end - ident_start + 1) in
    let prototype = skip_n_char_ (proto_start + 1) 1 s in

    let fq, name = split_at_two_colons ident in
    FUNC_DECL_WITH_PROTO(Some fq, name, prototype, pos lexbuf)
  }

| "$#" ident? ("::" ident)+ { arraylen_fqident_from_lexbuf lexbuf }
| "$#" ident { arraylen_ident_from_lexbuf lexbuf }

| stash ident? ("::" ident)+ { typed_fqident_from_lexbuf lexbuf }
| stash ident
| '$' [^ '{' ' ' '\n' '$']
| "$^" [^ '{' ' ' '\n'] { typed_ident_from_lexbuf lexbuf }

| "$$" [^ 'a'-'z' 'A'-'Z' '_' '{'] { putback lexbuf 1; SCALAR_IDENT(None, "$", pos lexbuf) }

| stash "::" { putback lexbuf 2; ident_type_from_char None "main" lexbuf (lexeme_char lexbuf 0) }

| ident? ("::" ident)+ { ident_from_lexbuf lexbuf }
| ident { not_ok_for_match := lexeme_end lexbuf; 
	  let word = lexeme lexbuf in
	  if word = "qx" then die lexbuf "don't use qx{...}, use `...` instead" else
	  BAREWORD(word, pos lexbuf) }

| ident ":" { LABEL(lexeme lexbuf, pos lexbuf) }

| '-' [ 'a'-'z' 'A'-'Z' ] [ ' ' '(' ';' ] { putback lexbuf 1; ONE_SCALAR_PARA(lexeme lexbuf, pos lexbuf) }

| ['0'-'9'] ['0'-'9' '_']* '.' ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9'] ['0'-'9' '_']*)+ 
| 'v' ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9'] ['0'-'9' '_']*)* 
  { 
    not_ok_for_match := lexeme_end lexbuf; 
    REVISION(lexeme lexbuf, pos lexbuf)
  }

| ['0'-'9']* '.' ['0'-'9']+ (['e' 'E']['-' '+']?['0'-'9']+)? { 
    not_ok_for_match := lexeme_end lexbuf; 
    FLOAT(lexeme lexbuf, pos lexbuf)
  }
| ['0'-'9'] ['0'-'9' '_']*  (['e' 'E']['-' '+']?['0'-'9']+)?
| "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ { 
    not_ok_for_match := lexeme_end lexbuf; 
    INT(lexeme lexbuf, pos lexbuf)
  }

| 'N' '_'? "(\"" { string_is_i18n := true ; putback lexbuf 2 ; BAREWORD(lexeme lexbuf, pos lexbuf) }

| '"'   { ins_to_string string lexbuf }
| "'"   { raw_ins_to_string rawstring lexbuf }
| '`'   { delimit_char := '`'; 
	  current_string_start_line := !current_file_current_line;
	  not_ok_for_match := lexeme_end lexbuf; 
	  let s, pos = ins delimited_string lexbuf in
	  check_multi_line_delimited_string None pos ;
	  COMMAND_STRING(s, pos) }
| "q" pattern_open { set_delimit_char_open lexbuf "q"; raw_ins_to_string qstring lexbuf }
| "qq" pattern_open { set_delimit_char_open lexbuf "qq"; ins_to_string qqstring lexbuf }
| "qx" pattern_open { set_delimit_char_open lexbuf "qx"; ins_to_string qqstring lexbuf }
| "qw" pattern_open { set_delimit_char_open lexbuf "qw"; let s, pos = raw_ins qstring lexbuf in QUOTEWORDS(s, pos) }

| "\n__END__" [^ '0'-'9' 'A'-'Z' 'a'-'z' '_'] 
| "\n__DATA__" [^ '0'-'9' 'A'-'Z' 'a'-'z' '_']
| eof   { EOF(pos lexbuf) }
| _ { failwith (Printf.sprintf "%serror tokenizing <<%s>>" (pos2sfull lexbuf) (lexeme lexbuf)) }

and string = parse
| '"' { () }
| '\\' { Stack.push string next_rule ; string_escape_kind := Double_quote; string_escape lexbuf }
| '$'  { Stack.push string next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push string next_rule ; string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next string lexbuf
  }
| "'" { string_escape_useful := Left true ; next string lexbuf }
| [^ '\n' '\\' '"' '$' '@']+ { next string lexbuf }
| eof { die_in_string lexbuf "Unterminated_string" }

and delimited_string = parse
| '\\' { Stack.push delimited_string next_rule ; string_escape_kind := Delimited; string_escape lexbuf }
| '$'  { Stack.push delimited_string next_rule ; delimited_string_interpolate_scalar lexbuf }
| '@'  { Stack.push delimited_string next_rule ; delimited_string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next delimited_string lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_delimited_string" }
| [ ^ '\n' '\\' '$' '@'] { if lexeme_char lexbuf 0 <> !delimit_char then next delimited_string lexbuf }

and re_delimited_string = parse
| '\\' { Stack.push re_delimited_string next_rule ; re_string_escape lexbuf }
| '$'  { Stack.push re_delimited_string next_rule ; delimited_string_interpolate_scalar lexbuf }
| '@'  { if lexeme_char lexbuf 0 <> !delimit_char then 
        (Stack.push re_delimited_string next_rule ; delimited_string_interpolate_array lexbuf) }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next re_delimited_string lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_delimited_string" }
| [ ^ '\n' '\\' '$' '@'] { if lexeme_char lexbuf 0 <> !delimit_char then next re_delimited_string lexbuf }

and rawstring = parse
| ''' { () }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next rawstring lexbuf
  }
| '\\' { next rawstring lexbuf }
| "\\'" { next_s "'" rawstring lexbuf }
| [^ '\n' ''' '\\']+ { next rawstring lexbuf }
| eof { die_in_string lexbuf "Unterminated_rawstring" }

and qqstring = parse
| pattern_close { 
    if lexeme_char lexbuf 0 = !delimit_char_close then
      if !string_nestness <> 0 then (decr string_nestness; next qqstring lexbuf)
      else ()
    else next qstring lexbuf
  }
| pattern_open {
    if lexeme_char lexbuf 0 = !delimit_char_open then incr string_nestness;
    next qqstring lexbuf
  }
| '\\' { Stack.push qqstring next_rule ; string_escape_kind := Qq; string_escape lexbuf }
| '$'  { Stack.push qqstring next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push qqstring next_rule ; string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next qqstring lexbuf
  }
| [^ '\n' '(' ')' '{' '}' '\\' '$' '@']+ { next qqstring lexbuf }
| eof { die_in_string lexbuf "Unterminated_qqstring" }

and qstring = parse
| pattern_close { 
    if lexeme_char lexbuf 0 = !delimit_char_close then
      if !string_nestness <> 0 then (decr string_nestness ; next qstring lexbuf) 
      else ()
    else next qstring lexbuf
  }
| pattern_open {
    if lexeme_char lexbuf 0 = !delimit_char_open then incr string_nestness;
    next qstring lexbuf
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next qstring lexbuf
  }
| [^ '\n' '(' ')' '{' '}']+ { next qstring lexbuf }
| eof { die_in_string lexbuf "Unterminated_qstring" }

and here_doc = parse
| '\\' { Stack.push here_doc next_rule ; string_escape_kind := Here_doc; string_escape lexbuf }
| '$'  { Stack.push here_doc next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push here_doc next_rule ; string_interpolate_array lexbuf }
| [ ^ '\n' '\\' '$' '@' ]* {
    let s = lexeme lexbuf in
    if chomps s <> !current_here_doc_mark
    then next_s s here_doc lexbuf 
    else if s <> !current_here_doc_mark then warn_with_pos [Warn_traps] (pos lexbuf) "Trailing spaces after HERE-document mark"
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next here_doc lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_here_doc" }

and raw_here_doc = parse
| [ ^ '\n' ]* {
    let s = lexeme lexbuf in
    if chomps s <> !current_here_doc_mark
    then next_s s raw_here_doc lexbuf 
    else if s <> !current_here_doc_mark then warn_with_pos [Warn_traps] (pos lexbuf) "Trailing spaces after HERE-document mark"
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next raw_here_doc lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_raw_here_doc" }


and string_escape = parse
| ['0'-'9']         { string_escape_useful := Left true; next_s (String.make 1 (Char.chr (int_of_string (lexeme lexbuf)))) (Stack.pop next_rule) lexbuf }
| 'n'               { string_escape_useful := Left true; next_s "\n" (Stack.pop next_rule) lexbuf }
| 't'               { string_escape_useful := Left true; next_s "\t" (Stack.pop next_rule) lexbuf }
| "x{" [^ '}']* '}' { string_escape_useful := Left true; hex_in_string lexbuf next_rule (skip_n_char_ 2 1 (lexeme lexbuf)) }
| 'x' [^ '{'] _     { string_escape_useful := Left true; hex_in_string lexbuf next_rule (skip_n_char 1 (lexeme lexbuf)) }
| '\n' { die lexbuf "do not use \"\\\" before end-of-line, it's useless and generally bad" }
| '\\'{ next_s "\\" (Stack.pop next_rule) lexbuf }
| ['b' 'f' 'a' 'r'] { string_escape_useful := Left true; next_s ("\\" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }
| ['$' '@' '%' '{' '[' ':'] { 
	if !string_escape_useful = Left false then string_escape_useful := Right (lexeme_char lexbuf 0) ;
	next_s (lexeme lexbuf) (Stack.pop next_rule) lexbuf 
  }
| _   { 
    let c = lexeme_char lexbuf 0 in
    (match !string_escape_kind with
    | Double_quote -> 
	if c <> '"' then
	  warn_escape_unneeded lexbuf c
	else (
	  if !string_escape_useful = Left false then string_escape_useful := Right c ;
	  string_quote_escape := true
        )
    | Qq -> if c <> !delimit_char_open && c <> !delimit_char_close then warn_escape_unneeded lexbuf c
    | Here_doc -> warn_escape_unneeded lexbuf c
    | Delimited -> if c = !delimit_char then 
          warn [Warn_suggest_simpler] lexbuf ("change the delimit character " ^ String.make 1 !delimit_char ^ " to get rid of this escape")
        else warn_escape_unneeded lexbuf c);
    let s = if c = '"' then String.make 1 c else "\\" ^ String.make 1 c in
    next_s s (Stack.pop next_rule) lexbuf 
  }

and re_string_escape = parse
| ['0'-'9'] { next_s (String.make 1 (Char.chr (int_of_string (lexeme lexbuf)))) (Stack.pop next_rule) lexbuf }
| '\\'{ next_s "\\" (Stack.pop next_rule) lexbuf }
| 'n' { next_s "\n" (Stack.pop next_rule) lexbuf }
| 't' { next_s "\t" (Stack.pop next_rule) lexbuf }
| "x{" [^ '}']* '}' { hex_in_string lexbuf next_rule (skip_n_char_ 2 1 (lexeme lexbuf)) }
| 'x' [^ '{'] _ { hex_in_string lexbuf next_rule (skip_n_char 1 (lexeme lexbuf)) }
| '\n' { die lexbuf "do not use \"\\\" before end-of-line, it's useless and generally bad" }
| ['r' 'b' 'f' '$' '@' '%' 's' 'S' 'd' 'D' 'w' 'W' 'Q' 'E' 'b' 'Z' 'z' '^' '.' '*' '+' '?' '[' ']' '(' ')' '|' '{' '}' '-' ':'] { 
     next_s ("\\" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf 
  }
| _  {
     let c = lexeme_char lexbuf 0 in 
     if c = !delimit_char then 
       warn [Warn_suggest_simpler] lexbuf ("change the delimit character " ^ String.make 1 !delimit_char ^ " to get rid of this escape")
     else warn_escape_unneeded lexbuf c ;
     next_s ("\\" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf 
  }

and string_interpolate_scalar = parse
| '$' ident
| ['0'-'9']
| '{' [^ '{' '}']* '}'
| in_string_expr
| [^ '{' '}' ' ' '\n' '"'] { (* eg: $! $$ *)
      string_interpolate token "$" lexbuf
  }

| "{"
| ident "->"? '{'
| '"' { putback lexbuf 1; next_s "$" (Stack.pop next_rule) lexbuf }
| eof {                   next_s "$" (Stack.pop next_rule) lexbuf }
| _ { warn [Warn_strange] lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); next_s ("$" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }

and delimited_string_interpolate_scalar = parse (* needed for delimited string like m!foo$! where $! should not be taken as is... *)
| '$' ident
| ['0'-'9']
| '{' [^ '{' '}']* '}'
| (ident | (ident? ("::" ident)+)) "->"? ('{' [^ '{' '}' '\n']* '}')*
| (ident | (ident? ("::" ident)+)) "->"? (('{' [^ '{' '}' '\n']* '}') | ('[' ('$' ident | ['0'-'9']+) ']'))*
  {
      string_interpolate token "$" lexbuf
  }

| (ident | (ident? ("::" ident)+)) "->"? (('{' [^ '{' '}' '\n']* '}') | ('[' ['$' '0'-'9'] [^ '[' ']' '\n']* ']'))*
  {
   die lexbuf (Printf.sprintf "I really can't handle this, [xxx] can be indexing or not based on stellar position :-(")
  }

| "{"
| ident "->"? '{'
| eof { next_s "$" (Stack.pop next_rule) lexbuf }
| _ { 
    let c = lexeme_char lexbuf 0 in
    if c <> !delimit_char && c <> '|' && c<>')' && c<>'/' && c<>' ' then warn [Warn_strange] lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); 
    putback lexbuf 1;
    next_s "$" (Stack.pop next_rule) lexbuf
  }

and string_interpolate_array = parse
| '$' ident
| '{' [^ '{' '}']* '}'
| in_string_expr { string_interpolate token "@" lexbuf }

| [ '@' '*' '<' '>' ']' '.' '(' ' ' ] { next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }
| '"' { putback lexbuf 1; next_s "@" (Stack.pop next_rule) lexbuf }
| eof {                   next_s "@" (Stack.pop next_rule) lexbuf }
| _ { warn [Warn_strange] lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }

and delimited_string_interpolate_array = parse
| '$' ident
| '{' [^ '{' '}']* '}'
| in_string_expr
    { string_interpolate token "@" lexbuf }

| [ '@' '*' '<' '>' ']' '.' '(' ' ' ] { next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }
| eof { next_s "@" (Stack.pop next_rule) lexbuf }
| _ { 
    let c = lexeme_char lexbuf 0 in
    if c <> !delimit_char then warn [Warn_strange] lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf));
    putback lexbuf 1;
    next_s "@" (Stack.pop next_rule) lexbuf
  }

and pattern_options = parse
| [ 'g' 'i' 'm' 'o' 's' 'x' 'e' 'd' ] { next pattern_options lexbuf }
| _ { putback lexbuf 1; () }

and pod_command = parse
| [^ '\n' ]+ {
  let s = lexeme lexbuf in
  let command = String.sub s 0 (try String.index s ' ' with Not_found -> String.length s) in
  match command with
  | "cut" ->
    if !(Stack.top building_current_string) = "" then
      failwith(pos2sfull lexbuf ^ "found POD command \"=cut\" but it is not a POD block")
  | "head1" | "head2" | "head3" | "head4" | "over" | "item" | "back" | "pod" | "begin" | "end" | "for" ->
      next pod lexbuf
  | s -> failwith(pos2sfull lexbuf ^ "unknown POD command \"" ^ s ^ "\"")
 }
| _ { failwith(pos2sfull lexbuf ^ "POD command expected") }

and pod = parse
| "\n=" { 
    add_a_new_line(lexeme_end lexbuf - 1);
    next pod_command lexbuf 
  }
| "\n" [^ '=' '\n'] [^ '\n']*
| "\n" { 
    add_a_new_line(lexeme_end lexbuf);
    next pod lexbuf 
  }
| eof
| _ { failwith(pos2sfull_with !current_string_start_pos (lexeme_end lexbuf) ^ "POD block still open") }
