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
  | NUM of (string * raw_pos)
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
  | SCALAR_IDENT of (string option * string * raw_pos)
  | ARRAY_IDENT of (string option * string * raw_pos)
  | HASH_IDENT of (string option * string * raw_pos)
  | FUNC_IDENT of (string option * string * raw_pos)
  | STAR_IDENT of (string option * string * raw_pos)
  | RAW_IDENT of (string option * string * raw_pos)
  | RAW_IDENT_PAREN of (string option * string * raw_pos)
  | ARRAYLEN_IDENT of (string option * string * raw_pos)
  | FUNC_DECL_WITH_PROTO of (string * string * raw_pos)

  | IF of raw_pos | ELSIF of raw_pos | ELSE of raw_pos | UNLESS of raw_pos | DO of raw_pos | WHILE of raw_pos | UNTIL of raw_pos | MY_OUR of (string * raw_pos) | CONTINUE of raw_pos | SUB of raw_pos
  | LOCAL of raw_pos | FOR of (string * raw_pos) | USE of raw_pos | PACKAGE of raw_pos | BEGIN of raw_pos | END of raw_pos | PRINT of (string * raw_pos) 
  | NEW of (raw_pos) | FORMAT of (raw_pos) | AT of raw_pos | DOLLAR of raw_pos | PERCENT of raw_pos | AMPERSAND of raw_pos
  | STAR of raw_pos | ARRAYLEN of raw_pos | SEMI_COLON of raw_pos | PKG_SCOPE of raw_pos | PAREN of raw_pos | PAREN_END of raw_pos | BRACKET of raw_pos
  | BRACKET_END of raw_pos | BRACKET_HASHREF of raw_pos | ARRAYREF of raw_pos | ARRAYREF_END of raw_pos | ARROW of raw_pos | INCR of raw_pos | DECR of raw_pos
  | POWER of raw_pos | TIGHT_NOT of raw_pos | BIT_NEG of raw_pos | REF of raw_pos | ONE_SCALAR_PARA of (string * raw_pos) | PATTERN_MATCH of raw_pos | PATTERN_MATCH_NOT of raw_pos | MULT of (string * raw_pos)
  | PLUS of (string * raw_pos) | BIT_SHIFT of (string * raw_pos)
  | LT of raw_pos | GT of raw_pos | COMPARE_OP of (string * raw_pos) | EQ_OP of (string * raw_pos)
  | BIT_AND of raw_pos | BIT_OR of raw_pos | BIT_XOR of raw_pos | AND_TIGHT of raw_pos | OR_TIGHT of raw_pos | DOTDOT of (string * raw_pos)
  | QUESTION_MARK of raw_pos | COLON of raw_pos | ASSIGN of (string * raw_pos) | COMMA of raw_pos | RIGHT_ARROW of raw_pos | NOT of raw_pos | AND of raw_pos | OR of raw_pos | XOR of raw_pos

and raw_interpolated_string = (string * raw_token list) list

let rec concat_bareword_paren accu = function
  | PRINT(s, pos1) :: PAREN(pos2) :: l
  | BAREWORD(s, pos1) :: PAREN(pos2) :: l ->
      concat_bareword_paren (PAREN(pos2) :: BAREWORD_PAREN(s, pos1) :: accu) l
  | RAW_IDENT(kind, ident, pos1) :: PAREN(pos2) :: l -> 
      concat_bareword_paren (PAREN(pos2) :: RAW_IDENT_PAREN(kind, ident, pos1) :: accu) l
  | [] -> List.rev accu
  | e :: l -> concat_bareword_paren (e :: accu) l

let rec raw_token_to_pos_and_token spaces = function
  | NUM(s, pos) -> pos, Parser.NUM(s, (spaces, pos))
  | RAW_STRING(s, pos) -> pos, Parser.RAW_STRING(s, (spaces, pos))
  | RAW_HERE_DOC(r, pos) -> pos, Parser.RAW_HERE_DOC(!r, (spaces, pos))
  | STRING(l, pos) -> pos, Parser.STRING(raw_interpolated_string_to_tokens l, (spaces, pos))
  | COMMAND_STRING(l, pos) -> pos, Parser.COMMAND_STRING(raw_interpolated_string_to_tokens l, (spaces, pos))
  | QR_PATTERN(s, opts, pos) -> pos, Parser.QR_PATTERN((raw_interpolated_string_to_tokens s, opts), (spaces, pos))
  | PATTERN(s, opts, pos) -> pos, Parser.PATTERN((raw_interpolated_string_to_tokens s, opts), (spaces, pos))
  | PATTERN_SUBST(from, to_, opts, pos) -> pos, Parser.PATTERN_SUBST((raw_interpolated_string_to_tokens from, raw_interpolated_string_to_tokens to_, opts), (spaces, pos))
  | HERE_DOC(l, pos) -> pos, Parser.HERE_DOC((raw_interpolated_string_to_tokens (fst !l), snd !l), (spaces, pos))
  | BAREWORD(s, pos) -> pos, Parser.BAREWORD(s, (spaces, pos))
  | BAREWORD_PAREN(s, pos) -> pos, Parser.BAREWORD_PAREN(s, (spaces, pos))
  | REVISION(s, pos) -> pos, Parser.REVISION(s, (spaces, pos))
  | PERL_CHECKER_COMMENT(s, pos) -> pos, Parser.PERL_CHECKER_COMMENT(s, (spaces, pos))
  | PO_COMMENT(s, pos) -> pos, Parser.PO_COMMENT(s, (spaces, pos))
  | POD(s, pos) -> pos, Parser.POD(s, (spaces, pos))
  | LABEL(s, pos) -> pos, Parser.LABEL(s, (spaces, pos))
  | PRINT(s, pos) -> pos, Parser.PRINT(s, (spaces, pos))
  | PRINT_TO_STAR(s, pos) -> pos, Parser.PRINT_TO_STAR(s, (spaces, pos))
  | PRINT_TO_SCALAR(s, pos) -> pos, Parser.PRINT_TO_SCALAR(s, (spaces, pos))
  | QUOTEWORDS(s, pos) -> pos, Parser.QUOTEWORDS(s, (spaces, pos))
  | COMPACT_HASH_SUBSCRIPT(s, pos) -> pos, Parser.COMPACT_HASH_SUBSCRIPT(s, (spaces, pos))
  | SCALAR_IDENT(kind, name, pos) -> pos, Parser.SCALAR_IDENT((kind, name), (spaces, pos))
  | ARRAY_IDENT(kind, name, pos) -> pos, Parser.ARRAY_IDENT((kind, name), (spaces, pos))
  | HASH_IDENT(kind, name, pos) -> pos, Parser.HASH_IDENT((kind, name), (spaces, pos))
  | FUNC_IDENT(kind, name, pos) -> pos, Parser.FUNC_IDENT((kind, name), (spaces, pos))
  | STAR_IDENT(kind, name, pos) -> pos, Parser.STAR_IDENT((kind, name), (spaces, pos))
  | RAW_IDENT(kind, name, pos) -> pos, Parser.RAW_IDENT((kind, name), (spaces, pos))
  | RAW_IDENT_PAREN(kind, name, pos) -> pos, Parser.RAW_IDENT_PAREN((kind, name), (spaces, pos))
  | ARRAYLEN_IDENT(kind, name, pos) -> pos, Parser.ARRAYLEN_IDENT((kind, name), (spaces, pos))
  | FUNC_DECL_WITH_PROTO(name, proto, pos) -> pos, Parser.FUNC_DECL_WITH_PROTO((name, proto), (spaces, pos))

  | NEW(pos) -> pos, Parser.NEW((), (spaces, pos))
  | FORMAT(pos) -> pos, Parser.FORMAT((), (spaces, pos))
  | COMPARE_OP(s, pos) -> pos, Parser.COMPARE_OP(s, (spaces, pos))
  | EQ_OP(s, pos) -> pos, Parser.EQ_OP(s, (spaces, pos))
  | ASSIGN(s, pos) -> pos, Parser.ASSIGN(s, (spaces, pos))
  | FOR(s, pos) -> pos, Parser.FOR(s, (spaces, pos))

  | DOTDOT(s, pos) -> pos, Parser.DOTDOT(s, (spaces, pos))
  | MULT(s, pos) -> pos, Parser.MULT(s, (spaces, pos))
  | BIT_SHIFT(s, pos) -> pos, Parser.BIT_SHIFT(s, (spaces, pos))
  | PLUS(s, pos) -> pos, Parser.PLUS(s, (spaces, pos))
  | ONE_SCALAR_PARA(s, pos) -> pos, Parser.ONE_SCALAR_PARA(s, (spaces, pos))
  | MY_OUR(s, pos) -> pos, Parser.MY_OUR(s, (spaces, pos))

  | EOF              (pos) -> pos, Parser.EOF              ((), (spaces, pos))
  | IF               (pos) -> pos, Parser.IF               ((), (spaces, pos))
  | ELSIF            (pos) -> pos, Parser.ELSIF            ((), (spaces, pos))
  | ELSE             (pos) -> pos, Parser.ELSE             ((), (spaces, pos))
  | UNLESS           (pos) -> pos, Parser.UNLESS           ((), (spaces, pos))
  | DO               (pos) -> pos, Parser.DO               ((), (spaces, pos))
  | WHILE            (pos) -> pos, Parser.WHILE            ((), (spaces, pos))
  | UNTIL            (pos) -> pos, Parser.UNTIL            ((), (spaces, pos))
  | CONTINUE         (pos) -> pos, Parser.CONTINUE         ((), (spaces, pos))
  | SUB              (pos) -> pos, Parser.SUB              ((), (spaces, pos))
  | LOCAL            (pos) -> pos, Parser.LOCAL            ((), (spaces, pos))
  | USE              (pos) -> pos, Parser.USE              ((), (spaces, pos))
  | PACKAGE          (pos) -> pos, Parser.PACKAGE          ((), (spaces, pos))
  | BEGIN            (pos) -> pos, Parser.BEGIN            ((), (spaces, pos))
  | END              (pos) -> pos, Parser.END              ((), (spaces, pos))
  | AT               (pos) -> pos, Parser.AT               ((), (spaces, pos))
  | DOLLAR           (pos) -> pos, Parser.DOLLAR           ((), (spaces, pos))
  | PERCENT          (pos) -> pos, Parser.PERCENT          ((), (spaces, pos))
  | AMPERSAND        (pos) -> pos, Parser.AMPERSAND        ((), (spaces, pos))
  | STAR             (pos) -> pos, Parser.STAR             ((), (spaces, pos))
  | ARRAYLEN         (pos) -> pos, Parser.ARRAYLEN         ((), (spaces, pos))
  | SEMI_COLON       (pos) -> pos, Parser.SEMI_COLON       ((), (spaces, pos))
  | PKG_SCOPE        (pos) -> pos, Parser.PKG_SCOPE        ((), (spaces, pos))
  | PAREN            (pos) -> pos, Parser.PAREN            ((), (spaces, pos))
  | PAREN_END        (pos) -> pos, Parser.PAREN_END        ((), (spaces, pos))
  | BRACKET          (pos) -> pos, Parser.BRACKET          ((), (spaces, pos))
  | BRACKET_END      (pos) -> pos, Parser.BRACKET_END      ((), (spaces, pos))
  | BRACKET_HASHREF  (pos) -> pos, Parser.BRACKET_HASHREF  ((), (spaces, pos))
  | ARRAYREF         (pos) -> pos, Parser.ARRAYREF         ((), (spaces, pos))
  | ARRAYREF_END     (pos) -> pos, Parser.ARRAYREF_END     ((), (spaces, pos))
  | ARROW            (pos) -> pos, Parser.ARROW            ((), (spaces, pos))
  | INCR             (pos) -> pos, Parser.INCR             ((), (spaces, pos))
  | DECR             (pos) -> pos, Parser.DECR             ((), (spaces, pos))
  | POWER            (pos) -> pos, Parser.POWER            ((), (spaces, pos))
  | TIGHT_NOT        (pos) -> pos, Parser.TIGHT_NOT        ((), (spaces, pos))
  | BIT_NEG          (pos) -> pos, Parser.BIT_NEG          ((), (spaces, pos))
  | REF              (pos) -> pos, Parser.REF              ((), (spaces, pos))
  | PATTERN_MATCH    (pos) -> pos, Parser.PATTERN_MATCH    ((), (spaces, pos))
  | PATTERN_MATCH_NOT(pos) -> pos, Parser.PATTERN_MATCH_NOT((), (spaces, pos))
  | LT               (pos) -> pos, Parser.LT               ((), (spaces, pos))
  | GT               (pos) -> pos, Parser.GT               ((), (spaces, pos))
  | BIT_AND          (pos) -> pos, Parser.BIT_AND          ((), (spaces, pos))
  | BIT_OR           (pos) -> pos, Parser.BIT_OR           ((), (spaces, pos))
  | BIT_XOR          (pos) -> pos, Parser.BIT_XOR          ((), (spaces, pos))
  | AND_TIGHT        (pos) -> pos, Parser.AND_TIGHT        ((), (spaces, pos))
  | OR_TIGHT         (pos) -> pos, Parser.OR_TIGHT         ((), (spaces, pos))
  | QUESTION_MARK    (pos) -> pos, Parser.QUESTION_MARK    ((), (spaces, pos))
  | COLON            (pos) -> pos, Parser.COLON            ((), (spaces, pos))
  | COMMA            (pos) -> pos, Parser.COMMA            ((), (spaces, pos))
  | RIGHT_ARROW      (pos) -> pos, Parser.RIGHT_ARROW      ((), (spaces, pos))
  | NOT              (pos) -> pos, Parser.NOT              ((), (spaces, pos))
  | AND              (pos) -> pos, Parser.AND              ((), (spaces, pos))
  | OR               (pos) -> pos, Parser.OR               ((), (spaces, pos))
  | XOR              (pos) -> pos, Parser.XOR              ((), (spaces, pos))

  | SPACE _ | CR -> internal_error "raw_token_to_token"

and raw_token_to_token spaces raw_token = 
  let _, token = raw_token_to_pos_and_token spaces raw_token in
  token

and raw_interpolated_string_to_tokens l =
  List.map (fun (s, rtok) -> s, concat_spaces Space_0 rtok) l

and concat_spaces spaces = function
  | CR :: l -> concat_spaces Space_cr l
  | SPACE n :: l ->
      let spaces' = 
	match spaces with
	| Space_cr -> Space_cr
	| Space_0 -> if n = 1 then Space_1 else Space_n
	| _ -> Space_n
      in
      concat_spaces spaces' l
  | [] -> []
  | token :: l -> raw_token_to_pos_and_token spaces token :: concat_spaces Space_0 l

let rec lexbuf2list accu t lexbuf =
  match t lexbuf with
  | EOF pos -> List.rev (EOF pos :: accu)
  | e -> lexbuf2list (e :: accu) t lexbuf

let get_token token lexbuf = 
  let tokens = lexbuf2list [] token lexbuf in
  let tokens = concat_bareword_paren [] tokens in
  let tokens = concat_spaces Space_0 tokens in
  tokens

let next_rule = Stack.create()


let pos lexbuf = lexeme_start lexbuf, lexeme_end lexbuf
let pos2sfull_with start end_ = Info.pos2sfull (!current_file, start, end_)
let pos2sfull lexbuf = pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf)
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
let not_ok_for_match = ref (-1)
let string_nestness = ref 0

let building_current_interpolated_string = Stack.create()
let building_current_string = Stack.create()
let current_string_start_pos = ref 0
let current_string_start_line = ref 0
let warn lexbuf err = prerr_endline (pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf) ^ err)
let die lexbuf err = failwith (pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf) ^ err)
let die_in_string lexbuf err = failwith (pos2sfull_with !current_string_start_pos (lexeme_end lexbuf) ^ err)

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
  let s, pos = ins t lexbuf in
  not_ok_for_match := lexeme_end lexbuf; 
  STRING(s, pos)

let next_s s t lexbuf =
  let r = Stack.top building_current_string in r := !r ^ s ;
  t lexbuf
let next t lexbuf = next_s (lexeme lexbuf) t lexbuf

let string_interpolate token pre lexbuf =
   let s = lexeme lexbuf in
   let local_lexbuf = Lexing.from_string (pre ^ s ^ " ") in (* add a space to help tokenizing "xxx$$" *)
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
}

let stash = [ '$' '@' '%' '&' '*' ]
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident = ident_start ['0'-'9' 'A'-'Z' 'a'-'z' '_'] *
let pattern_separator = [ '/' '!' ',' '|' ]

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
| "x" { MULT(lexeme lexbuf, pos lexbuf) }
| "+" { PLUS(lexeme lexbuf, pos lexbuf) }
| "-" { PLUS(lexeme lexbuf, pos lexbuf) }
| "." { PLUS(lexeme lexbuf, pos lexbuf) }
| "<<" { BIT_SHIFT(lexeme lexbuf, pos lexbuf) }
| ">>" { BIT_SHIFT(lexeme lexbuf, pos lexbuf) }
| "<" { LT(pos lexbuf) }
| ">" { GT(pos lexbuf) }
| "<=" | ">=" | "lt" | "gt" | "le" | "ge" { COMPARE_OP(lexeme lexbuf, pos lexbuf) }
| "==" | "!=" | "<=>" | "eq" | "ne" | "cmp" { EQ_OP(lexeme lexbuf, pos lexbuf) }
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
| "format"   { let _ = raw_here_doc_next_line "." in FORMAT(pos lexbuf) }
| "defined"  { ONE_SCALAR_PARA(lexeme lexbuf, pos lexbuf) }

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
    COMPACT_HASH_SUBSCRIPT(lexeme lexbuf, pos lexbuf)
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
      let s, pos = ins delimited_string lexbuf in
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
      let s, pos = ins delimited_string lexbuf in
      let opts, _ = raw_ins pattern_options lexbuf in
      PATTERN(s, opts, pos)
    ) 
  }

| "m" pattern_separator {
  delimit_char := lexeme_char lexbuf 1 ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins delimited_string lexbuf in
  let opts, _ = raw_ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN(s, opts, pos)
}

| "qr" pattern_separator {
  delimit_char := lexeme_char lexbuf 2 ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins delimited_string lexbuf in
  let opts, _ = raw_ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  QR_PATTERN(s, opts, pos)
}

| "s" pattern_separator {
  delimit_char := lexeme_char lexbuf 1 ;
  current_string_start_line := !current_file_current_line;
  let s1, (start, _) = ins delimited_string lexbuf in 
  let s2, (_, end_)  = ins delimited_string lexbuf in 
  let opts, _ = raw_ins pattern_options lexbuf in
  let pos = start, end_ in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}

| "tr" pattern_separator {
  delimit_char := lexeme_char lexbuf 2 ;
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
| "<<'" ident "'" { 
    not_ok_for_match := lexeme_end lexbuf; 
    RAW_HERE_DOC(raw_here_doc_next_line (skip_n_char_ 3 1 (lexeme lexbuf)), pos lexbuf)
  }
| "<<" ' '+ "'"
| "<<" ' '+ ident { 
    failwith (pos2sfull_with (lexeme_start lexbuf + 2) (lexeme_end lexbuf) ^ "No space allowed between \"<<\" and the marker")
  }
| "<<" ' '* '"' { 
    failwith (pos2sfull_with (lexeme_start lexbuf + 2) (lexeme_end lexbuf) ^ "Don't use <<\"MARK\", use <<MARK instead")
  }

| "\\" stash
| "\\" ['0'-'9' 'A'-'Z' 'a'-'z']
| "\\" ' '* '('
    { putback lexbuf 1; REF(pos lexbuf) }

| "sub" ' '+ ident ' '* '(' [ '$' '@' '\\' '&' ';' '%' ]* ')' {
    (* bloody prototypes, must be caught especially otherwise "($)" is badly tokenized *)
    (* and alas "($@)" is both valid as an expression and a prototype *)
    let s = lexeme lexbuf in
    let ident_start = non_index_from s 3 ' ' in
      
    let proto_start = String.index_from s ident_start '(' in
    let ident_end = non_rindex_from s (proto_start-1) ' ' in
    let ident = String.sub s ident_start (ident_end - ident_start + 1) in
    let prototype = skip_n_char_ (proto_start + 1) 1 s in

    FUNC_DECL_WITH_PROTO(ident, prototype, pos lexbuf)
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
| ident { not_ok_for_match := lexeme_end lexbuf; BAREWORD(lexeme lexbuf, pos lexbuf) }

| ident ":" { LABEL(lexeme lexbuf, pos lexbuf) }

| '-' [ 'a'-'z' 'A'-'Z' ] [ ' ' '(' ] { putback lexbuf 1; ONE_SCALAR_PARA(lexeme lexbuf, pos lexbuf) }

| ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9'] ['0'-'9' '_']*)+ 
| 'v' ['0'-'9'] ['0'-'9' '_']* ('.' ['0'-'9'] ['0'-'9' '_']*)* 
  { 
    not_ok_for_match := lexeme_end lexbuf; 
    REVISION(lexeme lexbuf, pos lexbuf)
  }

| ['0'-'9']* '.' ['0'-'9']+ (['e' 'E']['-' '+']?['0'-'9']+)? 
| ['0'-'9'] ['0'-'9' '_']*  (['e' 'E']['-' '+']?['0'-'9']+)?
| "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ { 
    not_ok_for_match := lexeme_end lexbuf; 
    NUM(lexeme lexbuf, pos lexbuf)
  }

| '"'   { ins_to_string string lexbuf }
| "'"   { raw_ins_to_string rawstring lexbuf }
| '`'   { delimit_char := '`'; 
	  current_string_start_line := !current_file_current_line;
	  not_ok_for_match := lexeme_end lexbuf; 
	  let s, pos = ins delimited_string lexbuf in
	  check_multi_line_delimited_string None pos ;
	  COMMAND_STRING(s, pos) }
| "q("  { raw_ins_to_string qstring lexbuf }
| "qq(" { ins_to_string qqstring lexbuf }
| "qw(" { let s, pos = raw_ins qstring lexbuf in QUOTEWORDS(s, pos) }

| eof   { EOF(pos lexbuf) }
| _ { failwith (Printf.sprintf "%serror tokenizing <<%s>>" (pos2sfull lexbuf) (lexeme lexbuf)) }

and string = parse
| '"' { () }
| '\\' { Stack.push string next_rule ; string_escape lexbuf }
| '$'  { Stack.push string next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push string next_rule ; string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next string lexbuf
  }
| [^ '\n' '\\' '"' '$' '@']+ { next string lexbuf }
| eof { die_in_string lexbuf "Unterminated_string" }

and delimited_string = parse
| '\\' { Stack.push delimited_string next_rule ; string_escape lexbuf }
| '$'  { Stack.push delimited_string next_rule ; delimited_string_interpolate_scalar lexbuf }
| '@'  { Stack.push delimited_string next_rule ; delimited_string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next delimited_string lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_delimited_string" }
| [ ^ '\n' '\\' '$' '@'] { if lexeme_char lexbuf 0 <> !delimit_char then next delimited_string lexbuf }

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
| ')' { 
    if !string_nestness <> 0 then (decr string_nestness; next qqstring lexbuf)
  }
| '(' {
    incr string_nestness;
    next qqstring lexbuf
  }
| '\\' { Stack.push qqstring next_rule ; string_escape lexbuf }
| '$'  { Stack.push qqstring next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push qqstring next_rule ; string_interpolate_array lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next qqstring lexbuf
  }
| [^ '\n' '(' ')' '\\' '$' '@']+ { next qqstring lexbuf }
| eof { die_in_string lexbuf "Unterminated_qqstring" }

and qstring = parse
| ')' { 
    if !string_nestness <> 0 then (decr string_nestness ; next qstring lexbuf) 
  }
| '(' {
    incr string_nestness;
    next qstring lexbuf
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next qstring lexbuf
  }
| [^ '\n' '(' ')']+ { next qstring lexbuf }
| eof { die_in_string lexbuf "Unterminated_qstring" }

and here_doc = parse
| '\\' { Stack.push here_doc next_rule ; string_escape lexbuf }
| '$'  { Stack.push here_doc next_rule ; string_interpolate_scalar lexbuf }
| '@'  { Stack.push here_doc next_rule ; string_interpolate_array lexbuf }
| [ ^ '\n' '\\' '$' '@' ]* {
    let s = lexeme lexbuf in
    if chomps s <> !current_here_doc_mark
    then next_s s here_doc lexbuf 
    else if s <> !current_here_doc_mark then Printf.eprintf "%sTrailing spaces after HERE-document mark\n" (pos2sfull lexbuf)
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
    else if s <> !current_here_doc_mark then Printf.eprintf "%sTrailing spaces after HERE-document mark\n" (pos2sfull lexbuf)
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next raw_here_doc lexbuf
  }
| eof { die_in_string lexbuf "Unterminated_raw_here_doc" }


and string_escape = parse
| '0' { next_s "\000" (Stack.pop next_rule) lexbuf }
| '"' { next_s "\"" (Stack.pop next_rule) lexbuf }
| ''' { next_s "'"  (Stack.pop next_rule) lexbuf }
| ':' { next_s ":"  (Stack.pop next_rule) lexbuf }
| '\\'{ next_s "\\" (Stack.pop next_rule) lexbuf }
| 'n' { next_s "\n" (Stack.pop next_rule) lexbuf }
| 't' { next_s "\t" (Stack.pop next_rule) lexbuf }
| 'x' _ _ { 
  try
    let s = String.make 1 (Char.chr (int_of_string ("0" ^ lexeme lexbuf))) in
    next_s s (Stack.pop next_rule) lexbuf 
  with Failure("int_of_string") -> die_in_string lexbuf ("Bad_hex_in_string \"" ^ lexeme lexbuf ^ "\"")
  }
| '\n' { die lexbuf "do not use \"\\\" before end-of-line, it's useless and generally bad" }
| _ { next_s ("\\" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }


and string_interpolate_scalar = parse
| '$' ident
| ['0'-'9']
| '{' [^ '{' '}']* '}'
| (ident | (ident? ("::" ident)+)) "->"? (('{' [^ '{' '}' '\n']* '}') | ('[' [^ '[' ']' '\n']* ']'))*
| [^ '{' '}' ' ' '\n' '"'] { (* eg: $! $$ *)
      string_interpolate token "$" lexbuf
  }

| "{"
| ident "->"? '{'
| '"' { putback lexbuf 1; next_s "$" (Stack.pop next_rule) lexbuf }
| eof {                   next_s "$" (Stack.pop next_rule) lexbuf }
| _ { warn lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); next_s ("$" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }

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
    if c <> !delimit_char && c <> '|' && c<>')' && c<>'/' && c<>' ' then warn lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); 
    putback lexbuf 1;
    next_s "$" (Stack.pop next_rule) lexbuf
  }

and string_interpolate_array = parse
| '$' ident
| '{' [^ '{' '}']* '}'
| (ident | (ident? ("::" ident)+)) { string_interpolate token "@" lexbuf }

| [ '@' '*' '<' '>' ']' '.' '('] { next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }
| '"' { putback lexbuf 1; next_s "@" (Stack.pop next_rule) lexbuf }
| eof {                   next_s "@" (Stack.pop next_rule) lexbuf }
| _ { warn lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf)); next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }

and delimited_string_interpolate_array = parse
| '$' ident
| '{' [^ '{' '}']* '}'
| (ident | (ident? ("::" ident)+)) { string_interpolate token "@" lexbuf }

| [ '@' '*' '<' '>' ']' '.' '('] { next_s ("@" ^ lexeme lexbuf) (Stack.pop next_rule) lexbuf }
| eof { next_s "@" (Stack.pop next_rule) lexbuf }
| _ { 
    let c = lexeme_char lexbuf 0 in
    if c <> !delimit_char then warn lexbuf (Printf.sprintf "weird \"%s\" in string" (lexeme lexbuf));
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
