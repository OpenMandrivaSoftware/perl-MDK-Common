{ (* -*- caml -*- *)
open Common
open Types
open Lexing
open Info

type raw_token =
  | EOF of raw_pos
  | SPACE of int
  | CR
  | NUM of (string * raw_pos)
  | STRING of (string * raw_pos)
  | BAREWORD of (string * raw_pos)
  | REVISION of (string * raw_pos)
  | COMMENT of (string * raw_pos)
  | POD of (string * raw_pos)
  | LABEL of (string * raw_pos)
  | COMMAND_STRING of (string * raw_pos)
  | PRINT_TO_STAR of (string * raw_pos)
  | PRINT_TO_SCALAR of (string * raw_pos)
  | QUOTEWORDS of (string * raw_pos)
  | COMPACT_HASH_SUBSCRIPT of (string * raw_pos)
  | HERE_DOC of ((string * raw_pos) ref * raw_pos)
  | PATTERN of (string * string * raw_pos)
  | PATTERN_SUBST of (string * string * string * raw_pos)
  | SCALAR_IDENT of (string option * string * raw_pos)
  | ARRAY_IDENT of (string option * string * raw_pos)
  | HASH_IDENT of (string option * string * raw_pos)
  | FUNC_IDENT of (string option * string * raw_pos)
  | STAR_IDENT of (string option * string * raw_pos)
  | RAW_IDENT of (string option * string * raw_pos)
  | ARRAYLEN_IDENT of (string option * string * raw_pos)
  | FUNC_DECL_WITH_PROTO of (string * string * raw_pos)

  | IF of raw_pos | ELSIF of raw_pos | ELSE of raw_pos | UNLESS of raw_pos | DO of raw_pos | WHILE of raw_pos | UNTIL of raw_pos | MY of raw_pos | CONTINUE of raw_pos | SUB of raw_pos
  | LOCAL of raw_pos | FOR of (string * raw_pos) | USE of raw_pos | PACKAGE of raw_pos | BEGIN of raw_pos | END of raw_pos | PRINT of (string * raw_pos) 
  | NEW of (raw_pos) | FORMAT of (raw_pos) | AT of raw_pos | DOLLAR of raw_pos | PERCENT of raw_pos | AMPERSAND of raw_pos
  | STAR of raw_pos | ARRAYLEN of raw_pos | SEMI_COLON of raw_pos | PKG_SCOPE of raw_pos | PAREN of raw_pos | PAREN_END of raw_pos | BRACKET of raw_pos
  | BRACKET_END of raw_pos | BRACKET_HASHREF of raw_pos | ARRAYREF of raw_pos | ARRAYREF_END of raw_pos | ARROW of raw_pos | INCR of raw_pos | DECR of raw_pos
  | POWER of raw_pos | TIGHT_NOT of raw_pos | BIT_NEG of raw_pos | REF of raw_pos | PATTERN_MATCH of raw_pos | PATTERN_MATCH_NOT of raw_pos | MULT of raw_pos
  | DIVISION of raw_pos | MODULO of raw_pos | REPLICATE of raw_pos | PLUS of raw_pos | MINUS of raw_pos | CONCAT of raw_pos | BIT_SHIFT_LEFT of raw_pos
  | BIT_SHIFT_RIGHT of raw_pos | LT of raw_pos | GT of raw_pos | COMPARE_OP of (string * raw_pos) | EQ_OP of (string * raw_pos)
  | BIT_AND of raw_pos | BIT_OR of raw_pos | BIT_XOR of raw_pos | AND_TIGHT of raw_pos | OR_TIGHT of raw_pos | DOTDOT of raw_pos | DOTDOTDOT of raw_pos
  | QUESTION_MARK of raw_pos | COLON of raw_pos | ASSIGN of (string * raw_pos) | COMMA of raw_pos | RIGHT_ARROW of raw_pos | NOT of raw_pos | AND of raw_pos | OR of raw_pos | XOR of raw_pos

let saved_token = ref None

let concat_bareword_paren get_token lexbuf =
  let token = match !saved_token with
  | None -> get_token lexbuf
  | Some t -> t
  in
  let token, next =
    match token with
    | Parser.PRINT(s, both)
    | Parser.BAREWORD(s, both) ->
	let next_token = get_token lexbuf in
	(match next_token with Parser.PAREN(_, (Space_0, _)) -> Parser.BAREWORD_PAREN(s, both) | _ -> token), Some next_token
    | Parser.RAW_IDENT(ident, both) ->
	let next_token = get_token lexbuf in
	(match next_token with Parser.PAREN(_, (Space_0, _)) -> Parser.RAW_IDENT_PAREN(ident, both) | _ -> token), Some next_token
    | _ -> token, None
  in
  saved_token := next ; token

let rec concat_spaces get_token lexbuf =
  let rec get_spaces spaces lexbuf =
    match get_token lexbuf with
    | CR -> get_spaces Space_cr lexbuf
    | SPACE n ->
	let spaces' = 
	  match spaces with
	  | Space_cr -> Space_cr
	  | Space_0 -> if n = 1 then Space_1 else Space_n
	  | _ -> Space_n
	in
	get_spaces spaces' lexbuf
    | token -> token, spaces
  in
  let token, spaces = get_spaces Space_0 lexbuf in
  match token with
  | NUM(s, pos) -> Parser.NUM(s, (spaces, pos))
  | STRING(s, pos) -> Parser.STRING(s, (spaces, pos))
  | BAREWORD(s, pos) -> Parser.BAREWORD(s, (spaces, pos))
  | REVISION(s, pos) -> Parser.REVISION(s, (spaces, pos))
  | COMMENT(s, pos) -> Parser.COMMENT(s, (spaces, pos))
  | POD(s, pos) -> Parser.POD(s, (spaces, pos))
  | LABEL(s, pos) -> Parser.LABEL(s, (spaces, pos))
  | COMMAND_STRING(s, pos) -> Parser.COMMAND_STRING(s, (spaces, pos))
  | PRINT(s, pos) -> Parser.PRINT(s, (spaces, pos))
  | PRINT_TO_STAR(s, pos) -> Parser.PRINT_TO_STAR(s, (spaces, pos))
  | PRINT_TO_SCALAR(s, pos) -> Parser.PRINT_TO_SCALAR(s, (spaces, pos))
  | QUOTEWORDS(s, pos) -> Parser.QUOTEWORDS(s, (spaces, pos))
  | COMPACT_HASH_SUBSCRIPT(s, pos) -> Parser.COMPACT_HASH_SUBSCRIPT(s, (spaces, pos))
  | HERE_DOC(r, pos) -> Parser.HERE_DOC(r, (spaces, pos))
  | PATTERN(s, opts, pos) -> Parser.PATTERN((s, opts), (spaces, pos))
  | PATTERN_SUBST(from, to_, opts, pos) -> Parser.PATTERN_SUBST((from, to_, opts), (spaces, pos))
  | SCALAR_IDENT(kind, name, pos) -> Parser.SCALAR_IDENT((kind, name), (spaces, pos))
  | ARRAY_IDENT(kind, name, pos) -> Parser.ARRAY_IDENT((kind, name), (spaces, pos))
  | HASH_IDENT(kind, name, pos) -> Parser.HASH_IDENT((kind, name), (spaces, pos))
  | FUNC_IDENT(kind, name, pos) -> Parser.FUNC_IDENT((kind, name), (spaces, pos))
  | STAR_IDENT(kind, name, pos) -> Parser.STAR_IDENT((kind, name), (spaces, pos))
  | RAW_IDENT(kind, name, pos) -> Parser.RAW_IDENT((kind, name), (spaces, pos))
  | ARRAYLEN_IDENT(kind, name, pos) -> Parser.ARRAYLEN_IDENT((kind, name), (spaces, pos))
  | FUNC_DECL_WITH_PROTO(name, proto, pos) -> Parser.FUNC_DECL_WITH_PROTO((name, proto), (spaces, pos))

  | NEW(pos) -> Parser.NEW((), (spaces, pos))
  | FORMAT(pos) -> Parser.FORMAT((), (spaces, pos))
  | COMPARE_OP(s, pos) -> Parser.COMPARE_OP(s, (spaces, pos))
  | EQ_OP(s, pos) -> Parser.EQ_OP(s, (spaces, pos))
  | ASSIGN(s, pos) -> Parser.ASSIGN(s, (spaces, pos))
  | FOR(s, pos) -> Parser.FOR(s, (spaces, pos))

  | EOF              (pos) -> Parser.EOF              ((), (spaces, pos))
  | IF               (pos) -> Parser.IF               ((), (spaces, pos))
  | ELSIF            (pos) -> Parser.ELSIF            ((), (spaces, pos))
  | ELSE             (pos) -> Parser.ELSE             ((), (spaces, pos))
  | UNLESS           (pos) -> Parser.UNLESS           ((), (spaces, pos))
  | DO               (pos) -> Parser.DO               ((), (spaces, pos))
  | WHILE            (pos) -> Parser.WHILE            ((), (spaces, pos))
  | UNTIL            (pos) -> Parser.UNTIL            ((), (spaces, pos))
  | MY               (pos) -> Parser.MY               ((), (spaces, pos))
  | CONTINUE         (pos) -> Parser.CONTINUE         ((), (spaces, pos))
  | SUB              (pos) -> Parser.SUB              ((), (spaces, pos))
  | LOCAL            (pos) -> Parser.LOCAL            ((), (spaces, pos))
  | USE              (pos) -> Parser.USE              ((), (spaces, pos))
  | PACKAGE          (pos) -> Parser.PACKAGE          ((), (spaces, pos))
  | BEGIN            (pos) -> Parser.BEGIN            ((), (spaces, pos))
  | END              (pos) -> Parser.END              ((), (spaces, pos))
  | AT               (pos) -> Parser.AT               ((), (spaces, pos))
  | DOLLAR           (pos) -> Parser.DOLLAR           ((), (spaces, pos))
  | PERCENT          (pos) -> Parser.PERCENT          ((), (spaces, pos))
  | AMPERSAND        (pos) -> Parser.AMPERSAND        ((), (spaces, pos))
  | STAR             (pos) -> Parser.STAR             ((), (spaces, pos))
  | ARRAYLEN         (pos) -> Parser.ARRAYLEN         ((), (spaces, pos))
  | SEMI_COLON       (pos) -> Parser.SEMI_COLON       ((), (spaces, pos))
  | PKG_SCOPE        (pos) -> Parser.PKG_SCOPE        ((), (spaces, pos))
  | PAREN            (pos) -> Parser.PAREN            ((), (spaces, pos))
  | PAREN_END        (pos) -> Parser.PAREN_END        ((), (spaces, pos))
  | BRACKET          (pos) -> Parser.BRACKET          ((), (spaces, pos))
  | BRACKET_END      (pos) -> Parser.BRACKET_END      ((), (spaces, pos))
  | BRACKET_HASHREF  (pos) -> Parser.BRACKET_HASHREF  ((), (spaces, pos))
  | ARRAYREF         (pos) -> Parser.ARRAYREF         ((), (spaces, pos))
  | ARRAYREF_END     (pos) -> Parser.ARRAYREF_END     ((), (spaces, pos))
  | ARROW            (pos) -> Parser.ARROW            ((), (spaces, pos))
  | INCR             (pos) -> Parser.INCR             ((), (spaces, pos))
  | DECR             (pos) -> Parser.DECR             ((), (spaces, pos))
  | POWER            (pos) -> Parser.POWER            ((), (spaces, pos))
  | TIGHT_NOT        (pos) -> Parser.TIGHT_NOT        ((), (spaces, pos))
  | BIT_NEG          (pos) -> Parser.BIT_NEG          ((), (spaces, pos))
  | REF              (pos) -> Parser.REF              ((), (spaces, pos))
  | PATTERN_MATCH    (pos) -> Parser.PATTERN_MATCH    ((), (spaces, pos))
  | PATTERN_MATCH_NOT(pos) -> Parser.PATTERN_MATCH_NOT((), (spaces, pos))
  | MULT             (pos) -> Parser.MULT             ((), (spaces, pos))
  | DIVISION         (pos) -> Parser.DIVISION         ((), (spaces, pos))
  | MODULO           (pos) -> Parser.MODULO           ((), (spaces, pos))
  | REPLICATE        (pos) -> Parser.REPLICATE        ((), (spaces, pos))
  | PLUS             (pos) -> Parser.PLUS             ((), (spaces, pos))
  | MINUS            (pos) -> Parser.MINUS            ((), (spaces, pos))
  | CONCAT           (pos) -> Parser.CONCAT           ((), (spaces, pos))
  | BIT_SHIFT_LEFT   (pos) -> Parser.BIT_SHIFT_LEFT   ((), (spaces, pos))
  | BIT_SHIFT_RIGHT  (pos) -> Parser.BIT_SHIFT_RIGHT  ((), (spaces, pos))
  | LT               (pos) -> Parser.LT               ((), (spaces, pos))
  | GT               (pos) -> Parser.GT               ((), (spaces, pos))
  | BIT_AND          (pos) -> Parser.BIT_AND          ((), (spaces, pos))
  | BIT_OR           (pos) -> Parser.BIT_OR           ((), (spaces, pos))
  | BIT_XOR          (pos) -> Parser.BIT_XOR          ((), (spaces, pos))
  | AND_TIGHT        (pos) -> Parser.AND_TIGHT        ((), (spaces, pos))
  | OR_TIGHT         (pos) -> Parser.OR_TIGHT         ((), (spaces, pos))
  | DOTDOT           (pos) -> Parser.DOTDOT           ((), (spaces, pos))
  | DOTDOTDOT        (pos) -> Parser.DOTDOTDOT        ((), (spaces, pos))
  | QUESTION_MARK    (pos) -> Parser.QUESTION_MARK    ((), (spaces, pos))
  | COLON            (pos) -> Parser.COLON            ((), (spaces, pos))
  | COMMA            (pos) -> Parser.COMMA            ((), (spaces, pos))
  | RIGHT_ARROW      (pos) -> Parser.RIGHT_ARROW      ((), (spaces, pos))
  | NOT              (pos) -> Parser.NOT              ((), (spaces, pos))
  | AND              (pos) -> Parser.AND              ((), (spaces, pos))
  | OR               (pos) -> Parser.OR               ((), (spaces, pos))
  | XOR              (pos) -> Parser.XOR              ((), (spaces, pos))

  | SPACE _ | CR -> internal_error "raw_token_to_token"

let rec lexbuf2list t lexbuf =
  let rec f () =
    match t lexbuf with
    | Parser.EOF _ -> []
    | e -> e :: f()
  in
  let l = f() in
  l

let next_rule = ref None

let bpos = -1,-1
let pos lexbuf = lexeme_start lexbuf, lexeme_end lexbuf
let pos2sfull_with start end_ = Info.pos2sfull (!current_file, start, end_)
let pos2sfull lexbuf = pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf)
let putback lexbuf nb = lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - nb

let add_a_new_line raw_pos = 
   incr current_file_current_line ;
   lpush current_file_lines_starts raw_pos

let here_docs = Queue.create()
let current_here_doc_mark = ref ""

let here_doc_next_line mark interpolate =
  let here_doc_ref = ref("", bpos) in
  Queue.push (interpolate, mark, here_doc_ref) here_docs ;
  here_doc_ref

let delimit_char = ref '/'
let not_ok_for_match = ref (-1)
let string_nestness = ref 0

let building_current_string = ref ""
let current_string_start_pos = ref 0
let current_string_start_line = ref 0
let die lexbuf err = failwith (pos2sfull_with !current_string_start_pos (lexeme_end lexbuf) ^ err)

let ins t lexbuf = 
  building_current_string := ""; 
  current_string_start_pos := lexeme_start lexbuf;
  t lexbuf ;
  !building_current_string, (!current_string_start_pos, lexeme_end lexbuf)
let ins_to_string t lexbuf =
  let s, pos = ins t lexbuf in
  not_ok_for_match := lexeme_end lexbuf; 
  STRING(s, pos)

let next_s s t lexbuf =
  building_current_string := !building_current_string ^ s ;
  t lexbuf
let next t lexbuf = next_s (lexeme lexbuf) t lexbuf

let ident_type_from_char fq name lexbuf c = 
  not_ok_for_match := lexeme_end lexbuf; 
  match c with
  | '$' -> SCALAR_IDENT(fq, name, pos lexbuf)
  | '@' -> ARRAY_IDENT (fq, name, pos lexbuf)
  | '%' -> HASH_IDENT  (fq, name, pos lexbuf)
  | '&' -> FUNC_IDENT  (fq, name, pos lexbuf)
  | '*' -> STAR_IDENT  (fq, name, pos lexbuf)
  | _ -> internal_error "ident_type_from_char"

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
| ' '+ { 
    (* propagate not_ok_for_match when it was set by the previous token *)
    if lexeme_start lexbuf = !not_ok_for_match then not_ok_for_match := lexeme_end lexbuf; 
    SPACE(lexeme_end lexbuf - lexeme_start lexbuf)
  }
| '#' [^ '\n']* { SPACE(1) }

| "\n=" { 
    add_a_new_line(lexeme_end lexbuf - 1);
    let _ = ins pod_command lexbuf in token lexbuf 
  }

| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    (try
      let (interpolate, mark, r) = Queue.pop here_docs in
      current_here_doc_mark := mark ;
      r := ins (if interpolate then here_doc else raw_here_doc) lexbuf
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
| "*" { MULT(pos lexbuf) }
| "%" { MODULO(pos lexbuf) }
| "x" { REPLICATE(pos lexbuf) }
| "+" { PLUS(pos lexbuf) }
| "-" { MINUS(pos lexbuf) }
| "." { CONCAT(pos lexbuf) }
| "<<" { BIT_SHIFT_LEFT(pos lexbuf) }
| ">>" { BIT_SHIFT_RIGHT(pos lexbuf) }
| "<" { LT(pos lexbuf) }
| ">" { GT(pos lexbuf) }
| "<=" | ">=" | "lt" | "gt" | "le" | "ge" { COMPARE_OP(lexeme lexbuf, pos lexbuf) }
| "==" | "!=" | "<=>" | "eq" | "ne" | "cmp" { EQ_OP(lexeme lexbuf, pos lexbuf) }
| "&" { BIT_AND(pos lexbuf) }
| "|" { BIT_OR(pos lexbuf) }
| "^" { BIT_XOR(pos lexbuf) }
| "&&" { AND_TIGHT(pos lexbuf) }
| "||" { OR_TIGHT(pos lexbuf) }
| ".." { DOTDOT(pos lexbuf) }
| "..." { DOTDOTDOT(pos lexbuf) }
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
| "my"       { MY(pos lexbuf) }
| "local"    { LOCAL(pos lexbuf) }
| "continue" { CONTINUE(pos lexbuf) }
| "sub"      { SUB(pos lexbuf) }
| "package"  { PACKAGE(pos lexbuf) }
| "use"      { USE(pos lexbuf) }
| "BEGIN"    { BEGIN(pos lexbuf) }
| "END"      { END(pos lexbuf) }
| "print"    { PRINT(lexeme lexbuf, pos lexbuf) }
| "new"      { NEW(pos lexbuf) }
| "format"   { let _ = here_doc_next_line "." false in FORMAT(pos lexbuf) }

| "split"
| "grep"  { (* ok_for_match! *) BAREWORD(lexeme lexbuf, pos lexbuf) }

| "print " ident ' ' { 
    putback lexbuf 1; 
    PRINT_TO_STAR(skip_n_char_ 6 1 (lexeme lexbuf), pos lexbuf);
  }
| "print $" ident ' ' { 
    putback lexbuf 1; 
    PRINT_TO_SCALAR(skip_n_char_ 7 1 (lexeme lexbuf), pos lexbuf);
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
| '*' ['$' '{'] { putback lexbuf 1; if lexeme_start lexbuf = !not_ok_for_match then MULT(pos lexbuf) else STAR(pos lexbuf) }


| ';' { SEMI_COLON(pos lexbuf) }
| '(' { PAREN(pos lexbuf) }
| '{' { BRACKET(pos lexbuf) }
| "+{"{ BRACKET_HASHREF(pos lexbuf) }
| '[' { ARRAYREF(pos lexbuf) }
| ')' { not_ok_for_match := lexeme_end lexbuf; PAREN_END(pos lexbuf) }
| '}' { not_ok_for_match := lexeme_end lexbuf; BRACKET_END(pos lexbuf) }
| ']' { not_ok_for_match := lexeme_end lexbuf; ARRAYREF_END(pos lexbuf) }

| "/" {
    if lexeme_start lexbuf = !not_ok_for_match then DIVISION(pos lexbuf)
    else (
      delimit_char := '/' ;
      current_string_start_line := !current_file_current_line;
      let s, pos = ins delimited_string lexbuf in
      let opts, _ = ins pattern_options lexbuf in
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
      let opts, _ = ins pattern_options lexbuf in
      PATTERN(s, opts, pos)
    ) 
  }

| "m" pattern_separator {
  delimit_char := lexeme_char lexbuf 1 ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins delimited_string lexbuf in
  let opts, _ = ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN(s, opts, pos)
}

| "qr" pattern_separator {
  delimit_char := lexeme_char lexbuf 2 ;
  current_string_start_line := !current_file_current_line;
  let s, pos = ins delimited_string lexbuf in
  let opts, _ = ins pattern_options lexbuf in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN(s, opts, pos)
}

| "s" pattern_separator {
  delimit_char := lexeme_char lexbuf 1 ;
  current_string_start_line := !current_file_current_line;
  let s1, (start, _) = ins delimited_string lexbuf in 
  let s2, (_, end_)  = ins delimited_string lexbuf in 
  let opts, _ = ins pattern_options lexbuf in
  let pos = start, end_ in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}

| "tr" pattern_separator {
  delimit_char := lexeme_char lexbuf 2 ;
  current_string_start_line := !current_file_current_line;
  let s1, (start, _) = ins delimited_string lexbuf in 
  let s2, (_, end_)  = ins delimited_string lexbuf in 
  let opts, _ = ins pattern_options lexbuf in
  let pos = start, end_ in
  check_multi_line_delimited_string None pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}
    
| "<<" ident { 
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char 2 (lexeme lexbuf)) true, pos lexbuf)
  }
| "<<'" ident "'" { 
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char_ 3 1 (lexeme lexbuf)) false, pos lexbuf)
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

| "$$" [^ 'a'-'z' 'A'-'Z' '_' '{'] { putback lexbuf 1; SCALAR_IDENT(None, "$$", pos lexbuf) }

| stash "::" { putback lexbuf 2; ident_type_from_char None "main" lexbuf (lexeme_char lexbuf 0) }

| ident? ("::" ident)+ { ident_from_lexbuf lexbuf }
| ident { not_ok_for_match := lexeme_end lexbuf; BAREWORD(lexeme lexbuf, pos lexbuf) }

| ident ":" { LABEL(lexeme lexbuf, pos lexbuf) }

| '-' [ 'a'-'z' 'A'-'Z' ] [ ' ' '(' ] { putback lexbuf 1; BAREWORD(lexeme lexbuf, pos lexbuf) }

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
| "'"   { ins_to_string rawstring lexbuf }
| '`'   { delimit_char := '`'; 
	  current_string_start_line := !current_file_current_line;
	  not_ok_for_match := lexeme_end lexbuf; 
	  let s, pos = ins delimited_string lexbuf in
	  check_multi_line_delimited_string None pos ;
	  COMMAND_STRING(s, pos) }
| "q("  { ins_to_string qstring lexbuf }
| "qq(" { ins_to_string qqstring lexbuf }
| "qw(" { let s, pos = ins qstring lexbuf in QUOTEWORDS(s, pos) }

| eof   { EOF(pos lexbuf) }
| _ { failwith (Printf.sprintf "%serror tokenizing <<%s>>" (pos2sfull lexbuf) (lexeme lexbuf)) }

and string = parse
  '"' { () }
| '\\' { next_rule := Some string ; string_escape lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next string lexbuf
  }
| [^ '\n' '\\' '"']+ { next string lexbuf }
| eof { die lexbuf "Unterminated_string" }

and delimited_string = parse
| '\\' { next_rule := Some delimited_string ; string_escape lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next delimited_string lexbuf
  }
| eof { die lexbuf "Unterminated_delimited_string" }
| [ ^ '\\' '\n' ] { if lexeme_char lexbuf 0 <> !delimit_char then next delimited_string lexbuf }

and rawstring = parse
  ''' { () }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next rawstring lexbuf
  }
| '\\' { next rawstring lexbuf }
| "\\'" { next_s "'" rawstring lexbuf }
| [^ '\n' ''' '\\']+ { next rawstring lexbuf }
| eof { die lexbuf "Unterminated_rawstring" }

and qqstring = parse
  ')' { 
    if !string_nestness <> 0 then (decr string_nestness; next qqstring lexbuf)
  }
| '(' {
    incr string_nestness;
    next qqstring lexbuf
  }
| '\\' { next_rule := Some qqstring ; string_escape lexbuf }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next qqstring lexbuf
  }
| [^ '\n' '(' ')' '\\']+ { next qqstring lexbuf }
| eof { die lexbuf "Unterminated_qqstring" }

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
| eof { die lexbuf "Unterminated_qstring" }

and here_doc = parse
| '\\' { next_rule := Some here_doc ; string_escape lexbuf }
| [ ^ '\n' '\\' ]* {
    let s = lexeme lexbuf in
    if chomps s <> !current_here_doc_mark
    then next_s s here_doc lexbuf 
    else if s <> !current_here_doc_mark then Printf.eprintf "%sTrailing spaces after HERE-document mark\n" (pos2sfull lexbuf)
  }
| '\n' { 
    add_a_new_line(lexeme_end lexbuf);
    next here_doc lexbuf
  }
| eof { die lexbuf "Unterminated_here_doc" }

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
| eof { die lexbuf "Unterminated_raw_here_doc" }


and string_escape = parse
| '0' { next_s "\000" (some !next_rule) lexbuf }
| '"' { next_s "\"" (some !next_rule) lexbuf }
| ''' { next_s "'"  (some !next_rule) lexbuf }
| 'n' { next_s "\n" (some !next_rule) lexbuf }
| 't' { next_s "\t" (some !next_rule) lexbuf }
| 'x' _ _ { 
  try
    let s = String.make 1 (Char.chr (int_of_string ("0" ^ lexeme lexbuf))) in
    next_s s (some !next_rule) lexbuf 
  with Failure("int_of_string") -> die lexbuf ("Bad_hex_in_string \"" ^ lexeme lexbuf ^ "\"")
  }
| _ { next_s ("\\" ^ lexeme lexbuf) (some !next_rule) lexbuf }

and pattern_options = parse
| [ 'g' 'i' 'm' 'o' 's' 'x' 'e' 'd' ] { next pattern_options lexbuf }
| _ { putback lexbuf 1; () }

and pod_command = parse
| [^ '\n' ]+ {
  let s = lexeme lexbuf in
  let command = String.sub s 0 (try String.index s ' ' with Not_found -> String.length s) in
  match command with
  | "cut" ->
    if !building_current_string = "" then
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
