{ (* -*- caml -*- *)
open Parser
open Common
open Lexing
open Info


let next_rule = ref None

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

let ins t lexbuf = 
  building_current_string := ""; 
  current_string_start_pos := lexeme_start lexbuf;
  t lexbuf ;
  !building_current_string, (!current_file, !current_string_start_pos, lexeme_end lexbuf)
let ins_to_string t lexbuf =
  let s, pos = ins t lexbuf in
  not_ok_for_match := lexeme_end lexbuf; 
  STRING(s, pos)

let next_s s t lexbuf =
  building_current_string := !building_current_string ^ s ;
  t lexbuf
let next t lexbuf = next_s (lexeme lexbuf) t lexbuf

let pos lexbuf = !current_file, lexeme_start lexbuf, lexeme_end lexbuf
let pos2sfull_with start end_ = Info.pos2sfull (!current_file, start, end_)
let pos2sfull lexbuf = pos2sfull_with (lexeme_start lexbuf) (lexeme_end lexbuf)

let putback lexbuf nb = lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - nb

let die lexbuf err = failwith (pos2sfull_with !current_string_start_pos (lexeme_end lexbuf) ^ err)

let rec lexbuf2list t lexbuf =
  let rec f () =
    match t lexbuf with
    | EOF -> []
    | e -> e :: f()
  in
  let l = f() in
  l

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

let check_multi_line_delimited_string opts (_, start, end_) =
   let check =
     match opts with
     | None -> true
     | Some s -> not (String.contains s 'x') in
   if check then
     if !current_file_current_line <> !current_string_start_line then
     failwith (pos2sfull_with start end_ ^ "multi-line patterns are not allowed (or use /x modifier)")
}

let space = [' ' '\t']
let stash = [ '$' '@' '%' '&' '*' ]
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident = ident_start ['0'-'9' 'A'-'Z' 'a'-'z' '_'] *
let pattern_separator = [ '/' '!' ',' '|' ]

rule token = parse
| space+ { 
    (* propagate not_ok_for_match when it was set by the previous token *)
    if lexeme_start lexbuf = !not_ok_for_match then not_ok_for_match := lexeme_end lexbuf; 
   (*SPACE(pos lexbuf) *) token lexbuf
  }
| '#' [^ '\n']* { (*COMMENT(lexeme lexbuf, pos lexbuf)*) token lexbuf }

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
    token lexbuf
  }
| "->" { ARROW }
| "++" { INCR }
| "--" { DECR }
| "**" { POWER }
| "!" { TIGHT_NOT }
| "~" { BIT_NEG }
| "=~" { PATTERN_MATCH }
| "!~" { PATTERN_MATCH_NOT }
| "*" { MULT }
| "%" { MODULO }
| "x" { REPLICATE }
| "+" { PLUS }
| "-" { MINUS }
| "." { CONCAT }
| "<<" { BIT_SHIFT_LEFT }
| ">>" { BIT_SHIFT_RIGHT }
| "<" { LT }
| ">" { GT }
| "<=" | ">=" | "lt" | "gt" | "le" | "ge" { COMPARE_OP(lexeme lexbuf) }
| "==" | "!=" | "<=>" | "eq" | "ne" | "cmp" { EQ_OP(lexeme lexbuf) }
| "&" { BIT_AND }
| "|" { BIT_OR }
| "^" { BIT_XOR }
| "&&" { AND_TIGHT }
| "||" { OR_TIGHT }
| ".." { DOTDOT }
| "..." { DOTDOTDOT }
| "?" { QUESTION_MARK }
| ":" { COLON }
| "::" { PKG_SCOPE }

| "=" | "+=" | "-=" | "*=" | ".=" | "|=" | "&=" | "^=" | "||=" | "&&=" { ASSIGN(lexeme lexbuf) }

| "," { COMMA }
| "=>" { RIGHT_ARROW }
| "not" { NOT }
| "and" { AND }
| "or" { OR }
| "xor" { XOR }

| "if"       { IF }
| "else"     { ELSE }
| "elsif"    { ELSIF }
| "unless"   { UNLESS }
| "do"       { DO }
| "while"    { WHILE }
| "until"    { UNTIL }
| "foreach"  { FOR("foreach") }
| "for"      { FOR("for") }
| "my"       { MY }
| "local"    { LOCAL }
| "continue" { CONTINUE }
| "sub"      { SUB }
| "package"  { PACKAGE }
| "use"      { USE }
| "BEGIN"    { BEGIN }
| "END"      { END }
| "print"    { PRINT(pos lexbuf) }
| "new"      { NEW(pos lexbuf) }
| "format"   { let _ = here_doc_next_line "." false in FORMAT(pos lexbuf) }

| "split"
| "grep"  { (* ok_for_match! *) BAREWORD(lexeme lexbuf, pos lexbuf) }

| ident space* "=>" { (* needed so that (if => 1) works *)
    let s = lexeme lexbuf in
    let end_ = String.length s - 1 in
    let ident_end = rindex_non_spaces_from (end_ - 2) s in
    putback lexbuf (end_ - ident_end); 
    BAREWORD(String.sub s 0 (ident_end+1), pos lexbuf)
  }

| "{" ident "}" { (* needed so that $h{if} works *)
    not_ok_for_match := lexeme_end lexbuf;
    COMPACT_HASH_SUBSCRIPT(lexeme lexbuf, pos lexbuf)
  }

| '@' { AT }
| '$' { DOLLAR }
| '$' '#' { ARRAYLEN }
| '%' ['$' '{'] { putback lexbuf 1; PERCENT }
| '&' ['$' '{'] { putback lexbuf 1; AMPERSAND }
| '*' ['$' '{'] { putback lexbuf 1; if lexeme_start lexbuf = !not_ok_for_match then MULT else STAR }


| ';' { SEMI_COLON }
| '(' { PAREN }
| '{' { BRACKET }
| "+{"{ BRACKET_HASHREF }
| '[' { ARRAYREF }
| ')' { not_ok_for_match := lexeme_end lexbuf; PAREN_END }
| '}' { not_ok_for_match := lexeme_end lexbuf; BRACKET_END }
| ']' { not_ok_for_match := lexeme_end lexbuf; ARRAYREF_END }

| "/" {
    if lexeme_start lexbuf = !not_ok_for_match then DIVISION 
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
    if lexeme_start lexbuf = !not_ok_for_match then ASSIGN(lexeme lexbuf)
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
  let s1, (_, start, _) = ins delimited_string lexbuf in 
  let s2, (_, _, end_)  = ins delimited_string lexbuf in 
  let opts, _ = ins pattern_options lexbuf in
  let pos = !current_file, start, end_ in
  check_multi_line_delimited_string (Some opts) pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}

| "tr" pattern_separator {
  delimit_char := lexeme_char lexbuf 2 ;
  current_string_start_line := !current_file_current_line;
  let s1, (_, start, _) = ins delimited_string lexbuf in 
  let s2, (_, _, end_)  = ins delimited_string lexbuf in 
  let opts, _ = ins pattern_options lexbuf in
  let pos = !current_file, start, end_ in
  check_multi_line_delimited_string None pos ;
  PATTERN_SUBST(s1, s2, opts, pos)
}
    
| "<<" ident { 
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char 2 (lexeme lexbuf)) true)
  }
| "<<'" ident "'" { 
    not_ok_for_match := lexeme_end lexbuf; 
    HERE_DOC(here_doc_next_line (skip_n_char_ 3 1 (lexeme lexbuf)) false)
  }
| "<<" space+ "'"
| "<<" space+ ident { 
    failwith (pos2sfull_with (lexeme_start lexbuf + 2) (lexeme_end lexbuf) ^ "No space allowed between \"<<\" and the marker")
  }
| "<<" space* '"' { 
    failwith (pos2sfull_with (lexeme_start lexbuf + 2) (lexeme_end lexbuf) ^ "Don't use <<\"MARK\", use <<MARK instead")
  }

| "\\" stash
| "\\" ['0'-'9' 'A'-'Z' 'a'-'z']
| "\\" space* '('
    { putback lexbuf 1; REF }

| "sub" space+ ident space* '(' [ '$' '@' '\\' '&' ';' ]* ')' {
    (* bloody prototypes, must be caught especially otherwise "($)" is badly tokenized *)
    (* and alas "($@)" is both valid as an expression and a prototype *)
    let s = lexeme lexbuf in
    let ident_start = index_non_spaces_from 3 s in
      
    let proto_start = String.index_from s ident_start '(' in
    let ident_end = rindex_non_spaces_from (proto_start-1) s in
    let ident = String.sub s ident_start (ident_end - ident_start + 1) in
    let prototype = skip_n_char_ (proto_start + 1) 1 s in

    FUNC_DECL_WITH_PROTO(ident, prototype, pos lexbuf)
  }

| "$#" ident? ("::" ident)+ { arraylen_fqident_from_lexbuf lexbuf }
| "$#" ident { arraylen_ident_from_lexbuf lexbuf }

| stash ident? ("::" ident)+ { typed_fqident_from_lexbuf lexbuf }
| stash ident
| '$' [^ '{' ' ' '\t' '\n' '$']
| "$^" [^ '{' ' ' '\t' '\n'] { typed_ident_from_lexbuf lexbuf }

| "$$" [^ 'a'-'z' 'A'-'Z' '_' '{'] { putback lexbuf 1; SCALAR_IDENT(None, "$$", pos lexbuf) }

| stash "::" { putback lexbuf 2; ident_type_from_char None "main" lexbuf (lexeme_char lexbuf 0) }

| ident? ("::" ident)+ { ident_from_lexbuf lexbuf }
| ident { not_ok_for_match := lexeme_end lexbuf; BAREWORD(lexeme lexbuf, pos lexbuf) }

| ident ":" { LABEL(lexeme lexbuf, pos lexbuf) }

| '-' [ 'a'-'z' 'A'-'Z' ] [ ' ' '\t' '(' ] { putback lexbuf 1; BAREWORD(lexeme lexbuf, pos lexbuf) }

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

| eof   { EOF }
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
  if String.contains s '\t' then failwith(pos2sfull lexbuf ^ "tabulations not accepted in POD commands") else
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
