%{ (* -*- caml -*- *)
  open Types
  open Common
  open Parser_helper

  let parse_error msg = die_rule msg
  let prog_ref = ref None
  let to_String e = Parser_helper.to_String (some !prog_ref) e
  let from_PATTERN e = Parser_helper.from_PATTERN (some !prog_ref) e
  let from_PATTERN_SUBST e = Parser_helper.from_PATTERN_SUBST (some !prog_ref) e
%}


%token <unit   * (Types.spaces * Types.raw_pos)> EOF
%token <string * (Types.spaces * Types.raw_pos)> NUM RAW_STRING BAREWORD BAREWORD_PAREN REVISION COMMENT POD LABEL PERL_CHECKER_COMMENT ONE_SCALAR_PARA
%token <(string * string) * (Types.spaces * Types.raw_pos)> PRINT_TO_STAR PRINT_TO_SCALAR
%token <string * (Types.spaces * Types.raw_pos)> QUOTEWORDS COMPACT_HASH_SUBSCRIPT
%token <(string * Types.raw_pos) * (Types.spaces * Types.raw_pos)> RAW_HERE_DOC
%token <(string * ((int * int) * token) list) list * (Types.spaces * Types.raw_pos)> STRING COMMAND_STRING
%token <((string * ((int * int) * token) list) list * Types.raw_pos) * (Types.spaces * Types.raw_pos)> HERE_DOC

%token <((string * ((int * int) * token) list) list * string) * (Types.spaces * Types.raw_pos)> PATTERN
%token <((string * ((int * int) * token) list) list * (string * ((int * int) * token) list) list * string) * (Types.spaces * Types.raw_pos)> PATTERN_SUBST

%token <(string option * string) * (Types.spaces * Types.raw_pos)> SCALAR_IDENT ARRAY_IDENT HASH_IDENT FUNC_IDENT STAR_IDENT RAW_IDENT RAW_IDENT_PAREN ARRAYLEN_IDENT
%token <(string * string) * (Types.spaces * Types.raw_pos)> FUNC_DECL_WITH_PROTO

%token <string * (Types.spaces * Types.raw_pos)> FOR PRINT
%token <unit   * (Types.spaces * Types.raw_pos)> NEW FORMAT
%token <string * (Types.spaces * Types.raw_pos)> COMPARE_OP EQ_OP
%token <string * (Types.spaces * Types.raw_pos)> ASSIGN MY_OUR

%token <unit   * (Types.spaces * Types.raw_pos)> IF ELSIF ELSE UNLESS DO WHILE UNTIL CONTINUE SUB LOCAL
%token <unit   * (Types.spaces * Types.raw_pos)> USE PACKAGE BEGIN END
%token <unit   * (Types.spaces * Types.raw_pos)> AT DOLLAR PERCENT AMPERSAND STAR ARRAYLEN
%token <unit   * (Types.spaces * Types.raw_pos)> SEMI_COLON PKG_SCOPE
%token <unit   * (Types.spaces * Types.raw_pos)> PAREN PAREN_END
%token <unit   * (Types.spaces * Types.raw_pos)> BRACKET BRACKET_END BRACKET_HASHREF
%token <unit   * (Types.spaces * Types.raw_pos)> ARRAYREF ARRAYREF_END

%token <unit   * (Types.spaces * Types.raw_pos)> ARROW
%token <unit   * (Types.spaces * Types.raw_pos)> INCR DECR
%token <unit   * (Types.spaces * Types.raw_pos)> POWER
%token <unit   * (Types.spaces * Types.raw_pos)> TIGHT_NOT BIT_NEG REF
%token <unit   * (Types.spaces * Types.raw_pos)> PATTERN_MATCH PATTERN_MATCH_NOT
%token <string * (Types.spaces * Types.raw_pos)> MULT
%token <string * (Types.spaces * Types.raw_pos)> PLUS
%token <string * (Types.spaces * Types.raw_pos)> BIT_SHIFT
%token <unit   * (Types.spaces * Types.raw_pos)> LT GT
%token <unit   * (Types.spaces * Types.raw_pos)> BIT_AND
%token <unit   * (Types.spaces * Types.raw_pos)> BIT_OR BIT_XOR
%token <unit   * (Types.spaces * Types.raw_pos)> AND_TIGHT
%token <unit   * (Types.spaces * Types.raw_pos)> OR_TIGHT
%token <string * (Types.spaces * Types.raw_pos)> DOTDOT
%token <unit   * (Types.spaces * Types.raw_pos)> QUESTION_MARK COLON
%token <unit   * (Types.spaces * Types.raw_pos)> COMMA RIGHT_ARROW
%token <unit   * (Types.spaces * Types.raw_pos)> NOT
%token <unit   * (Types.spaces * Types.raw_pos)> AND
%token <unit   * (Types.spaces * Types.raw_pos)> OR XOR

%nonassoc PREC_LOW
%nonassoc LOOPEX

%left       OR XOR
%left       AND
%right      NOT
%nonassoc   LSTOP
%left   COMMA RIGHT_ARROW

%right      ASSIGN
%right      QUESTION_MARK COLON
%nonassoc   DOTDOT
%left       OR_TIGHT
%left       AND_TIGHT
%left       BIT_OR BIT_XOR
%left       BIT_AND
%nonassoc   EQ_OP
%nonassoc   LT GT COMPARE_OP
%nonassoc   UNIOP
%left       BIT_SHIFT
%left       PLUS
%left       MULT
%left       PATTERN_MATCH PATTERN_MATCH_NOT
%right      TIGHT_NOT BIT_NEG REF UNARY_MINUS
%right      POWER
%nonassoc   INCR DECR
%left       ARROW

%nonassoc PAREN_END
%left PAREN PREC_HIGH
%left ARRAYREF BRACKET

%type <Types.fromparser list> prog
%type <(Types.priority * Types.fromparser) * (Types.spaces * Types.raw_pos)> expr term

%start prog


%%
prog: lines EOF {fst $1}

lines: /* A collection of "lines" in the program */
| {[], (Space_none, bpos)}
| sideff {[fst $1], snd $1}
| line lines {fst $1 @ fst $2, sp_pos_range $1 $2}

line:
| decl {[fst $1], snd $1}
| if_then_else {[fst $1], snd $1}
| loop {[fst $1], snd $1}
| LABEL {sp_cr($1); [Label(fst $1)], snd $1}
| PERL_CHECKER_COMMENT {sp_p($1); [Perl_checker_comment(fst $1, get_pos $1)], snd $1}
| semi_colon {[Semi_colon], snd $1}
| sideff semi_colon {[fst $1 ; Semi_colon], snd $1}
| BRACKET lines BRACKET_END {check_block_sub $2 $3; [Block(fst $2)], sp_pos_range $1 $3}

if_then_else: /* Real conditional expressions */
| IF     PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; to_Call_op("if",     prio_lo P_loose $3 :: Block(fst $6) :: fst $8 @ fst $9) (sp_pos_range $1 $9)}
| UNLESS PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; to_Call_op("unless", prio_lo P_loose $3 :: Block(fst $6) :: fst $8 @ fst $9) (sp_pos_range $1 $9)}

elsif:
|  {[], (Space_none, bpos)}
| ELSIF PAREN expr PAREN_END BRACKET lines BRACKET_END elsif {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; prio_lo P_loose $3 :: Block(fst $6) :: fst $8, sp_pos_range $1 $8}

else_: 
|            { [], (Space_none, bpos) }
| ELSE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); check_block_sub $3 $4; [Block(fst $3)], sp_pos_range $1 $4}

loop:
| WHILE PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; to_Call_op("while", [ prio_lo P_loose $3; Block(fst $6) ]) (sp_pos_range $1 $8)}
| UNTIL PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; to_Call_op("until", [ prio_lo P_loose $3; Block(fst $6) ]) (sp_pos_range $1 $8)}
| FOR PAREN expr_or_empty semi_colon expr_or_empty semi_colon expr_or_empty PAREN_END BRACKET lines BRACKET_END {sp_p($1); check_for($1); sp_n($2); sp_0($3); sp_p($5); sp_p($7); sp_0($8); sp_n($9); check_block_sub $10 $11; to_Call_op("for", [ fst $3; fst $5; fst $7; Block(fst $10) ]) (sp_pos_range $1 $11)}
| FOR SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont { die_rule "don't use for without \"my\"ing the iteration variable" }
| FOR PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); check_block_sub $6 $7; check_for_foreach $1 $3; to_Call_op("foreach", [ prio_lo P_loose $3; Block(fst $6) ]) (sp_pos_range $1 $8)}
| for_my lines BRACKET_END cont {check_block_sub $2 $3; to_Call_op("foreach my", fst $1 @ [ Block(fst $2) ]) (sp_pos_range $1 $4)}

for_my:
| FOR MY_OUR SCALAR_IDENT PAREN expr PAREN_END BRACKET {sp_p($1); check_my($2); check_foreach($1); sp_n($4); sp_0($5); sp_0_or_cr($6); sp_p($7); [ My_our(fst $2, [I_scalar, sndfst $3], get_pos $3); prio_lo P_loose $5 ], sp_pos_range $1 $7}


cont: /* Continue blocks */
|  {(), (Space_none, bpos)}
| CONTINUE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); check_block_sub $3 $4; (), sp_pos_range $1 $4}

sideff: /* An expression which may have a side-effect */
| expr  {sndfst $1, snd $1}
| expr   IF    expr {sp_p($2); sp_p($3);                    call_op_if_infix         (prio_lo P_loose $1) (prio_lo P_loose $3)   (sp_pos_range $1 $3)}
| expr UNLESS  expr {sp_p($2); sp_p($3);                    call_op_unless_infix     (prio_lo P_loose $1) (prio_lo P_loose $3)   (sp_pos_range $1 $3)}
| expr  WHILE  expr {sp_p($2); sp_p($3);                    to_Call_op("while infix" , [ prio_lo P_loose $1 ; prio_lo P_loose $3 ]) (sp_pos_range $1 $3)}
| expr  UNTIL  expr {sp_p($2); sp_p($3);                    to_Call_op("until infix" , [ prio_lo P_loose $1 ; prio_lo P_loose $3 ]) (sp_pos_range $1 $3)}
| expr  FOR    expr {sp_p($2); sp_p($3); check_foreach($2); to_Call_op("for infix"   , [ prio_lo P_loose $1 ; prio_lo P_loose $3 ]) (sp_pos_range $1 $3)}

decl:
| FORMAT BAREWORD ASSIGN {Too_complex, sp_pos_range $1 $3}
| FORMAT ASSIGN {Too_complex, sp_pos_range $1 $2}
| func_decl semi_colon {die_rule (if sndfst $1 = "" then "there is no need to pre-declare in Perl!" else "please don't use prototype pre-declaration") }
| func_decl BRACKET BRACKET_END {sp_n($2); sp_0_or_cr($3); let name, proto = fst $1 in sub_declaration (name, proto) [], sp_pos_range $1 $3}
| func_decl BRACKET lines BRACKET_END {sp_n($2); check_block_sub $3 $4; sub_declaration (fst $1) (fst $3), sp_pos_range $1 $4}
| func_decl BRACKET BRACKET expr BRACKET_END            BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sub_declaration (fst $1) [Ref(I_hash, prio_lo P_loose $4)], sp_pos_range $1 $6}
| func_decl BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($7); sub_declaration (fst $1) [Ref(I_hash, prio_lo P_loose $4); Semi_colon], sp_pos_range $1 $7}
| PACKAGE word semi_colon {sp_0_or_cr($1); sp_1($2); Package(fst $2), sp_pos_range $1 $3}
| BEGIN BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); check_block_sub $3 $4; Sub_declaration(Ident(None, "BEGIN", get_pos $1), "", Block(fst $3)), sp_pos_range $1 $4}
| END   BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); check_block_sub $3 $4; Sub_declaration(Ident(None, "END",   get_pos $1), "", Block(fst $3)), sp_pos_range $1 $4}
| use {$1}

use:
| use_word listexpr semi_colon {sp_n($2); Use(fst $1, sndfst $2), sp_pos_range $1 $3}

use_word:
| use_revision word comma {fst $2, sp_pos_range $1 $3}
| use_revision word {fst $2, sp_pos_range $1 $2}
| use_revision {Ident(None, "", get_pos $1), snd $1}

use_revision:
| USE REVISION comma {$1}
| USE REVISION {$1}
| USE {$1}

func_decl:
| SUB word {(fst $2, ""), sp_pos_range $1 $2}
| FUNC_DECL_WITH_PROTO {(Ident(None, fstfst $1, get_pos $1), sndfst $1), snd $1}

listexpr: /* Basic list expressions */
| %prec PREC_LOW {(P_tok, []), (Space_none, bpos)}
| argexpr %prec PREC_LOW {$1}

expr: /* Ordinary expressions; logical combinations */
| expr AND expr {sp_p($2); sp_p($3); to_Call_op_(P_and, "and", [ prio_lo P_and $1; prio_lo_after P_and $3 ]) (sp_pos_range $1 $3)}
| expr OR  expr {sp_p($2); sp_p($3); to_Call_op_(P_or,  "or",  [ prio_lo P_or  $1; prio_lo_after P_or  $3 ]) (sp_pos_range $1 $3)}
| argexpr %prec PREC_LOW {(fstfst $1, List(sndfst $1)), snd $1}

argexpr: /* Expressions are a list of terms joined by commas */
| argexpr comma {(P_comma, sndfst $1), sp_pos_range $1 $2}
| argexpr comma term {if not_simple (sndfst $3) then sp_p($3); (P_comma, followed_by_comma $1 $2 @ [sndfst $3]), sp_pos_range $1 $3}
| argexpr comma BRACKET expr BRACKET_END {sp_p($3); sp_p($5); (P_comma, followed_by_comma $1 $2 @ [ Ref(I_hash, sndfst $4) ]), sp_pos_range $1 $5}
| term %prec PREC_LOW {(fstfst $1, [sndfst $1]), snd $1}

/********************************************************************************/
term:
| term ASSIGN     term {let pri = P_assign    in call_op_(op   pri (fst $2) $2, $3, [sndfst      $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term PLUS       term {let pri = P_add       in call_op_(op   pri (fst $2) $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term COMPARE_OP term {let pri = P_cmp       in call_op_(op_p pri (fst $2) $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term LT         term {let pri = P_cmp       in call_op_(op_p pri "<"      $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term GT         term {let pri = P_cmp       in call_op_(op_p pri ">"      $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term EQ_OP      term {let pri = P_eq        in call_op_(op_p pri (fst $2) $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term POWER      term {let pri = P_tight     in call_op_(op   pri "**"     $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term BIT_AND    term {let pri = P_expr      in call_op_(op_p pri "&"      $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term BIT_OR     term {let pri = P_expr      in call_op_(op   pri "|"      $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term BIT_XOR    term {let pri = P_expr      in call_op_(op_p pri "^"      $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term AND_TIGHT  term {let pri = P_tight_and in call_op_(op_p pri "&&"     $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term OR_TIGHT   term {let pri = P_tight_or  in call_op_(op_p pri "||"     $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term MULT       term {let pri = P_mul       in call_op_(op   pri (fst $2) $2, $3, [prio_lo_concat $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term DOTDOT     term {let pri = P_paren_wanted P_expr  in call_op_(op   pri (fst $2) $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term BIT_SHIFT  term {let pri = P_paren_wanted P_tight in call_op_(op   pri (fst $2) $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}
| term XOR        term {let pri = P_paren_wanted P_expr  in call_op_(op_p pri "xor"    $2, $3, [prio_lo pri $1; prio_lo_after pri $3]) (sp_pos_range $1 $3)}

| term ASSIGN     BRACKET expr BRACKET_END {sp_p($3); sp_p($4); sp_p($5); call_op_(op_p P_assign (fst $2) $2, $3, [prio_lo P_assign    $1; Ref(I_hash, sndfst $4)]) (sp_pos_range $1 $5)}
| term AND_TIGHT  BRACKET expr BRACKET_END {sp_p($3); sp_p($4); sp_p($5); call_op_(op_p P_tight_and "&&"  $2, $3, [prio_lo P_assign    $1; Ref(I_hash, sndfst $4)]) (sp_pos_range $1 $5)}
| term OR_TIGHT   BRACKET expr BRACKET_END {sp_p($3); sp_p($4); sp_p($5); call_op_(op_p P_tight_or  "||"  $2, $3, [prio_lo P_assign    $1; Ref(I_hash, sndfst $4)]) (sp_pos_range $1 $5)}


| term PATTERN_MATCH     PATTERN   {sp_n($2); sp_p($3); check_unneeded_var_dollar_   ($1); to_Call_op_(P_expr, "m//",  sndfst $1 :: from_PATTERN $3) (sp_pos_range $1 $3)}
| term PATTERN_MATCH_NOT PATTERN   {sp_n($2); sp_p($3); check_unneeded_var_dollar_not($1); to_Call_op_(P_expr, "!m//", sndfst $1 :: from_PATTERN $3) (sp_pos_range $1 $3)}
| term PATTERN_MATCH PATTERN_SUBST {sp_n($2); sp_p($3); check_unneeded_var_dollar_s  ($1); to_Call_op_(P_expr, "s///", sndfst $1 :: from_PATTERN_SUBST $3) (sp_pos_range $1 $3)}
| term PATTERN_MATCH_NOT PATTERN_SUBST {die_with_rawpos (sndsnd $2) "use =~ instead of !~ and negate the return value"}

| term PATTERN_MATCH     scalar { (P_expr, Too_complex), sp_pos_range $1 $3}
| term PATTERN_MATCH_NOT scalar { (P_expr, Too_complex), sp_pos_range $1 $3}

| term PATTERN_MATCH     RAW_STRING {die_with_rawpos (sndsnd $3) "use a regexp, not a string"}
| term PATTERN_MATCH_NOT RAW_STRING {die_with_rawpos (sndsnd $3) "use a regexp, not a string"}
| term PATTERN_MATCH     STRING {die_with_rawpos (sndsnd $3) "use a regexp, not a string"}
| term PATTERN_MATCH_NOT STRING {die_with_rawpos (sndsnd $3) "use a regexp, not a string"}


| term QUESTION_MARK term COLON term {sp_p($2); sp_p($3); sp_p($4); sp_p($5); to_Call_op_(P_ternary, "?:", [ prio_lo P_ternary $1 ; prio_lo_after P_ternary $3; prio_lo_after P_ternary $5]) (sp_pos_range $1 $5)}
| term QUESTION_MARK term COLON BRACKET expr BRACKET_END {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); to_Call_op_(P_ternary, "?:", [ prio_lo P_ternary $1 ; prio_lo_after P_ternary $3; sndfst $6]) (sp_pos_range $1 $7)}
| term QUESTION_MARK BRACKET expr BRACKET_END COLON term {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); to_Call_op_(P_ternary, "?:", [ prio_lo P_ternary $1 ; sndfst $4; prio_lo_after P_ternary $7]) (sp_pos_range $1 $7)}
| term QUESTION_MARK BRACKET expr BRACKET_END COLON BRACKET expr BRACKET_END {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); sp_p($8); sp_p($9); to_Call_op_(P_ternary, "?:", [ prio_lo P_ternary $1 ; sndfst $4; sndfst $8]) (sp_pos_range $1 $9)}


/* Unary operators and terms */
| PLUS term %prec UNARY_MINUS {if fst $1 <> "-" then die_rule "syntax error"; sp_0($2); to_Call_op_(P_tight, "- unary", [sndfst $2]) (sp_pos_range $1 $2)}
| TIGHT_NOT term {to_Call_op_(P_tight, "not", [sndfst $2]) (sp_pos_range $1 $2)}
| BIT_NEG term {to_Call_op_(P_expr, "~", [sndfst $2]) (sp_pos_range $1 $2)}
| INCR term    {sp_0($2); to_Call_op_(P_tight, "++", [sndfst $2]) (sp_pos_range $1 $2)}
| DECR term    {sp_0($2); to_Call_op_(P_tight, "--", [sndfst $2]) (sp_pos_range $1 $2)}
| term INCR    {sp_0($2); to_Call_op_(P_tight, "++ post", [sndfst $1]) (sp_pos_range $1 $2)}
| term DECR    {sp_0($2); to_Call_op_(P_tight, "-- post", [sndfst $1]) (sp_pos_range $1 $2)}
| NOT argexpr  {to_Call_op_(P_and, "not", sndfst $2) (sp_pos_range $1 $2)}

| ONE_SCALAR_PARA RAW_STRING               {call_one_scalar_para $1 [to_Raw_string $2], sp_pos_range $1 $2}
| ONE_SCALAR_PARA STRING                   {call_one_scalar_para $1 [to_String true $2], sp_pos_range $1 $2}
| ONE_SCALAR_PARA variable                 {call_one_scalar_para $1 [fst $2], sp_pos_range $1 $2}
| ONE_SCALAR_PARA restricted_subscripted   {call_one_scalar_para $1 [fst $2], sp_pos_range $1 $2}
| ONE_SCALAR_PARA parenthesized            {call_one_scalar_para $1 (sndfst $2), sp_pos_range $1 $2}
| ONE_SCALAR_PARA word_paren parenthesized {call_one_scalar_para $1 [call(Deref(I_func, fst $2), sndfst $3)], sp_pos_range $1 $3}

/* Constructors for anonymous data */

| ARRAYREF ARRAYREF_END {sp_0($2); (P_expr, Ref(I_array, List[])), sp_pos_range $1 $2}
| arrayref_start ARRAYREF_END {(if fst $1 = [] then sp_0 else sp_p)($2) ; (P_expr, Ref(I_array, List(fst $1))), sp_pos_range $1 $2}
| arrayref_start expr ARRAYREF_END {sp_same $2 $3; (P_expr, Ref(I_array, List(fst $1 @ [sndfst $2]))), sp_pos_range $1 $3}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {sp_same $2 $5; (P_expr, Ref(I_array, List(fst $1 @ [Ref(I_hash, sndfst $3)]))), sp_pos_range $1 $5}

| BRACKET BRACKET_END {(P_expr, Ref(I_hash, List [])), sp_pos_range $1 $2} /* empty hash */
| BRACKET_HASHREF expr BRACKET_END %prec PREC_HIGH {sp_p($3); (P_expr, Ref(I_hash, sndfst $2)), sp_pos_range $1 $3} /* { foo => "Bar" } */
| SUB BRACKET BRACKET_END %prec PREC_HIGH {sp_n($2); sp_0($3); (P_expr, anonymous_sub []), sp_pos_range $1 $3}
| SUB BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); check_block_sub $3 $4; (P_expr, anonymous_sub(fst $3)), sp_pos_range $1 $4}

| termdo {(P_tok, fst $1), snd $1}
| REF term {(P_expr, Ref(I_scalar, sndfst $2)), sp_pos_range $1 $2} /* \$x, \@y, \%z */
| my_our %prec UNIOP {(P_expr, fst $1), snd $1}
| LOCAL term    %prec UNIOP {sp_n($2); (P_expr, to_Local $2), sp_pos_range $1 $2}

| parenthesized {(fstfst $1, List(sndfst $1)), snd $1} /* (1, 2) */
| parenthesized arrayref {sp_0($2); (P_tok, to_Deref_with(I_array, (if is_only_one_in_List(fst $2) then I_scalar else I_array), List(sndfst $1), List(fst $2))), sp_pos_range $1 $2} /* list indexing or slicing */

| variable {(P_tok, fst $1), snd $1}

| subscripted {(P_tok, fst $1), snd $1}

| array arrayref {(P_expr, to_Deref_with(I_array, I_array, from_array $1, List(fst $2))), sp_pos_range $1 $2} /* array slice: @array[vals] */
| array BRACKET expr BRACKET_END {sp_0($2); sp_0($3); sp_0($4); (P_expr, to_Deref_with(I_hash, I_array, from_array $1, sndfst $3)), sp_pos_range $1 $4} /* hash slice: @hash{@keys} */


/* function_calls */
| func parenthesized {sp_0($2); (P_tok, call_func true (fst $1, sndfst $2)), sp_pos_range $1 $2} /* &foo(@args) */
| word argexpr {check_parenthesized_first_argexpr (string_of_Ident (fst $1)) $2; (P_call_no_paren, call(Deref(I_func, fst $1), sndfst $2)), sp_pos_range $1 $2} /* foo $a, $b */
| word_paren parenthesized {sp_0($2); (P_tok, call(Deref(I_func, fst $1), sndfst $2)), sp_pos_range $1 $2} /* foo(@args) */
| word BRACKET lines BRACKET_END listexpr %prec LSTOP {sp_n($2); check_block_sub $3 $4; ((if sndfst $5 = [] then P_tok else P_call_no_paren), call(Deref(I_func, fst $1), anonymous_sub(fst $3) :: sndfst $5)), sp_pos_range $1 $5} /* map { foo } @bar */
| word BRACKET BRACKET expr BRACKET_END            BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); ((if sndfst $7 = [] then P_tok else P_call_no_paren), call(Deref(I_func, fst $1), anonymous_sub [ Ref(I_hash, sndfst $4) ] :: sndfst $7)), sp_pos_range $1 $7} /* map { { foo } } @bar */
| word BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($7); ((if sndfst $8 = [] then P_tok else P_call_no_paren), call(Deref(I_func, fst $1), anonymous_sub [ Ref(I_hash, sndfst $4); Semi_colon ] :: sndfst $8)), sp_pos_range $1 $8} /* map { { foo }; } @bar */

| term ARROW word_or_scalar parenthesized {sp_0($2); sp_0($3); sp_0($4); (P_tok, to_Method_call(sndfst $1, fst $3, sndfst $4)), sp_pos_range $1 $4} /* $foo->bar(list) */
| term ARROW word_or_scalar {sp_0($2); sp_0($3); (P_tok, to_Method_call(sndfst $1, fst $3, [])), sp_pos_range $1 $3} /* $foo->bar */
| term ARROW MULT parenthesized {check_MULT_is_x $3; sp_0($2); sp_0($3); sp_0($4); (P_tok, to_Method_call(sndfst $1, Ident(None, "x", get_pos $3), sndfst $4)), sp_pos_range $1 $4} /* $foo->bar(list) */
| term ARROW MULT {check_MULT_is_x $3; sp_0($2); sp_0($3); (P_tok, to_Method_call(sndfst $1, Ident(None, "x", get_pos $3), [])), sp_pos_range $1 $3} /* $foo->bar */

| NEW word { sp_n($2); (P_call_no_paren, to_Method_call(fst $2, Ident(None, "new", get_pos $1), [])), sp_pos_range $1 $2} /* new Class */
| NEW word_paren parenthesized { sp_n($2); sp_0($3); (P_call_no_paren, to_Method_call(fst $2, Ident(None, "new", get_pos $1), sndfst $3)), sp_pos_range $1 $3} /* new Class(...) */
| NEW word terminal { die_rule "you must parenthesize parameters: \"new Class(...)\" instead of \"new Class ...\"" }
| NEW word variable { die_rule "you must parenthesize parameters: \"new Class(...)\" instead of \"new Class ...\"" }

| PRINT { to_Call_op_(P_call_no_paren, fst $1, var_STDOUT :: [ var_dollar_ ]) (snd $1)}
| PRINT argexpr {check_parenthesized_first_argexpr (fst $1) $2; to_Call_op_(P_call_no_paren, fst $1, var_STDOUT :: sndfst $2) (sp_pos_range $1 $2)}
| PRINT_TO_SCALAR         { to_Call_op_(P_call_no_paren, fstfst $1, var_STDOUT :: [ Deref(I_scalar, Ident(None, sndfst $1, get_pos $1)) ]) (snd $1)}
| PRINT_TO_SCALAR argexpr { to_Call_op_(P_call_no_paren, fstfst $1, Deref(I_scalar, Ident(None, sndfst $1, get_pos $1)) :: sndfst $2) (sp_pos_range $1 $2)}
| PRINT_TO_STAR           { to_Call_op_(P_call_no_paren, fstfst $1, Deref(I_star, Ident(None, sndfst $1, get_pos $1)) :: [ var_dollar_ ]) (snd $1)}
| PRINT_TO_STAR argexpr   { to_Call_op_(P_call_no_paren, fstfst $1, Deref(I_star, Ident(None, sndfst $1, get_pos $1)) :: sndfst $2) (sp_pos_range $1 $2)}

| hash PKG_SCOPE {sp_0($2); (P_tok, Too_complex), sp_pos_range $1 $2} /* %main:: */

| terminal {$1}

terminal:
| word {(P_tok, check_word_alone $1), snd $1}
| NUM {(P_tok, Num(fst $1, get_pos $1)), snd $1}
| STRING {(P_tok, to_String true $1), snd $1}
| RAW_STRING {(P_tok, to_Raw_string $1), snd $1}
| REVISION {(P_tok, to_Raw_string $1), snd $1}
| COMMAND_STRING {to_Call_op_(P_expr, "``", [to_String false $1]) (snd $1)}
| QUOTEWORDS {to_Call_op_(P_tok, "qw", [to_Raw_string $1]) (snd $1)}
| HERE_DOC {(P_tok, String([], raw_pos2pos (sndfst $1))), snd $1}
| RAW_HERE_DOC {(P_tok, Raw_string(fstfst $1, raw_pos2pos (sndfst $1))), snd $1}
| PATTERN {to_Call_op_(P_expr, "m//", var_dollar_ :: from_PATTERN $1) (snd $1)}
| PATTERN_SUBST {to_Call_op_(P_expr, "s///", var_dollar_ :: from_PATTERN_SUBST $1) (snd $1)}
| diamond {(P_expr, fst $1), snd $1}

diamond:
| LT GT {sp_0($2); to_Call_op("<>", []) (sp_pos_range $1 $2)}
| LT term GT {sp_0($2); sp_0($3); to_Call_op("<>", [sndfst $2]) (sp_pos_range $1 $3)}

subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {sp_0($2); sp_0($3); Too_complex, sp_pos_range $1 $3} /* $foo::{something} */
| scalar bracket_subscript             {sp_0($2); to_Deref_with(I_hash , I_scalar, from_scalar $1, fst      $2), sp_pos_range $1 $2} /* $foo{bar} */
| scalar arrayref                      {sp_0($2); to_Deref_with(I_array, I_scalar, from_scalar $1, only_one $2), sp_pos_range $1 $2} /* $array[$element] */
| term ARROW bracket_subscript         {sp_0($2); sp_0($3); check_arrow_needed $1 $2; to_Deref_with(I_hash , I_scalar, sndfst $1, fst      $3), sp_pos_range $1 $3} /* somehref->{bar} */
| term ARROW arrayref                  {sp_0($2); sp_0($3); check_arrow_needed $1 $2; to_Deref_with(I_array, I_scalar, sndfst $1, only_one $3), sp_pos_range $1 $3} /* somearef->[$element] */
| term ARROW parenthesized             {sp_0($2); sp_0($3); to_Deref_with(I_func , I_scalar, sndfst $1, List(sndfst $3)), sp_pos_range $1 $3} /* $subref->(@args) */
| subscripted bracket_subscript        {sp_0($2); to_Deref_with(I_hash , I_scalar, fst $1, fst      $2), sp_pos_range $1 $2} /* $foo->[bar]{baz} */
| subscripted arrayref                 {sp_0($2); to_Deref_with(I_array, I_scalar, fst $1, only_one $2), sp_pos_range $1 $2} /* $foo->[$bar][$baz] */
| subscripted parenthesized            {sp_0($2); to_Deref_with(I_func , I_scalar, fst $1, List(sndfst $2)), sp_pos_range $1 $2} /* $foo->{bar}(@args) */

restricted_subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {sp_0($2); sp_0($3); Too_complex, sp_pos_range $1 $3} /* $foo::{something} */
| scalar bracket_subscript             {sp_0($2); to_Deref_with(I_hash , I_scalar, from_scalar $1, fst      $2), sp_pos_range $1 $2} /* $foo{bar} */
| scalar arrayref                      {sp_0($2); to_Deref_with(I_array, I_scalar, from_scalar $1, only_one $2), sp_pos_range $1 $2} /* $array[$element] */
| restricted_subscripted bracket_subscript        {sp_0($2); to_Deref_with(I_hash , I_scalar, fst $1, fst      $2), sp_pos_range $1 $2} /* $foo->[bar]{baz} */
| restricted_subscripted arrayref                 {sp_0($2); to_Deref_with(I_array, I_scalar, fst $1, only_one $2), sp_pos_range $1 $2} /* $foo->[$bar][$baz] */
| restricted_subscripted parenthesized            {sp_0($2); to_Deref_with(I_func , I_scalar, fst $1, List(sndfst $2)), sp_pos_range $1 $2} /* $foo->{bar}(@args) */

arrayref:
| arrayref_start ARRAYREF_END {sp_0($2); fst $1, sp_pos_range $1 $2}
| arrayref_start expr ARRAYREF_END {sp_0($3); fst $1 @ [sndfst $2], sp_pos_range $1 $3}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {sp_p($2); sp_p($4); sp_0($5); fst $1 @ [Ref(I_hash, sndfst $3)], sp_pos_range $1 $5}
parenthesized:
| parenthesized_start PAREN_END {sp_0_or_cr($2); ((if fst $1 = [] then P_tok else P_paren P_comma), fst $1), sp_pos_range $1 $2}
| parenthesized_start expr PAREN_END {sp_0_or_cr($3); (P_paren(if fst $1 = [] then fstfst $2 else P_comma), fst $1 @ [(if fst $1 = [] then prio_lo P_loose else prio_lo_after P_comma) $2]), sp_pos_range $1 $3}
| parenthesized_start BRACKET expr BRACKET_END PAREN_END {sp_p($4); sp_0_or_cr($5); (P_paren(if fst $1 = [] then P_expr else P_comma), fst $1 @ [Ref(I_hash, sndfst $3)]), sp_pos_range $1 $5}

arrayref_start:
| ARRAYREF {[], snd $1}
| arrayref_start BRACKET expr BRACKET_END comma {sp_p($2); sp_p($3); sp_p($4); fst $1 @ [Ref(I_hash, sndfst $3)], sp_pos_range $1 $5}
parenthesized_start:
| PAREN {[], snd $1}
| parenthesized_start BRACKET expr BRACKET_END comma {(if fst $1 = [] then sp_0_or_cr else sp_p)($2); sp_p($3); sp_p($4); fst $1 @ [Ref(I_hash, sndfst $3)], sp_pos_range $1 $5}

my_our: /* Things that can be "my"'d */
| my_our_paren PAREN_END {sp_0($2); if sndfst $1 <> [] && fst (fstfst $1) then die_rule "syntax error";    My_our(snd (fstfst $1), sndfst $1, get_pos $1), sp_pos_range $1 $2}
| my_our_paren SCALAR_IDENT PAREN_END {(if sndfst $1 = [] then sp_0 else sp_1)($2); check_my_our_paren $1; My_our(snd (fstfst $1), sndfst $1 @ [I_scalar, sndfst $2], pos_range $1 $3), sp_pos_range $1 $3}
| my_our_paren HASH_IDENT   PAREN_END {(if sndfst $1 = [] then sp_0 else sp_1)($2); check_my_our_paren $1; My_our(snd (fstfst $1), sndfst $1 @ [I_hash,   sndfst $2], pos_range $1 $3), sp_pos_range $1 $3}
| my_our_paren ARRAY_IDENT  PAREN_END {(if sndfst $1 = [] then sp_0 else sp_1)($2); check_my_our_paren $1; My_our(snd (fstfst $1), sndfst $1 @ [I_array,  sndfst $2], pos_range $1 $3), sp_pos_range $1 $3}
| MY_OUR SCALAR_IDENT {My_our(fst $1, [I_scalar, sndfst $2], get_pos $2), sp_pos_range $1 $2}
| MY_OUR HASH_IDENT   {My_our(fst $1, [I_hash,   sndfst $2], get_pos $2), sp_pos_range $1 $2}
| MY_OUR ARRAY_IDENT  {My_our(fst $1, [I_array,  sndfst $2], get_pos $2), sp_pos_range $1 $2}

my_our_paren:
| MY_OUR PAREN {sp_1($2); ((true, fst $1), []), sp_pos_range $1 $2}
| my_our_paren comma {if fst (fstfst $1) then die_rule "syntax error"; ((true, snd (fstfst $1)), sndfst $1), sp_pos_range $1 $2}
| my_our_paren BAREWORD {check_my_our_paren $1; if fst $2 <> "undef" then die_rule "scalar expected"; ((false, snd (fstfst $1)), sndfst $1 @ [I_raw, fst $2]), sp_pos_range $1 $2}
| my_our_paren SCALAR_IDENT {check_my_our_paren $1; ((false, snd (fstfst $1)), sndfst $1 @ [I_scalar, sndfst $2]), sp_pos_range $1 $2}
| my_our_paren HASH_IDENT   {check_my_our_paren $1; ((false, snd (fstfst $1)), sndfst $1 @ [I_hash,   sndfst $2]), sp_pos_range $1 $2}
| my_our_paren ARRAY_IDENT  {check_my_our_paren $1; ((false, snd (fstfst $1)), sndfst $1 @ [I_array,  sndfst $2]), sp_pos_range $1 $2}

termdo: /* Things called with "do" */
| DO term %prec UNIOP { die_rule "\"do EXPR\" not allowed" } /* do $filename */
| DO BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); check_block_sub $3 $4; Block(fst $3), sp_pos_range $1 $4} /* do { code */

bracket_subscript:
| BRACKET expr BRACKET_END {sp_0($1); sp_same $2 $3; check_hash_subscript $2; only_one_in_List $2, sp_pos_range $1 $3}
| COMPACT_HASH_SUBSCRIPT {sp_0($1); to_Raw_string $1, snd $1}

variable:
| scalar   %prec PREC_HIGH {$1}
| star     %prec PREC_HIGH {$1}
| hash     %prec PREC_HIGH {$1}
| array    %prec PREC_HIGH {$1}
| arraylen %prec PREC_HIGH {$1} /* $#x, $#{ something } */
| func     %prec PREC_HIGH {$1} /* &foo; */

word:
| bareword { $1 }
| RAW_IDENT { to_Ident $1, snd $1}

comma: COMMA {true, snd $1} | RIGHT_ARROW {sp_p($1); false, snd $1}

semi_colon: SEMI_COLON {sp_0($1); $1}

word_or_scalar:
| word      {$1}
| scalar    {$1}
| word_paren {$1}

bareword:
| NEW { Ident(None, "new", get_pos $1), snd $1}
| FORMAT { Ident(None, "format", get_pos $1), snd $1}
| BAREWORD { Ident(None, fst $1, get_pos $1), snd $1}

word_paren:
| BAREWORD_PAREN { Ident(None, fst $1, get_pos $1), snd $1}
| RAW_IDENT_PAREN { to_Ident $1, snd $1}

arraylen: ARRAYLEN_IDENT {deref_arraylen (to_Ident $1), snd $1} | ARRAYLEN  scalar {sp_0($2); deref_arraylen (fst $2), snd $1} | ARRAYLEN  bracket_subscript {deref_arraylen (fst $2), sp_pos_range $1 $2}
scalar:   SCALAR_IDENT   {Deref(I_scalar, to_Ident $1), snd $1} | DOLLAR    scalar {sp_0($2); Deref(I_scalar, fst $2), snd $1} | DOLLAR    bracket_subscript {Deref(I_scalar, fst $2), sp_pos_range $1 $2} | DOLLAR BRACKET BRACKET expr BRACKET_END BRACKET_END {sp_0($2); sp_0($3); sp_p($5); sp_0($6); Deref(I_scalar, Ref(I_hash, sndfst $4)), sp_pos_range $1 $6}
func:     FUNC_IDENT     {Deref(I_func  , to_Ident $1), snd $1} | AMPERSAND scalar {sp_0($2); Deref(I_func  , fst $2), snd $1} | AMPERSAND bracket_subscript {Deref(I_func  , fst $2), sp_pos_range $1 $2}
array:    ARRAY_IDENT    {Deref(I_array , to_Ident $1), snd $1} | AT        scalar {sp_0($2); Deref(I_array , fst $2), snd $1} | AT        bracket_subscript {Deref(I_array , fst $2), sp_pos_range $1 $2}
hash:     HASH_IDENT     {Deref(I_hash  , to_Ident $1), snd $1} | PERCENT   scalar {sp_0($2); Deref(I_hash  , fst $2), snd $1} | PERCENT   bracket_subscript {Deref(I_hash  , fst $2), sp_pos_range $1 $2}
star:     STAR_IDENT     {Deref(I_star  , to_Ident $1), snd $1} | STAR      scalar {sp_0($2); Deref(I_star  , fst $2), snd $1} | STAR      bracket_subscript {Deref(I_star  , fst $2), sp_pos_range $1 $2}

expr_or_empty: {Block [], (Space_none, bpos)} | expr {sndfst $1, snd $1}

%%

;;
prog_ref := Some prog
;;
