%{ (* -*- caml -*- *)
  open Types
  open Common
  open Parser_helper

  let parse_error msg = die_rule msg

%}


%token <unit   * (Types.spaces * Types.raw_pos)> EOF
%token <string * (Types.spaces * Types.raw_pos)> NUM STRING BAREWORD BAREWORD_PAREN REVISION COMMENT POD LABEL PRINT_TO_STAR PRINT_TO_SCALAR
%token <string * (Types.spaces * Types.raw_pos)> COMMAND_STRING QUOTEWORDS COMPACT_HASH_SUBSCRIPT

%token <(string * Types.raw_pos) ref * (Types.spaces * Types.raw_pos)> HERE_DOC
%token <(string * string) * (Types.spaces * Types.raw_pos)> PATTERN
%token <(string * string * string) * (Types.spaces * Types.raw_pos)> PATTERN_SUBST

%token <(string option * string) * (Types.spaces * Types.raw_pos)> SCALAR_IDENT ARRAY_IDENT HASH_IDENT FUNC_IDENT STAR_IDENT RAW_IDENT RAW_IDENT_PAREN ARRAYLEN_IDENT
%token <(string * string) * (Types.spaces * Types.raw_pos)> FUNC_DECL_WITH_PROTO

%token <string * (Types.spaces * Types.raw_pos)> FOR PRINT
%token <unit   * (Types.spaces * Types.raw_pos)> NEW FORMAT
%token <string * (Types.spaces * Types.raw_pos)> COMPARE_OP EQ_OP
%token <string * (Types.spaces * Types.raw_pos)> ASSIGN

%token <unit   * (Types.spaces * Types.raw_pos)> IF ELSIF ELSE UNLESS DO WHILE UNTIL MY CONTINUE SUB LOCAL
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
%token <unit   * (Types.spaces * Types.raw_pos)> MULT DIVISION MODULO REPLICATE
%token <unit   * (Types.spaces * Types.raw_pos)> PLUS MINUS CONCAT
%token <unit   * (Types.spaces * Types.raw_pos)> BIT_SHIFT_LEFT BIT_SHIFT_RIGHT
%token <unit   * (Types.spaces * Types.raw_pos)> LT GT
%token <unit   * (Types.spaces * Types.raw_pos)> BIT_AND
%token <unit   * (Types.spaces * Types.raw_pos)> BIT_OR BIT_XOR
%token <unit   * (Types.spaces * Types.raw_pos)> AND_TIGHT
%token <unit   * (Types.spaces * Types.raw_pos)> OR_TIGHT
%token <unit   * (Types.spaces * Types.raw_pos)> DOTDOT DOTDOTDOT
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
%nonassoc   DOTDOT DOTDOTDOT
%left       OR_TIGHT
%left       AND_TIGHT
%left       BIT_OR BIT_XOR
%left       BIT_AND
%nonassoc   EQ_OP
%nonassoc   LT GT COMPARE_OP
%nonassoc   UNIOP
%left       BIT_SHIFT_LEFT BIT_SHIFT_RIGHT
%left       PLUS MINUS CONCAT
%left       MULT DIVISION MODULO REPLICATE
%left       PATTERN_MATCH PATTERN_MATCH_NOT
%right      TIGHT_NOT BIT_NEG REF UNARY_MINUS
%right      POWER
%nonassoc   INCR DECR
%left       ARROW

%nonassoc PAREN_END
%left PAREN PREC_HIGH
%left ARRAYREF BRACKET

%type <Types.fromparser list> prog
%type <Types.fromparser * (Types.spaces * Types.raw_pos)> expr

%start prog


%%
prog: lines EOF { fst $1 }

lines: /* A collection of "lines" in the program */
| {[], (Space_none, bpos)}
| sideff {[fst $1], snd $1}
| line lines {fst $1 :: fst $2, snd $1}

line:
| decl {$1}
| if_then_else {$1}
| loop {$1}
| LABEL {sp_cr($1); Label(fst $1), snd $1}
| semi_colon {List [], snd $1}
| sideff semi_colon {$1}
| BRACKET lines BRACKET_END {sp_p($2); sp_p($3); Block(fst $2), snd $1}

if_then_else: /* Real conditional expressions */
| IF     PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($6); sp_p($7); Call_op("if",     fst $3 :: Block(fst $6) :: fst $8 @ fst $9), snd $1}
| UNLESS PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($6); sp_p($7); Call_op("unless", fst $3 :: Block(fst $6) :: fst $8 @ fst $9), snd $1}

elsif:
|  {[], (Space_none, bpos)}
| ELSIF PAREN expr PAREN_END BRACKET lines BRACKET_END elsif {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($7); fst $3 :: Block(fst $6) :: fst $8, snd $1}

else_: 
|            { [], (Space_none, bpos) }
| ELSE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); sp_p($3); sp_p($4); [ Block(fst $3) ], snd $1 }

loop:
| WHILE PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($6); sp_p($7); Call_op("while", fst $3 :: fst $6), snd $1}
| UNTIL PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($6); sp_p($7); Call_op("until", fst $3 :: fst $6), snd $1}
| FOR MY SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); check_foreach($1); sp_n($4); sp_0($5); sp_0_or_cr($6); sp_p($7); sp_p($8); sp_p($9); Call_op("foreach my", to_Ident $3 :: fst $5 :: fst $8), snd $1}
| FOR SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont { die_rule "don't use for without \"my\"ing the iteration variable" }
| FOR PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); sp_p($6); sp_p($7); check_foreach($1); Call_op("foreach", fst $3 :: fst $6), snd $1}
| FOR PAREN expr_or_empty semi_colon expr_or_empty semi_colon expr_or_empty PAREN_END BRACKET lines BRACKET_END {sp_p($1); sp_n($2); sp_0($3); check_for($1); Call_op("for", fst $3 :: fst $5 :: fst $7 :: fst $10), snd $1}

cont: /* Continue blocks */
|  {(), (Space_none, bpos)}
| CONTINUE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); sp_p($4); (), snd $1}

sideff: /* An expression which may have a side-effect */
| expr  {$1}
| expr   IF    expr {let f = "if"     in sp_p($2); sp_p($3);                    check_no_paren f $3; Call_op(f ^ " infix",     [ fst $1 ; fst $3 ]), snd $1}
| expr UNLESS  expr {let f = "unless" in sp_p($2); sp_p($3);                    check_no_paren f $3; Call_op(f ^ " infix", [ fst $1 ; fst $3 ]), snd $1}
| expr  WHILE  expr {let f = "while"  in sp_p($2); sp_p($3);                    check_no_paren f $3; Call_op(f ^ " infix",  [ fst $1 ; fst $3 ]), snd $1}
| expr  UNTIL  expr {let f = "until"  in sp_p($2); sp_p($3);                    check_no_paren f $3; Call_op(f ^ " infix",  [ fst $1 ; fst $3 ]), snd $1}
| expr  FOR    expr {let f = "for"    in sp_p($2); sp_p($3); check_foreach($2); check_no_paren f $3; Call_op(f ^ " infix",    [ fst $1 ; fst $3 ]), snd $1}

decl:
| FORMAT BAREWORD ASSIGN {Too_complex, snd $1}
| FORMAT ASSIGN {Too_complex, snd $1}
| func_decl semi_colon {die_rule (if snd (fst $1) = "" then "there is no need to pre-declare in Perl!" else "please don't use prototype pre-declaration") }
| func_decl BRACKET BRACKET_END {sp_n($2); sp_0_or_cr($3); let name, proto = fst $1 in sub_declaration (name, proto) [], snd $1}
| func_decl BRACKET lines BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sub_declaration (fst $1) (fst $3), snd $1}
| func_decl BRACKET BRACKET expr BRACKET_END            BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sub_declaration (fst $1) [Ref(I_hash, fst $4)], snd $1}
| func_decl BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($7); sub_declaration (fst $1) [Ref(I_hash, fst $4)], snd $1}
| PACKAGE word semi_colon {sp_0_or_cr($1); Package(fst $2), snd $1}
| BEGIN BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); sp_p($3); sp_p($4); Sub_declaration(Ident(None, "BEGIN", get_pos $1), "", fst $3), snd $1}
| END   BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); sp_p($3); sp_p($4); Sub_declaration(Ident(None, "END",   get_pos $1), "", fst $3), snd $1}
| use {$1}

use:
| use_word listexpr semi_colon {Use(fst $1, fst $2), snd $1}

use_word:
| use_revision word comma {fst $2, snd $1}
| use_revision word {fst $2, snd $1}
| use_revision {Ident(None, "", get_pos $1), snd $1}

use_revision:
| USE REVISION comma {$1}
| USE REVISION {$1}
| USE {$1}

func_decl:
| SUB word {(fst $2, ""), snd $1}
| FUNC_DECL_WITH_PROTO {(Ident(None, fst(fst $1), get_pos $1), snd(fst $1)), snd $1}

listexpr: /* Basic list expressions */
| %prec PREC_LOW {[], (Space_none, bpos)}
| argexpr %prec PREC_LOW {$1}

expr: /* Ordinary expressions; logical combinations */
| expr AND expr {sp_p($2); sp_p($3); Call_op("and", [ fst $1; fst $3 ]), snd $1}
| expr OR  expr {sp_p($2); sp_p($3); Call_op("or",  [ fst $1; fst $3 ]), snd $1}
| argexpr %prec PREC_LOW {List(fst $1), snd $1}

argexpr: /* Expressions are a list of terms joined by commas */
| argexpr comma {fst $1, snd $1}
| argexpr comma term {fst $1 @ [fst $3], snd $1}
| argexpr comma BRACKET expr BRACKET_END {sp_p($3); sp_p($5); fst $1 @ [ Ref(I_hash, fst $4) ], snd $1}
| term %prec PREC_LOW {[fst $1], snd $1}

/********************************************************************************/
term:
| term binop term {Call_op(fst $2, [fst $1 ; fst $3]), snd $1}
| term binop BRACKET expr BRACKET_END {sp_p($3); sp_p($5); Call_op(fst $2, [fst $1 ; Ref(I_hash, fst $4)]), snd $1}
| term LT term {Call_op("<", [fst $1 ; fst $3]), snd $1}
| term GT term {Call_op(">", [fst $1 ; fst $3]), snd $1}

| term PATTERN_MATCH     PATTERN   {Call_op("m//",  fst $1 :: from_PATTERN $3), snd $1}
| term PATTERN_MATCH_NOT PATTERN   {Call_op("!m//", fst $1 :: from_PATTERN $3), snd $1}
| term PATTERN_MATCH PATTERN_SUBST {Call_op("s///", fst $1 :: from_PATTERN_SUBST $3), snd $1}

| term PATTERN_MATCH     scalar { Too_complex, snd $1 }
| term PATTERN_MATCH_NOT scalar { Too_complex, snd $1 }

| term PATTERN_MATCH     STRING {failwith (msg_with_pos (snd (snd $3)) "use a regexp, not a string")}
| term PATTERN_MATCH_NOT STRING {failwith (msg_with_pos (snd (snd $3)) "use a regexp, not a string")}

/* Unary operators and terms */
| MINUS term %prec UNARY_MINUS {Call_op("- unary", [fst $2]), snd $1}
| TIGHT_NOT term {Call_op("not", [fst $2]), snd $1}
| BIT_NEG term {Call_op("~", [fst $2]), snd $1}
| INCR term    {Call_op("++", [fst $2]), snd $1}
| DECR term    {Call_op("--", [fst $2]), snd $1}
| term INCR    {sp_0($2); Call_op("++ post", [fst $1]), snd $1}
| term DECR    {sp_0($2); Call_op("-- post", [fst $1]), snd $1}

| NOT argexpr  {Call_op("not", fst $2), snd $1}


/* Constructors for anonymous data */

| ARRAYREF ARRAYREF_END {sp_0($2); Ref(I_array, List[]), snd $1}
| arrayref_start ARRAYREF_END {Ref(I_array, List(fst $1)), snd $1}
| arrayref_start expr ARRAYREF_END {Ref(I_array, List(fst $1 @ [fst $2])), snd $1}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {Ref(I_array, List(fst $1 @ [Ref(I_hash, fst $3)])), snd $1}

| BRACKET BRACKET_END {Ref(I_hash, List []), snd $1} /* empty hash */
| BRACKET_HASHREF expr BRACKET_END %prec PREC_HIGH {sp_p($3); Ref(I_hash, fst $2), snd $1} /* { foo => "Bar" } */
| SUB BRACKET BRACKET_END %prec PREC_HIGH {sp_n($2); sp_0($3); Anonymous_sub(Block[]), snd $1}
| SUB BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); sp_p($4); Anonymous_sub(Block(fst $3)), snd $1}

| termdo {$1}
| term question_mark_ colon_ { Call_op("?:", [ fst $1 ; fst $2; fst $3]), snd $1}
| REF term { Ref(I_scalar, fst $2), snd $1} /* \$x, \@y, \%z */
| my %prec UNIOP {List(fst $1), snd $1}
| LOCAL term    %prec UNIOP {Local(fst $2), snd $1}

| parenthesized {List(fst $1), snd $1} /* (1, 2) */
| parenthesized arrayref {Deref_with(I_array, List(fst $1), List(fst $2)), snd $1} /* list slice */

| variable {$1}

| subscripted {$1}

| array arrayref { Deref_with(I_array, fst $1, List(fst $2)), snd $1} /* array slice: @array[vals] */
| array BRACKET expr BRACKET_END {sp_0($2); sp_0($4); Deref_with(I_hash, array_ident_to_hash_ident $1, fst $3), snd $1} /* hash slice: @hash{@keys} */


/* function_calls */
| func parenthesized {Call(fst $1, fst $2), snd $1} /* &foo(@args) */
| word argexpr {check_parenthesized_first_argexpr (string_of_Ident (fst $1)) $2; Call(fst $1, fst $2), snd $1} /* foo $a, $b */
| word_paren parenthesized {Call(fst $1, fst $2), snd $1} /* foo(@args) */
| word BRACKET lines BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($4); Call(fst $1, Anonymous_sub(Block(fst $3)) :: fst $5), snd $1} /* map { foo } @bar */
| word BRACKET BRACKET expr BRACKET_END            BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($5); sp_p($6); Call(fst $1, Anonymous_sub(Ref(I_hash, fst $4)) :: fst $7), snd $1} /* map { { foo } } @bar */
| word BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($5); sp_p($7); Call(fst $1, Anonymous_sub(Ref(I_hash, fst $4)) :: fst $8), snd $1} /* map { { foo }; } @bar */

| term ARROW word_or_scalar parenthesized {sp_0($2); Method_call(fst $1, fst $3, fst $4), snd $1} /* $foo->bar(list) */
| term ARROW word_or_scalar {sp_0($2); Method_call(fst $1, fst $3, []), snd $1} /* $foo->bar */

| NEW word listexpr { Method_call(fst $2, Ident(None, "new", get_pos $1), fst $3), snd $1} /* new Class @args */

| PRINT { Call_op("print", var_STDOUT :: [ var_dollar_ ]), snd $1 }
| PRINT argexpr {check_parenthesized_first_argexpr (fst $1) $2; Call_op("print", var_STDOUT :: fst $2), snd $1 }
| PRINT_TO_STAR argexpr { Call_op("print", Deref(I_star,   Ident(None, fst $1, get_pos $1)) :: fst $2), snd $1 }
| PRINT_TO_SCALAR { Call_op("print", var_STDOUT :: [ Deref(I_scalar, Ident(None, fst $1, get_pos $1)) ]), snd $1 }
| PRINT_TO_SCALAR argexpr { Call_op("print", Deref(I_scalar, Ident(None, fst $1, get_pos $1)) :: fst $2), snd $1 }

| word {$1}

| NUM {Num(fst $1, get_pos $1), snd $1}
| STRING {to_String $1, snd $1}
| REVISION {to_String $1, snd $1}
| COMMAND_STRING {Call_op("``", [to_String $1]), snd $1}
| QUOTEWORDS {Call_op("qw", [to_String $1]), snd $1}
| HERE_DOC {String(fst!(fst $1), get_pos $1), snd $1}
| PATTERN {Call_op("m//", var_dollar_ :: from_PATTERN $1), snd $1}
| PATTERN_SUBST {Call_op("s///", var_dollar_ :: from_PATTERN_SUBST $1), snd $1}
| diamond {$1}

diamond:
| LT GT {sp_0($2); Call_op("<>", []), snd $1}
| LT term GT {sp_0($3); Call_op("<>", [fst $2]), snd $1}

subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {sp_0($2); Too_complex, snd $1} /* $foo::{something} */
| scalar bracket_subscript             {Deref_with(I_hash , fst $1, fst      $2), snd $1} /* $foo{bar} */
| scalar arrayref                      {Deref_with(I_array, fst $1, only_one $2), snd $1} /* $array[$element] */
| term ARROW bracket_subscript         {sp_0($2); Deref_with(I_hash , fst $1, fst      $3), snd $1} /* somehref->{bar} */
| term ARROW arrayref                  {sp_0($2); Deref_with(I_array, fst $1, only_one $3), snd $1} /* somearef->[$element] */
| term ARROW parenthesized             {sp_0($2); Deref_with(I_func , fst $1, List(fst $3)), snd $1} /* $subref->(@args) */
| subscripted bracket_subscript        {Deref_with(I_hash , fst $1, fst      $2), snd $1} /* $foo->[bar]{baz} */
| subscripted arrayref                 {Deref_with(I_array, fst $1, only_one $2), snd $1} /* $foo->[$bar][$baz] */
| subscripted parenthesized            {Deref_with(I_func , fst $1, List(fst $2)), snd $1} /* $foo->{bar}(@args) */

arrayref:
| arrayref_start ARRAYREF_END {sp_0($2); fst $1, snd $1}
| arrayref_start expr ARRAYREF_END {sp_0($3); fst $1 @ [fst $2], snd $1}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {sp_p($2); sp_p($4); sp_0($5); fst $1 @ [Ref(I_hash, fst $3)], snd $1}
parenthesized:
| parenthesized_start PAREN_END {sp_0_or_cr($2); fst $1, snd $1}
| parenthesized_start expr PAREN_END {sp_0_or_cr($3); fst $1 @ [fst $2], snd $1}
| parenthesized_start BRACKET expr BRACKET_END PAREN_END {sp_p($4); sp_0_or_cr($5); fst $1 @ [Ref(I_hash, fst $3)], snd $1}

arrayref_start:
| ARRAYREF {[], snd $1}
| arrayref_start BRACKET expr BRACKET_END comma {sp_p($4); fst $1 @ [Ref(I_hash, fst $3)], snd $1}
parenthesized_start:
| PAREN {[], snd $1}
| parenthesized_start BRACKET expr BRACKET_END comma {sp_p($4); fst $1 @ [Ref(I_hash, fst $3)], snd $1}

my: /* Things that can be "my"'d */
| MY parenthesized {List.map (fun e -> My e) (fst $2), snd $1}
| MY scalar {[My(fst $2)], snd $1}
| MY hash {[My(fst $2)], snd $1}
| MY array {[My(fst $2)], snd $1}

termdo: /* Things called with "do" */
| DO term %prec UNIOP { die_rule "\"do EXPR\" not allowed" } /* do $filename */
| DO BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); sp_p($4); Block(fst $3), snd $1} /* do { code */

question_mark_:
| QUESTION_MARK term {sp_n($1); fst $2, snd $1}
| QUESTION_MARK BRACKET expr BRACKET_END {sp_n($1); sp_p($2); sp_p($4); Ref(I_hash, fst $3), snd $1}
colon_:
| COLON term {sp_n($1); fst $2, snd $1}
| COLON BRACKET expr BRACKET_END {sp_n($1); sp_p($2); sp_p($4); Ref(I_hash, fst $3), snd $1}

bracket_subscript:
| BRACKET expr BRACKET_END {sp_0($1); sp_0($3); only_one_in_List $2, snd $1}
| COMPACT_HASH_SUBSCRIPT {sp_0($1); to_String $1, snd $1}

binop:
| ASSIGN {"=", snd $1}
| POWER {"**", snd $1}
| MULT {"*", snd $1} | DIVISION {"/", snd $1} | MODULO {"%", snd $1} | REPLICATE {"x", snd $1}
| PLUS {"+", snd $1} | MINUS {"-", snd $1} | CONCAT {".", snd $1}
| BIT_SHIFT_LEFT {"<<", snd $1} | BIT_SHIFT_RIGHT {">>", snd $1}
| COMPARE_OP {fst $1, snd $1}
| EQ_OP {fst $1, snd $1}
| BIT_AND {"&", snd $1}
| BIT_OR {"|", snd $1} | BIT_XOR {"^", snd $1}
| DOTDOT {"..", snd $1} | DOTDOTDOT {"...", snd $1}
| AND_TIGHT {"&&", snd $1}
| OR_TIGHT {"||", snd $1} | XOR {"xor", snd $1}

variable:
| scalar   %prec PREC_HIGH {$1}
| star     %prec PREC_HIGH {$1}
| hash     %prec PREC_HIGH {$1}
| array    %prec PREC_HIGH {$1}
| arraylen %prec PREC_HIGH {$1} /* $#x, $#{ something } */
| func     %prec PREC_HIGH {$1} /* &foo; */

word:
| bareword { $1 }
| RAW_IDENT { to_Ident $1, snd $1 }

comma: COMMA {$1} | RIGHT_ARROW {sp_p($1); $1}

semi_colon: SEMI_COLON {sp_0($1); $1}

word_or_scalar:
| word      {$1}
| scalar    {$1}
| word_paren {$1}

bareword:
| NEW { Ident(None, "new", get_pos $1), snd $1 }
| FORMAT { Ident(None, "format", get_pos $1), snd $1 }
| BAREWORD { Ident(None, fst $1, get_pos $1), snd $1 }

word_paren:
| BAREWORD_PAREN { Ident(None, fst $1, get_pos $1), snd $1 }
| RAW_IDENT_PAREN { to_Ident $1, snd $1 }

arraylen: ARRAYLEN_IDENT {Deref(I_arraylen, to_Ident $1), snd $1} | ARRAYLEN  scalar {Deref(I_arraylen, fst $2), snd $1} | ARRAYLEN  BRACKET lines BRACKET_END {sp_0($2); Deref(I_arraylen, Block(fst $3)), snd $1}
scalar:   SCALAR_IDENT   {Deref(I_scalar  , to_Ident $1), snd $1} | DOLLAR    scalar {Deref(I_scalar  , fst $2), snd $1} | DOLLAR    BRACKET lines BRACKET_END {sp_0($2); Deref(I_scalar  , Block(fst $3)), snd $1} | DOLLAR BRACKET BRACKET expr BRACKET_END BRACKET_END {sp_0($2); sp_0($3); sp_p($5); sp_0($6); Deref(I_scalar, Ref(I_hash, fst $4)), snd $1}
func:     FUNC_IDENT     {Deref(I_func    , to_Ident $1), snd $1} | AMPERSAND scalar {Deref(I_func    , fst $2), snd $1} | AMPERSAND BRACKET lines BRACKET_END {sp_0($2); Deref(I_func    , Block(fst $3)), snd $1}
array:    ARRAY_IDENT    {Deref(I_array   , to_Ident $1), snd $1} | AT        scalar {Deref(I_array   , fst $2), snd $1} | AT        BRACKET lines BRACKET_END {sp_0($2); Deref(I_array   , Block(fst $3)), snd $1}
hash:     HASH_IDENT     {Deref(I_hash    , to_Ident $1), snd $1} | PERCENT   scalar {Deref(I_hash    , fst $2), snd $1} | PERCENT   BRACKET lines BRACKET_END {sp_0($2); Deref(I_hash    , Block(fst $3)), snd $1}
star:     STAR_IDENT     {Deref(I_star    , to_Ident $1), snd $1} | STAR      scalar {Deref(I_star    , fst $2), snd $1} | STAR      BRACKET lines BRACKET_END {sp_0($2); Deref(I_star    , Block(fst $3)), snd $1}

expr_or_empty: {Block [], (Space_none, bpos)} | expr {$1}
