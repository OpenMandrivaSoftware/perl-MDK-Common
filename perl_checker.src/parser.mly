%{ (* -*- caml -*- *)
  open Types
  open Common

  let die msg =
      failwith (Info.pos2sfull_current (Parsing.symbol_start()) (Parsing.symbol_end()) ^ msg)

  let parse_error msg = die msg

  let warn msg = if false then prerr_endline msg

  let to_Ident = function
   | BAREWORD(name, pos) -> Ident(I_raw, None, name, pos)
   | SCALAR_IDENT(fq, name, pos) -> Ident(I_scalar, fq, name, pos)
   | ARRAY_IDENT (fq, name, pos) -> Ident(I_array,  fq, name, pos)
   | HASH_IDENT  (fq, name, pos) -> Ident(I_hash,   fq, name, pos)
   | FUNC_IDENT  (fq, name, pos) -> Ident(I_func,   fq, name, pos)
   | STAR_IDENT  (fq, name, pos) -> Ident(I_star,   fq, name, pos)
   | RAW_IDENT   (fq, name, pos) -> Ident(I_raw,    fq, name, pos)
   | _ -> internal_error "Parser.to_Ident"
%}


%token EOF
%token <Types.pos> SPACE
%token <string * Types.pos> NUM STRING BAREWORD REVISION COMMENT POD LABEL
%token <string * Types.pos> COMMAND_STRING QUOTEWORDS COMPACT_HASH_SUBSCRIPT
%token <(string * Types.pos) ref> HERE_DOC
%token <string * string * Types.pos> PATTERN
%token <string * string * string * Types.pos> PATTERN_SUBST

%token <string option * string * Types.pos> SCALAR_IDENT ARRAY_IDENT HASH_IDENT FUNC_IDENT STAR_IDENT RAW_IDENT ARRAYLEN_IDENT
%token <string * string * Types.pos> FUNC_DECL_WITH_PROTO

%token IF ELSIF ELSE UNLESS DO WHILE UNTIL MY CONTINUE SUB LOCAL
%token <string> FOR
%token USE PACKAGE BEGIN END
%token <Types.pos> PRINT NEW FORMAT
%token AT DOLLAR PERCENT AMPERSAND STAR ARRAYLEN
%token SEMI_COLON PKG_SCOPE
%token PAREN PAREN_END
%token BRACKET BRACKET_END BRACKET_HASHREF
%token ARRAYREF ARRAYREF_END


%token ARROW
%token INCR DECR
%token POWER
%token TIGHT_NOT BIT_NEG REF
%token PATTERN_MATCH PATTERN_MATCH_NOT
%token MULT DIVISION MODULO REPLICATE
%token PLUS MINUS CONCAT
%token BIT_SHIFT_LEFT BIT_SHIFT_RIGHT
%token LT GT
%token <string> COMPARE_OP EQ_OP
%token BIT_AND
%token BIT_OR BIT_XOR
%token AND_TIGHT
%token OR_TIGHT
%token DOTDOT DOTDOTDOT
%token QUESTION_MARK COLON
%token <string> ASSIGN
%token COMMA RIGHT_ARROW
%token NOT
%token AND
%token OR XOR

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
%left PAREN
%left ARRAYREF BRACKET

%type <string> prog

%start prog


%%
prog: lines EOF { "" }

lines: /* A collection of "lines" in the program */
| {()}
| sideff {()}
| line lines {()}

line:
| decl {()}
| if_then_else {()}
| loop {()}
| LABEL {()}
| SEMI_COLON {()}
| sideff SEMI_COLON {()}
| BRACKET lines BRACKET_END {()}

if_then_else: /* Real conditional expressions */
| IF     PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {()}
| UNLESS PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {()}

elsif:
|  {()}
| ELSIF PAREN expr PAREN_END BRACKET lines BRACKET_END elsif {()}

else_: 
|            { () }
| ELSE BRACKET lines BRACKET_END { () }

loop:
| WHILE PAREN expr PAREN_END BRACKET lines BRACKET_END cont {()}
| UNTIL PAREN expr PAREN_END BRACKET lines BRACKET_END cont {()}
| FOR MY SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont {()}
| FOR SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont {()}
| FOR PAREN expr PAREN_END BRACKET lines BRACKET_END cont {()}
| FOR PAREN expr_or_empty SEMI_COLON expr_or_empty SEMI_COLON expr_or_empty PAREN_END BRACKET lines BRACKET_END {()}

cont: /* Continue blocks */
|  {()}
| CONTINUE BRACKET lines BRACKET_END {()}

sideff: /* An expression which may have a side-effect */
| error {()}
| expr  {()}
| expr   IF    expr {()}
| expr UNLESS  expr {()}
| expr  WHILE  expr {()}
| expr  UNTIL  expr {()}
| expr  FOR    expr {()}

decl:
| FORMAT BAREWORD ASSIGN {()}
| FORMAT ASSIGN {()}
| func_decl SEMI_COLON {()}
| func_decl BRACKET lines BRACKET_END {()}
| func_decl BRACKET BRACKET expr BRACKET_END BRACKET_END {()}
| func_decl BRACKET BRACKET expr BRACKET_END SEMI_COLON BRACKET_END {()}
| PACKAGE word SEMI_COLON {()}
| BEGIN BRACKET lines BRACKET_END {()}
| END BRACKET lines BRACKET_END {()}
| use {()}

use:
| use_word listexpr SEMI_COLON {()}

use_word:
| use_revision word COLON {()}
| use_revision word {()}
| use_revision {()}

use_revision:
| USE REVISION COLON {()}
| USE REVISION {()}
| USE {()}

func_decl:
| SUB word {()}
| FUNC_DECL_WITH_PROTO {()}

listexpr: /* Basic list expressions */
| %prec PREC_LOW {()}
| argexpr %prec PREC_LOW {()}

expr: /* Ordinary expressions; logical combinations */
| expr AND expr {()}
| expr OR expr {()}
| argexpr %prec PREC_LOW {()}

argexpr: /* Expressions are a list of terms joined by commas */
| argexpr comma {()}
| argexpr comma term {()}
| argexpr comma BRACKET expr BRACKET_END {()}
| term %prec PREC_LOW {()}

/********************************************************************************/
term:
| term binop term {()}
| term binop BRACKET expr BRACKET_END {()}
| term LT term {()}
| term LT BRACKET expr BRACKET_END {()}
| term GT term {()}
| term GT BRACKET expr BRACKET_END {()}

/* Unary operators and terms */
| MINUS term %prec UNARY_MINUS {()}
| TIGHT_NOT term {()}
| BIT_NEG term {()}
| INCR term    {()}
| DECR term    {()}
| term INCR    {()}
| term DECR    {()}

| NOT argexpr  {()}


/* Constructors for anonymous data */
| arrayref {()} /* [ 1, 2 ] */
| BRACKET BRACKET_END {()} /* empty hash */
| BRACKET_HASHREF expr_or_empty BRACKET_END %prec PAREN {()} /* { foo => "Bar" } */
| SUB BRACKET lines BRACKET_END %prec PAREN {()}

| termdo {()}
| term question_mark_ colon_ {()}
| REF term {()} /* \$x, \@y, \%z */
| REF BRACKET expr BRACKET_END {()} /* \$x, \@y, \%z */
| my %prec UNIOP {()}
| LOCAL term    %prec UNIOP {()}

| parenthesized {()} /* (1, 2) */
| parenthesized arrayref {()} /* list slice */

| variable {()}

| subscripted {()}

| array arrayref {()} /* array slice */
| array BRACKET expr BRACKET_END {()} /* @hash{@keys} */


/* function_calls */
| func parenthesized {()} /* &foo(@args) */
| word argexpr {()} /* foo(@args) */
| word BRACKET lines BRACKET_END listexpr %prec LSTOP {()} /* map { foo } @bar */
| word BRACKET BRACKET expr BRACKET_END BRACKET_END listexpr %prec LSTOP {()} /* map { foo } @bar */
| word BRACKET BRACKET expr BRACKET_END SEMI_COLON BRACKET_END listexpr %prec LSTOP {()} /* map { foo } @bar */

| term ARROW word_or_scalar parenthesized {warn "term->word_or_scalar(expr_or_empty) -> function_call"} /* $foo->bar(list) */
| term ARROW word_or_scalar {warn "term->word_or_scalar -> function_call"} /* $foo->bar */

| NEW word listexpr {()} /* new Class @args */
| print listexpr {()}

| word {()}

| NUM {()}
| STRING {()}
| REVISION {()}
| COMMAND_STRING {()}
| QUOTEWORDS {()}
| HERE_DOC {()}
| PATTERN {()}
| PATTERN_SUBST {()}
| diamond {()}

diamond:
| LT GT {()}
| LT term GT {()}

print:
| PRINT bareword COLON {()}
| PRINT bareword {()} /* print FH @args */
| PRINT scalar COLON {()}
| PRINT scalar {()} /* print $fh @args */
| PRINT {()}

| PRINT bareword BRACKET {die "use parentheses around print"}

subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {()} /* *main::{something} */
| scalar bracket_subscript             {()} /* $foo{bar} */
| scalar arrayref                      {()} /* $array[$element] */
| term ARROW bracket_subscript         {()} /* somehref->{bar} */
| term ARROW arrayref                  {()} /* somearef->[$element] */
| term ARROW parenthesized             {()} /* $subref->(@args) */
| subscripted bracket_subscript        {()} /* $foo->[bar]{baz} */
| subscripted arrayref                 {()} /* $foo->[$bar][$baz] */
| subscripted parenthesized            {()} /* $foo->{bar}(@args) */

arrayref:
| arrayref_start ARRAYREF_END {()}
| arrayref_start expr ARRAYREF_END {()}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {()}
parenthesized:
| parenthesized_start PAREN_END {()}
| parenthesized_start expr PAREN_END {()}
| parenthesized_start BRACKET expr BRACKET_END PAREN_END {()}

arrayref_start:
| ARRAYREF {()}
| arrayref_start expr comma {()}
| arrayref_start BRACKET expr BRACKET_END comma {()}
parenthesized_start:
| PAREN {()}
| parenthesized_start expr comma {()}
| parenthesized_start BRACKET expr BRACKET_END comma {()}

my: /* Things that can be "my"'d */
| MY parenthesized {()}
| MY scalar {()}
| MY hash {()}
| MY array {()}

termdo: /* Things called with "do" */
| DO term %prec UNIOP {()} /* do $filename */
| DO BRACKET lines BRACKET_END %prec PAREN {()} /* do { code */

question_mark_:
| QUESTION_MARK term {()}
| QUESTION_MARK BRACKET expr BRACKET_END {()}
colon_:
| COLON term {()}
| COLON BRACKET expr BRACKET_END {()}

bracket_subscript:
| BRACKET expr BRACKET_END {()}
| COMPACT_HASH_SUBSCRIPT {()}

binop:
| ASSIGN {()}
| POWER {()}
| MULT {()} | DIVISION {()} | MODULO {()} | REPLICATE {()}
| PLUS {()} | MINUS {()} | CONCAT {()}
| BIT_SHIFT_LEFT {()} | BIT_SHIFT_RIGHT {()}
| COMPARE_OP {()}
| EQ_OP {()}
| BIT_AND {()}
| BIT_OR {()} | BIT_XOR {()}
| DOTDOT {()} | DOTDOTDOT {()}
| AND_TIGHT {()}
| OR_TIGHT {()} | XOR {()}
| PATTERN_MATCH {()} | PATTERN_MATCH_NOT {()}

variable:
| scalar   %prec PAREN {()}
| star     %prec PAREN {()}
| hash     %prec PAREN {()}
| array    %prec PAREN {()}
| arraylen %prec PAREN {()} /* $#x, $#{ something } */
| func     %prec PAREN {()} /* &foo; */

word:
| bareword { fst $1 }
| RAW_IDENT { 
    match $1 with
    | None, name, _ -> name
    | Some s, name, _ -> s ^ "::" ^ name
  }

comma: COMMA {()} | RIGHT_ARROW {()}

word_or_scalar:
| word      {()}
| scalar    {()}

bareword:
| NEW { "new", $1 }
| FORMAT { "format", $1 }
| BAREWORD { $1 }

arraylen: ARRAYLEN_IDENT {()} | ARRAYLEN  scalar {()} | ARRAYLEN  BRACKET lines BRACKET_END {()}
scalar:   SCALAR_IDENT   {()} | DOLLAR    scalar {()} | DOLLAR    BRACKET lines BRACKET_END {()} | DOLLAR BRACKET BRACKET expr BRACKET_END BRACKET_END {()}
func:     FUNC_IDENT     {()} | AMPERSAND scalar {()} | AMPERSAND BRACKET lines BRACKET_END {()}
array:    ARRAY_IDENT    {()} | AT        scalar {()} | AT        BRACKET lines BRACKET_END {()}
hash:     HASH_IDENT     {()} | PERCENT   scalar {()} | PERCENT   BRACKET lines BRACKET_END {()}
star:     STAR_IDENT     {()} | STAR      scalar {()} | STAR      BRACKET lines BRACKET_END {()}

expr_or_empty: {()} | expr {()}

