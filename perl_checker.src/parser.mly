%{ (* -*- caml -*- *)
  open Types
  open Common

  let parse_error msg =
   failwith (Info.pos2sfull_current (Parsing.symbol_start()) (Parsing.symbol_end()) ^ msg)

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
prog: lineseq EOF { "" }

block: BRACKET lineseq BRACKET_END { $2 }

lineseq: /* A collection of "lines" in the program */
|              {[]}
| decl lineseq {[]}
| line {[]}
| LABEL lineseq {[]}
| line {[]}

line:
| if_then_else lineseq { [] }
| loop lineseq { [] }
| SEMI_COLON lineseq { [] }
| sideff SEMI_COLON lineseq { [] }

| sideff { [] }


if_then_else: /* Real conditional expressions */
| IF     PAREN expr PAREN_END block elsif else_ {[]}
| UNLESS PAREN expr PAREN_END block elsif else_ {[]}

elsif:
|  { [] }
| ELSIF PAREN expr PAREN_END block elsif { [ $3, $5 ] @ $6 }

else_: 
|            { None }
| ELSE block { Some $2 }

loop:
| WHILE PAREN expr_or_empty PAREN_END block cont {[]}
| UNTIL PAREN expr PAREN_END block cont {[]}
| FOR MY SCALAR_IDENT PAREN expr PAREN_END block cont {[]}
| FOR SCALAR_IDENT PAREN expr PAREN_END block cont {[]}
| FOR PAREN expr PAREN_END block cont {[]}
| FOR PAREN expr_or_empty SEMI_COLON expr_or_empty SEMI_COLON expr_or_empty PAREN_END block {[]}
| block cont {[]} /* a block is a loop that happens once */

cont: /* Continue blocks */
|  {[]}
| CONTINUE block {[]}

sideff: /* An expression which may have a side-effect */
| error { [] }
| expr { $1 }
| expr   IF    expr { [ (*Binary("if",      $1, $3)*) ] }
| expr UNLESS  expr { [ (*Binary("unless",  $1, $3)*) ] }
| expr  WHILE  expr { [ (*Binary("while",   $1, $3)*) ] }
| expr  UNTIL  expr { [ (*Binary("until",   $1, $3)*) ] }
| expr  FOR    expr { [ (*Binary($2,        $1, $3)*) ] }

decl:
| FORMAT bareword_or_empty ASSIGN {[]}
| SUB word subbody {[]}
| FUNC_DECL_WITH_PROTO subbody {[]}
| PACKAGE word SEMI_COLON {[]}
| BEGIN block {[]}
| END block {[]}
| USE word_or_empty revision_or_empty listexpr SEMI_COLON {[]}

formname: {[]} | BAREWORD {[]}
subbody: block { $1 } | SEMI_COLON {[]}


listexpr: /* Basic list expressions */
| %prec PREC_LOW {[]}
| argexpr %prec PREC_LOW {[]}

expr: /* Ordinary expressions; logical combinations */
| expr AND expr {[]}
| expr OR expr {[]}
| argexpr %prec PREC_LOW {[]}

argexpr: /* Expressions are a list of terms joined by commas */
| argexpr comma {[]}
| argexpr comma term {[]}
| term %prec PREC_LOW {[]}

term:
| term binop term {[]}
| termunop {[]}
| anonymous {[]}
| termdo {[]}
| term QUESTION_MARK term COLON term {[]}
| REF term {[]} /* \$x, \@y, \%z */
| MY myterm     %prec UNIOP {[]}
| LOCAL term    %prec UNIOP {[]}
| PAREN expr_or_empty PAREN_END {[]}

| variable {[]}

| subscripted {[]}

| PAREN expr_or_empty PAREN_END ARRAYREF expr ARRAYREF_END {[]} /* list slice */
| array ARRAYREF expr ARRAYREF_END {[]} /* array slice */
| array BRACKET expr BRACKET_END {[]} /* @hash{@keys} */

| function_call {[]}

| word {[]}
| value {[]}

function_call:
| func PAREN expr_or_empty PAREN_END {[]} /* &foo(@args) */
| word argexpr {[]} /* foo(@args) */
| word block listexpr %prec LSTOP {[]} /* map { foo } @bar */

| term ARROW word_or_scalar PAREN expr_or_empty PAREN_END {[]} /* $foo->bar(list) */
| term ARROW word_or_scalar {[]} /* $foo->bar */

| NEW word listexpr {[]} /* new Class @args */
| PRINT argexpr {[]} /* print $fh @args */
| PRINT word_or_scalar argexpr {[]} /* print $fh @args */

termdo: /* Things called with "do" */
| DO term %prec UNIOP                               {[]} /* do $filename */
| DO block %prec PAREN                   {[]} /* do { code */

termunop: /* Unary operators and terms */
| MINUS term %prec UNARY_MINUS {[]}
| TIGHT_NOT term {[]}
| BIT_NEG term {[]}
| INCR term    {[]}
| DECR term    {[]}
| term INCR    {[]}
| term DECR    {[]}

| NOT argexpr  {[]}

myterm: /* Things that can be "my"'d */
| PAREN expr_or_empty PAREN_END {[]}
| scalar {[]}
| hash {[]}
| array {[]}

subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript        {[]} /* *main::{something} */
| scalar bracket_subscript                    {[]} /* $foo{bar} */
| scalar ARRAYREF expr ARRAYREF_END           {[]} /* $array[$element] */
| term ARROW bracket_subscript                {[]} /* somehref->{bar} */
| term ARROW ARRAYREF expr ARRAYREF_END       {[]} /* somearef->[$element] */
| term ARROW PAREN expr_or_empty PAREN_END    {[]} /* $subref->(@args) */
| subscripted bracket_subscript               {[]} /* $foo->[bar]{baz;} */
| subscripted ARRAYREF expr ARRAYREF_END      {[]} /* $foo->[$bar][$baz] */
| subscripted PAREN expr_or_empty PAREN_END   {[]} /* $foo->{bar}(@args) */

bracket_subscript:
| BRACKET expr BRACKET_END {[]}
| COMPACT_HASH_SUBSCRIPT {[]}

anonymous: /* Constructors for anonymous data */
| ARRAYREF expr_or_empty ARRAYREF_END {[]}
| BRACKET expr_or_empty BRACKET_END %prec PAREN {[]} /* { foo => "Bar" } */
| BRACKET_HASHREF expr_or_empty BRACKET_END %prec PAREN {[]} /* { foo => "Bar" } */
| SUB block %prec PAREN {[]}

binop:
| ASSIGN {[]}
| POWER {[]}
| MULT {[]} | DIVISION {[]} | MODULO {[]} | REPLICATE {[]}
| PLUS {[]} | MINUS {[]} | CONCAT {[]}
| BIT_SHIFT_LEFT {[]} | BIT_SHIFT_RIGHT {[]}
| LT {[]} | GT {[]} | COMPARE_OP {[]}
| EQ_OP {[]}
| BIT_AND {[]}
| BIT_OR {[]} | BIT_XOR {[]}
| DOTDOT {[]} | DOTDOTDOT {[]}
| AND_TIGHT {[]}
| OR_TIGHT {[]} | XOR {[]}
| PATTERN_MATCH {[]} | PATTERN_MATCH_NOT {[]}

value:
| NUM {[]}
| STRING {[]}
| REVISION {[]}
| COMMAND_STRING {[]}
| QUOTEWORDS {[]}
| HERE_DOC {[]}
| PATTERN {[]}
| PATTERN_SUBST {[]}
| LT GT {[]}
| LT term GT {[]}

variable:
| scalar   %prec PAREN {[]}
| star     %prec PAREN {[]}
| hash     %prec PAREN {[]}
| array    %prec PAREN {[]}
| arraylen %prec PAREN {[]} /* $#x, $#{ something } */
| func     %prec PAREN {[]} /* &foo; */

word:
| bareword { fst $1 }
| RAW_IDENT { 
    match $1 with
    | None, name, _ -> name
    | Some s, name, _ -> s ^ "::" ^ name
  }

comma: COMMA {[]} | RIGHT_ARROW {[]}

word_or_scalar:
| bareword  { [] }
| RAW_IDENT { [] }
| scalar    { [] }

block_or_scalar: block {[]} | scalar {[]}

bareword:
| NEW { "new", $1 }
| PRINT { "print", $1 }
| FORMAT { "format", $1 }
| BAREWORD { $1 }

arraylen: ARRAYLEN_IDENT {[]} | ARRAYLEN block_or_scalar {[]}
scalar: SCALAR_IDENT {[]} | DOLLAR    block_or_scalar {[]}
func:   FUNC_IDENT   {[]} | AMPERSAND block_or_scalar {[]}
array:  ARRAY_IDENT  {[]} | AT        block_or_scalar {[]}
hash:   HASH_IDENT   {[]} | PERCENT   block_or_scalar {[]}
star:   STAR_IDENT   {[]} | STAR      block_or_scalar {[]}

expr_or_empty: {[]} | expr {[]}
word_or_empty: {[]} | word {[]}
bareword_or_empty: {[]} | bareword {[]}
revision_or_empty: {[]} | REVISION {[]}
