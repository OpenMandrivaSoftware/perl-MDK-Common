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


%token <unit   Types.any_spaces_pos> EOF
%token <string Types.any_spaces_pos> NUM RAW_STRING BAREWORD BAREWORD_PAREN REVISION COMMENT POD LABEL PO_COMMENT PERL_CHECKER_COMMENT ONE_SCALAR_PARA
%token <(string * string) Types.any_spaces_pos> PRINT_TO_STAR PRINT_TO_SCALAR
%token <string Types.any_spaces_pos> QUOTEWORDS COMPACT_HASH_SUBSCRIPT
%token <(string * Types.raw_pos) Types.any_spaces_pos> RAW_HERE_DOC
%token <(string * ((int * int) * token) list) list Types.any_spaces_pos> STRING COMMAND_STRING
%token <((string * ((int * int) * token) list) list * Types.raw_pos) Types.any_spaces_pos> HERE_DOC FORMAT

%token <((string * ((int * int) * token) list) list * string) Types.any_spaces_pos> PATTERN QR_PATTERN
%token <((string * ((int * int) * token) list) list * (string * ((int * int) * token) list) list * string) Types.any_spaces_pos> PATTERN_SUBST

%token <(string option * string) Types.any_spaces_pos> SCALAR_IDENT ARRAY_IDENT HASH_IDENT FUNC_IDENT STAR_IDENT RAW_IDENT RAW_IDENT_PAREN ARRAYLEN_IDENT
%token <string Types.any_spaces_pos> SUB_WITH_PROTO
%token <(string option * string * string) Types.any_spaces_pos> FUNC_DECL_WITH_PROTO

%token <string Types.any_spaces_pos> FOR PRINT
%token <unit   Types.any_spaces_pos> NEW
%token <string Types.any_spaces_pos> COMPARE_OP COMPARE_OP_STR EQ_OP EQ_OP_STR
%token <string Types.any_spaces_pos> ASSIGN MY_OUR

%token <unit   Types.any_spaces_pos> IF ELSIF ELSE UNLESS DO WHILE UNTIL CONTINUE SUB LOCAL
%token <unit   Types.any_spaces_pos> USE PACKAGE BEGIN END
%token <unit   Types.any_spaces_pos> AT DOLLAR PERCENT AMPERSAND STAR ARRAYLEN
%token <unit   Types.any_spaces_pos> SEMI_COLON PKG_SCOPE
%token <unit   Types.any_spaces_pos> PAREN PAREN_END
%token <unit   Types.any_spaces_pos> BRACKET BRACKET_END BRACKET_HASHREF
%token <unit   Types.any_spaces_pos> ARRAYREF ARRAYREF_END

%token <unit   Types.any_spaces_pos> ARROW
%token <unit   Types.any_spaces_pos> INCR DECR
%token <unit   Types.any_spaces_pos> POWER
%token <unit   Types.any_spaces_pos> TIGHT_NOT BIT_NEG REF
%token <unit   Types.any_spaces_pos> PATTERN_MATCH PATTERN_MATCH_NOT
%token <string Types.any_spaces_pos> MULT
%token <string Types.any_spaces_pos> PLUS
%token <string Types.any_spaces_pos> BIT_SHIFT
%token <unit   Types.any_spaces_pos> LT GT CONCAT MULT_L_STR
%token <unit   Types.any_spaces_pos> BIT_AND
%token <unit   Types.any_spaces_pos> BIT_OR BIT_XOR
%token <unit   Types.any_spaces_pos> AND_TIGHT
%token <unit   Types.any_spaces_pos> OR_TIGHT
%token <string Types.any_spaces_pos> DOTDOT
%token <unit   Types.any_spaces_pos> QUESTION_MARK COLON
%token <unit   Types.any_spaces_pos> COMMA RIGHT_ARROW
%token <unit   Types.any_spaces_pos> NOT
%token <unit   Types.any_spaces_pos> AND
%token <unit   Types.any_spaces_pos> OR XOR

%nonassoc PREC_LOW
%nonassoc LOOPEX

%right      OR XOR
%right      AND
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
%nonassoc   EQ_OP EQ_OP_STR
%nonassoc   LT GT COMPARE_OP COMPARE_OP_STR
%nonassoc   UNIOP
%left       BIT_SHIFT
%left       PLUS CONCAT
%left       MULT MULT_L_STR
%left       PATTERN_MATCH PATTERN_MATCH_NOT
%right      TIGHT_NOT BIT_NEG REF UNARY_MINUS
%right      POWER
%nonassoc   INCR DECR
%left       ARROW

%nonassoc PAREN_END
%left PAREN PREC_HIGH
%left ARRAYREF BRACKET

%type <Types.fromparser list> prog
%type <prio_expr_spaces_pos> expr term
%type <fromparser any_spaces_pos> scalar bracket_subscript variable restricted_subscripted

%start prog


%%
prog: lines EOF {$1.any}

lines: /* A collection of "lines" in the program */
| { default_esp [] }
| sideff { new_1esp [$1.any] $1 }
| line lines { if $2.any <> [] then mcontext_check_none "value is dropped" $1.any $1; new_esp $2.mcontext ($1.any @ $2.any) $1 $2 }

line:
| decl { new_1esp [$1.any] $1 }
| if_then_else { new_1esp [$1.any] $1 }
| loop { new_1esp [$1.any] $1 }
| LABEL { sp_cr($1); new_1esp [Label $1.any] $1 }
| PERL_CHECKER_COMMENT {sp_p($1); new_1esp [Perl_checker_comment($1.any, get_pos $1)] $1 }
| semi_colon {new_1esp [Semi_colon] $1 }
| sideff semi_colon {new_1esp [$1.any ; Semi_colon] $1 }
| BRACKET lines BRACKET_END            {check_block_sub $2 $3; new_esp $2.mcontext [Block $2.any] $1 $3}
| BRACKET lines BRACKET_END semi_colon {check_block_sub $2 $3; new_esp $2.mcontext [Block $2.any] $1 $4}

if_then_else: /* Real conditional expressions */
| IF     PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_bool $3; check_block_sub $6 $7; to_Call_op (if $9.any = [] then M_none else mcontext_lmerge ($6.mcontext :: mcontext_lmaybe $8 @ [$9.mcontext])) "if"     (prio_lo P_loose $3 :: Block $6.any :: $8.any @ $9.any) $1 $9}
| UNLESS PAREN expr PAREN_END BRACKET lines BRACKET_END elsif else_ {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_bool $3; check_block_sub $6 $7; check_unless_else $8 $9; to_Call_op M_none "unless" (prio_lo P_loose $3 :: Block $6.any :: $8.any @ $9.any) $1 $9}

elsif:
|  {default_esp []}
| ELSIF PAREN expr PAREN_END BRACKET lines BRACKET_END elsif {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_bool $3; check_block_sub $6 $7; new_esp (mcontext_lmerge ($6.mcontext :: mcontext_lmaybe $8)) (prio_lo P_loose $3 :: Block $6.any :: $8.any) $1 $8}

else_: 
|            { default_esp [] }
| ELSE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); check_block_sub $3 $4; new_esp $3.mcontext [Block $3.any] $1 $4}

loop:
| WHILE PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_bool $3; check_block_sub $6 $7; to_Call_op M_none "while" [ prio_lo P_loose $3; Block $6.any ] $1 $8}
| UNTIL PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_bool $3; check_block_sub $6 $7; to_Call_op M_none "until" [ prio_lo P_loose $3; Block $6.any ] $1 $8}
| FOR PAREN expr_or_empty semi_colon expr_or_empty semi_colon expr_or_empty PAREN_END BRACKET lines BRACKET_END {sp_p($1); check_for($1); sp_n($2); sp_0($3); sp_p($5); sp_p($7); sp_0($8); sp_n($9); check_block_sub $10 $11; to_Call_op M_none "for" [ $3.any; $5.any; $7.any; Block $10.any ] $1 $11}
| FOR SCALAR_IDENT PAREN expr PAREN_END BRACKET lines BRACKET_END cont { die_rule "don't use for without \"my\"ing the iteration variable" }
| FOR PAREN expr PAREN_END BRACKET lines BRACKET_END cont {sp_p($1); sp_n($2); sp_0($3); sp_0_or_cr($4); sp_p($5); mcontext_check M_list $3; check_block_sub $6 $7; check_for_foreach $1 $3; to_Call_op M_none "foreach" [ prio_lo P_loose $3; Block $6.any ] $1 $8}
| for_my lines BRACKET_END cont {check_block_sub $2 $3; to_Call_op M_none "foreach my" ($1.any @ [ Block $2.any ]) $1 $4}

for_my:
| FOR MY_OUR SCALAR_IDENT PAREN expr PAREN_END BRACKET {sp_p($1); check_my($2); check_foreach($1); sp_n($4); sp_0($5); sp_0_or_cr($6); sp_p($7); new_esp M_none [ My_our($2.any, [I_scalar, snd $3.any], get_pos $3); prio_lo P_loose $5 ] $1 $7}


cont: /* Continue blocks */
|  {default_esp ()}
| CONTINUE BRACKET lines BRACKET_END {sp_p($1); sp_n($2); check_block_sub $3 $4; new_esp $3.mcontext () $1 $4}

sideff: /* An expression which may have a side-effect */
| expr  { new_1esp $1.any.expr $1 }
| expr   IF    expr {sp_p($2); sp_p($3); mcontext_check M_bool $3; call_op_if_infix     (prio_lo P_loose $1) (prio_lo P_loose $3) $1 $3}
| expr UNLESS  expr {sp_p($2); sp_p($3); mcontext_check M_bool $3; call_op_unless_infix (prio_lo P_loose $1) (prio_lo P_loose $3) $1 $3}
| expr  WHILE  expr {sp_p($2); sp_p($3); mcontext_check M_bool $3; to_Call_op M_none "while infix" [ prio_lo P_loose $1 ; prio_lo P_loose $3 ] $1 $3}
| expr  UNTIL  expr {sp_p($2); sp_p($3); mcontext_check M_bool $3; to_Call_op M_none "until infix" [ prio_lo P_loose $1 ; prio_lo P_loose $3 ] $1 $3}
| expr  FOR    expr {sp_p($2); sp_p($3); mcontext_check M_list $3; check_foreach($2); to_Call_op M_none "for infix"   [ prio_lo P_loose $1 ; prio_lo P_loose $3 ] $1 $3}

decl:
| FORMAT BAREWORD ASSIGN {to_Call_op M_none "format" [Raw_string($2.any, get_pos $2) ; to_String false (new_1esp (fst $1.any) $1)] $1 $3}
| FORMAT ASSIGN {new_esp M_none Too_complex $1 $2}
| func_decl semi_colon {if snd $1.any = None then die_rule "there is no need to pre-declare in Perl!" else (warn_rule "please don't use prototype pre-declaration" ; new_esp M_special Too_complex $1 $2) }
| func_decl BRACKET BRACKET_END {sp_n($2); sp_0_or_cr($3); let name, proto = $1.any in new_esp M_none (sub_declaration (name, proto) [] Real_sub_declaration) $1 $3}
| func_decl BRACKET lines BRACKET_END {sp_n($2); check_block_sub $3 $4; new_esp M_none (sub_declaration $1.any $3.any Real_sub_declaration) $1 $4}
| func_decl BRACKET BRACKET expr BRACKET_END            BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); new_esp M_none (sub_declaration $1.any [hash_ref $4] Real_sub_declaration) $1 $6}
| func_decl BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($7); new_esp M_none (sub_declaration $1.any [hash_ref $4; Semi_colon] Real_sub_declaration) $1 $7}
| PACKAGE word semi_colon {sp_0_or_cr($1); sp_1($2); new_esp M_none (Package $2.any) $1 $3}
| BEGIN BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); check_block_sub $3 $4; new_esp M_none (Sub_declaration(Ident(None, "BEGIN", get_pos $1), None, Block $3.any, Glob_assign)) $1 $4}
| END   BRACKET lines BRACKET_END {sp_0_or_cr($1); sp_1($2); check_block_sub $3 $4; new_esp M_none (Sub_declaration(Ident(None, "END",   get_pos $1), None, Block $3.any, Glob_assign)) $1 $4}
| use {$1}

use:
| use_word listexpr semi_colon {sp_n($2); new_esp M_none (Use($1.any, $2.any.expr)) $1 $3}
| use_revision word_paren PAREN listexpr PAREN_END {sp_0($4); sp_0_or_cr($5); new_esp M_none (Use($2.any, $4.any.expr)) $1 $5}

use_word:
| use_revision word comma {new_esp M_none $2.any $1 $3}
| use_revision word {new_esp M_none $2.any $1 $2}
| use_revision {new_1esp (Ident(None, "", get_pos $1)) $1 }

use_revision:
| USE REVISION comma {$1}
| USE REVISION {$1}
| USE {$1}

func_decl:
| SUB word { new_esp M_none ($2.any, None) $1 $2}
| FUNC_DECL_WITH_PROTO {new_1esp (Ident(fst3 $1.any, snd3 $1.any, get_pos $1), Some (ter3 $1.any)) $1 }

listexpr: /* Basic list expressions */
| %prec PREC_LOW { default_pesp P_tok []}
| argexpr %prec PREC_LOW {$1}

expr: /* Ordinary expressions; logical combinations */
| expr AND expr {sp_p($2); sp_p($3); mcontext_check M_bool $1; mcontext_check_none "value should be dropped" [$3.any.expr] $3; to_Call_op_ M_none P_and "and" [ prio_lo P_and $1; prio_lo_after P_and $3 ] $1 $3}
| expr OR  expr {sp_p($2); sp_p($3); mcontext_check M_bool $1; mcontext_check_none "value should be dropped" [$3.any.expr] $3; to_Call_op_ M_none P_or  "or"  [ prio_lo P_or  $1; prio_lo_after P_or  $3 ] $1 $3}
| argexpr %prec PREC_LOW { new_1pesp $1.any.priority (List $1.any.expr) $1 }

argexpr: /* Expressions are a list of terms joined by commas */
| argexpr comma { new_pesp $1.mcontext P_comma $1.any.expr $1 $2}
| bareword RIGHT_ARROW term {if not_simple ($3.any.expr) then sp_p($3); new_pesp (mtuple_context_concat M_string $3.mcontext)  P_comma (followed_by_comma [$1.any] false @ [$3.any.expr]) $1 $3}
| bareword RIGHT_ARROW BRACKET expr BRACKET_END {sp_p($3); sp_p($5); new_pesp (mtuple_context_concat M_string (M_ref M_hash)) P_comma (followed_by_comma [$1.any] false @ [ hash_ref $4 ]) $1 $5}
| argexpr comma term {prio_lo_check P_comma $1.any.priority $1.pos (last $1.any.expr); if not_simple ($3.any.expr) then sp_p($3); new_pesp (mtuple_context_concat $1.mcontext $3.mcontext) P_comma (followed_by_comma $1.any.expr $2.any @ [$3.any.expr]) $1 $3}
| argexpr comma BRACKET expr BRACKET_END {sp_p($3); sp_p($5); new_pesp (mtuple_context_concat $1.mcontext (M_ref M_hash)) P_comma (followed_by_comma $1.any.expr $2.any @ [ hash_ref $4 ]) $1 $5}
| term %prec PREC_LOW { new_1pesp $1.any.priority [$1.any.expr] $1 }

/********************************************************************************/
term:
| term 
   COMPARE_OP_STR term {sp_p $2; symops P_cmp M_string M_bool $2.any $1 $2 $3}
| term COMPARE_OP term {sp_p $2; symops P_cmp M_float M_bool $2.any $1 $2 $3}
| term LT         term {sp_p $2; symops P_cmp M_float M_bool "<" $1 $2 $3}
| term GT         term {sp_p $2; symops P_cmp M_float M_bool ">" $1 $2 $3}
| term EQ_OP      term {sp_p $2; symops P_eq M_float M_bool $2.any $1 $2 $3}
| term EQ_OP_STR  term {sp_p $2; symops P_eq M_string M_bool $2.any $1 $2 $3}

| term BIT_AND    term {sp_p $2; symops P_bit M_int M_int "&" $1 $2 $3}
| term BIT_OR     term {         symops P_bit M_int M_int "|" $1 $2 $3}
| term BIT_XOR    term {sp_p $2; symops P_bit M_int M_int "^" $1 $2 $3}

| term POWER      term {         symops P_tight M_float (mcontext_float_or_int [$1.mcontext; $3.mcontext]) "**" $1 $2 $3}
| term PLUS       term {         symops P_add M_float (mcontext_float_or_int [$1.mcontext; $3.mcontext]) $2.any $1 $2 $3}
| term CONCAT     term {         symops P_add M_string M_string "." $1 $2 $3}
| term BIT_SHIFT  term {         symops (P_paren_wanted P_tight) M_int M_int $2.any $1 $2 $3}
| term XOR        term {sp_p $2; symops (P_paren_wanted P_expr) M_bool M_bool "xor" $1 $2 $3}
| term DOTDOT     term {         symops (P_paren_wanted P_expr) M_unknown_scalar M_string $2.any $1 $2 $3}

| term AND_TIGHT  term {sp_p $2; sp_same $2 $3; mcontext_check M_bool $1; let pri = P_tight_and in to_Call_op_ (mcontext_to_scalar $3.mcontext) pri "&&" [prio_lo pri $1; prio_lo_after pri $3] $1 $3}
| term OR_TIGHT   term {sp_p $2; sp_same $2 $3; mcontext_check M_bool $1; let pri = P_tight_or in to_Call_op_ (mcontext_to_scalar (mcontext_merge $1.mcontext $3.mcontext)) pri "||" [prio_lo pri $1; prio_lo_after pri $3] $1 $3} 

| term MULT       term {sp_same $2 $3; let pri = P_mul in to_Call_op_ (mcontext_float_or_int [$1.mcontext; $3.mcontext]) pri $2.any [prio_lo_concat $1; prio_lo_after pri $3] $1 $3}
| term MULT_L_STR term {sp_same $2 $3; mcontext_check M_int $3; let pri = P_mul in to_Call_op_ (if mcontext_lower $1.mcontext M_string then M_string else M_list) pri "x" 
    										                                                 [prio_lo_concat $1; prio_lo_after pri $3] $1 $3}

| term ASSIGN     term {sp_same $2 $3; let pri = P_assign    in to_Call_op_ (mcontext_op_assign $1 $3)       pri $2.any [$1.any.expr   ; prio_lo_after pri $3] $1 $3}

| term ASSIGN     BRACKET expr_bracket_end {sp_p($2); sp_p($3); sp_p($4); to_Call_op_ (M_mixed [M_ref M_hash; M_none]) P_assign $2.any [prio_lo P_assign $1; $4.any] $1 $4}
| term AND_TIGHT  BRACKET expr_bracket_end {sp_p($2); sp_p($3); sp_p($4); to_Call_op_ M_bool P_tight_and "&&"   [prio_lo P_assign $1; $4.any] $1 $4}
| term OR_TIGHT   BRACKET expr_bracket_end {sp_p($2); sp_p($3); sp_p($4); to_Call_op_ M_bool P_tight_or  "||"   [prio_lo P_assign $1; $4.any] $1 $4}


| term PATTERN_MATCH     PATTERN   {sp_n($2); sp_p($3); check_unneeded_var_dollar_   ($1); mcontext_check M_string $1; to_Call_op_ M_array P_expr "m//"  ($1.any.expr :: from_PATTERN $3) $1 $3}
| term PATTERN_MATCH_NOT PATTERN   {sp_n($2); sp_p($3); check_unneeded_var_dollar_not($1); mcontext_check M_string $1; to_Call_op_ M_int   P_expr "!m//" ($1.any.expr :: from_PATTERN $3) $1 $3}
| term PATTERN_MATCH PATTERN_SUBST {sp_n($2); sp_p($3); check_unneeded_var_dollar_s  ($1); to_Call_op_ (M_mixed[M_none; M_int]) P_expr "s///" ($1.any.expr :: from_PATTERN_SUBST $3) $1 $3}
| term PATTERN_MATCH_NOT PATTERN_SUBST {die_with_rawpos $2.pos "use =~ instead of !~ and negate the return value"}

| term PATTERN_MATCH     QR_PATTERN {warn $3.pos "use m/.../ or /.../ instead of qr/.../ when you do a pattern matching"; to_Call_op_ M_array P_expr "m//"  ($1.any.expr :: from_PATTERN $3) $1 $3}
| term PATTERN_MATCH_NOT QR_PATTERN {warn $3.pos "use m/.../ or /.../ instead of qr/.../ when you do a pattern matching"; to_Call_op_ M_int   P_expr "!m//" ($1.any.expr :: from_PATTERN $3) $1 $3}
| term PATTERN_MATCH     scalar { new_pesp M_array P_expr (Call(Too_complex, [$1.any.expr ; $3.any ])) $1 $3}
| term PATTERN_MATCH_NOT scalar { new_pesp M_int   P_expr (Call(Too_complex, [$1.any.expr ; $3.any ])) $1 $3}

| term PATTERN_MATCH     RAW_STRING {warn $3.pos "use a regexp, not a string"; to_Call_op_ M_array P_expr "m//"  [ $1.any.expr; to_Raw_string $3 ] $1 $3}
| term PATTERN_MATCH_NOT RAW_STRING {warn $3.pos "use a regexp, not a string"; to_Call_op_ M_int   P_expr "!m//" [ $1.any.expr; to_Raw_string $3 ] $1 $3}
| term PATTERN_MATCH     STRING {warn $3.pos "use a regexp, not a string"; to_Call_op_ M_array P_expr "m//"  [ $1.any.expr; to_String false $3 ] $1 $3}
| term PATTERN_MATCH_NOT STRING {warn $3.pos "use a regexp, not a string"; to_Call_op_ M_int   P_expr "!m//" [ $1.any.expr; to_String false $3 ] $1 $3}


| term QUESTION_MARK term COLON term {sp_p($2); sp_p($3); sp_p($4); sp_p($5); mcontext_check M_bool $1; to_Call_op_ (mcontext_merge $3.mcontext $5.mcontext) P_ternary "?:" (check_ternary_paras(prio_lo P_ternary $1, prio_lo_after P_ternary $3, prio_lo_after P_ternary $5)) $1 $5}
| term QUESTION_MARK term COLON BRACKET expr BRACKET_END {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); mcontext_check M_bool $1; to_Call_op_ (mcontext_merge $3.mcontext (M_ref M_hash)) P_ternary "?:" (check_ternary_paras(prio_lo P_ternary $1, prio_lo_after P_ternary $3, hash_ref $6)) $1 $7}
| term QUESTION_MARK BRACKET expr BRACKET_END COLON term {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); mcontext_check M_bool $1; to_Call_op_ (mcontext_merge $7.mcontext (M_ref M_hash)) P_ternary "?:" (check_ternary_paras(prio_lo P_ternary $1, hash_ref $4, prio_lo_after P_ternary $7)) $1 $7}
| term QUESTION_MARK BRACKET expr BRACKET_END COLON BRACKET expr BRACKET_END {sp_p($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); sp_p($7); sp_p($8); sp_p($9); mcontext_check M_bool $1; to_Call_op_ (M_ref M_hash) P_ternary "?:" (check_ternary_paras(prio_lo P_ternary $1, hash_ref $4, hash_ref $8)) $1 $9}


/* Unary operators and terms */
| PLUS term %prec UNARY_MINUS {
    sp_0($2); 
    match $1.any with
    | "+" ->
	warn_rule "don't use unary +" ;
	to_Call_op_ (mcontext_float_or_int [$2.mcontext]) P_tight "+ unary" [$2.any.expr] $1 $2
    | "-" ->
	to_Call_op_ (mcontext_float_or_int [$2.mcontext]) P_tight "- unary" [$2.any.expr] $1 $2
    | _ -> die_rule "syntax error"
}
| TIGHT_NOT term {check_negatable_expr $2; mcontext_check M_bool $2; to_Call_op_ M_bool P_tight "not" [$2.any.expr] $1 $2}
| BIT_NEG term {          mcontext_check M_int $2; to_Call_op_ M_int P_expr "~" [$2.any.expr] $1 $2}
| INCR term    {sp_0($2); mcontext_check M_int $2; to_Call_op_ (M_mixed [M_int ; M_none]) P_tight "++" [$2.any.expr] $1 $2}
| DECR term    {sp_0($2); mcontext_check M_int $2; to_Call_op_ (M_mixed [M_int ; M_none]) P_tight "--" [$2.any.expr] $1 $2}
| term INCR    {sp_0($2); mcontext_check M_int $1; to_Call_op_ (M_mixed [M_int ; M_none]) P_tight "++ post" [$1.any.expr] $1 $2}
| term DECR    {sp_0($2); mcontext_check M_int $1; to_Call_op_ (M_mixed [M_int ; M_none]) P_tight "-- post" [$1.any.expr] $1 $2}
| NOT argexpr  {warn_rule "don't use \"not\", use \"!\" instead"; mcontext_check_unop_l M_bool $2; to_Call_op_ M_bool P_and "not" ($2.any.expr) $1 $2}

/* Constructors for anonymous data */

| ARRAYREF ARRAYREF_END {sp_0($2); new_pesp (M_ref M_array) P_expr (Ref(I_array, List[])) $1 $2}
| arrayref_start ARRAYREF_END {(if $1.any = [] then sp_0 else sp_p)($2) ; new_pesp (M_ref M_array) P_expr (Ref(I_array, List $1.any)) $1 $2}
| arrayref_start expr ARRAYREF_END {sp_same $2 $3; new_pesp (M_ref M_array) P_expr (Ref(I_array, List($1.any @ [$2.any.expr]))) $1 $3}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {sp_same $2 $5; new_pesp (M_ref M_array) P_expr (Ref(I_array, List($1.any @ [hash_ref $3]))) $1 $5}

| BRACKET BRACKET_END {new_pesp (M_ref M_hash) P_expr (Ref(I_hash, List [])) $1 $2} /* empty hash */
| BRACKET_HASHREF expr BRACKET_END %prec PREC_HIGH {sp_p($3); new_pesp (M_ref M_hash) P_expr (hash_ref $2) $1 $3} /* { foo => "Bar" } */
| SUB            BRACKET BRACKET_END %prec PREC_HIGH {sp_n($2); sp_0($3); new_pesp (M_ref M_sub) P_expr (anonymous_sub None          (new_esp (M_ref M_array) [] $2 $2)) $1 $3}
| SUB_WITH_PROTO BRACKET BRACKET_END %prec PREC_HIGH {sp_n($2); sp_0($3); new_pesp (M_ref M_sub) P_expr (anonymous_sub (Some $1.any) (new_esp (M_ref M_array) [] $2 $2)) $1 $3}
| SUB            BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); check_block_sub $3 $4; new_pesp (M_ref M_sub) P_expr (anonymous_sub None          $3) $1 $4}
| SUB_WITH_PROTO BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); check_block_sub $3 $4; new_pesp (M_ref M_sub) P_expr (anonymous_sub (Some $1.any) $3) $1 $4}

| termdo {new_1pesp P_tok $1.any $1}
| REF term {new_pesp (M_ref $2.mcontext) P_expr (Ref(I_scalar, remove_call_with_same_para_special $2.any.expr)) $1 $2} /* \$x, \@y, \%z */
| my_our %prec UNIOP {new_1pesp P_expr $1.any $1}
| LOCAL term    %prec UNIOP {sp_n($2); new_pesp (M_mixed [ $2.mcontext ; M_none ]) P_expr (to_Local $2) $1 $2}

| parenthesized {new_1pesp $1.any.priority (List $1.any.expr) $1} /* (1, 2) */
| parenthesized arrayref {sp_0($2); let is_slice = not (is_only_one_in_List $2.any) in new_pesp (if is_slice then M_list else M_unknown_scalar) P_tok (to_Deref_with(I_array, (if is_slice then I_array else I_scalar), List $1.any.expr, List $2.any)) $1 $2} /* list indexing or slicing */

| variable { 
    let e = 
      match $1.any with
      | Deref(I_func, Ident _) ->
	  call_with_same_para_special $1.any (* not the same as f(@_) *)
      | e -> e in
    new_1pesp P_tok e $1 
  }

| subscripted {new_1pesp P_tok $1.any $1}

| array arrayref {new_pesp M_list P_expr (to_Deref_with(I_array, I_array, from_array $1, List $2.any)) $1 $2} /* array slice: @array[vals] */
| array BRACKET expr BRACKET_END {sp_0($2); sp_0($3); sp_0($4); new_pesp M_list P_expr (to_Deref_with(I_hash, I_array, from_array $1, $3.any.expr)) $1 $4} /* hash slice: @hash{@keys} */

/* function_calls */
| ONE_SCALAR_PARA RAW_STRING               {call_one_scalar_para $1 [to_Raw_string $2] $1 $2}
| ONE_SCALAR_PARA STRING                   {call_one_scalar_para $1 [to_String true $2] $1 $2}
| ONE_SCALAR_PARA variable                 {call_one_scalar_para $1 [$2.any] $1 $2}
| ONE_SCALAR_PARA restricted_subscripted   {call_one_scalar_para $1 [$2.any] $1 $2}
| ONE_SCALAR_PARA parenthesized            {call_one_scalar_para $1 $2.any.expr $1 $2}
| ONE_SCALAR_PARA BRACKET lines BRACKET_END {sp_n($2); check_block_sub $3 $4; new_pesp M_unknown P_tok (call(Deref(I_func, Ident(None, $1.any, raw_pos2pos $1.pos)), [anonymous_sub None $3])) $1 $4} /* eval { foo } */
| ONE_SCALAR_PARA diamond {call_one_scalar_para $1 [$2.any] $1 $2}
| ONE_SCALAR_PARA {call_one_scalar_para $1 [] $1 $1}
| ONE_SCALAR_PARA word argexpr {check_parenthesized_first_argexpr_with_Ident  $2.any $3; call_one_scalar_para $1 [call(Deref(I_func, $2.any), $3.any.expr)] $1 $3} /* ref foo $a, $b */
| ONE_SCALAR_PARA hash PKG_SCOPE {sp_0($3); call_one_scalar_para $1 [ Call(Too_complex, [$2.any]) ] $1 $3} /* keys %main:: */
| ONE_SCALAR_PARA BAREWORD {if $2.any = "_" && $1.any.[0] = '-' then new_pesp M_bool P_mul Too_complex $1 $2 else die_rule "syntax error"} /* -e "foo" && -f _ */

| func parenthesized {sp_0($2); call_func $1 $2} /* &foo(@args) */
| word argexpr {check_parenthesized_first_argexpr_with_Ident $1.any $2; call_no_paren $1 $2} /* foo $a, $b */
| word BRACKET lines BRACKET_END MULT { die_with_rawpos $5.pos "I can't handle this correctly, please add parentheses" }
| word BRACKET lines BRACKET_END COMMA argexpr %prec LSTOP {sp_n($2); new_pesp M_unknown P_call_no_paren (call(Deref(I_func, $1.any), Ref(I_hash, List $3.any) :: $6.any.expr)) $1 $6} /* bless { foo }, $bar */
| word_paren parenthesized {sp_0($2); call_with_paren $1 $2} /* foo(@args) */
| word BRACKET lines BRACKET_END listexpr %prec LSTOP {sp_n($2); check_block_sub $3 $4; call_and_context(Deref(I_func, $1.any), anonymous_sub None $3 :: $5.any.expr) false (if $5.any.expr = [] then P_tok else P_call_no_paren) $1 $5} /* map { foo } @bar */
| word BRACKET BRACKET expr BRACKET_END            BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($6); new_pesp M_unknown (if $7.any.expr = [] then P_tok else P_call_no_paren) (call(Deref(I_func, $1.any), anonymous_sub None (new_esp (M_ref M_hash) [ hash_ref $4 ] $4 $4) :: $7.any.expr)) $1 $7} /* map { { foo } } @bar */
| word BRACKET BRACKET expr BRACKET_END semi_colon BRACKET_END listexpr %prec LSTOP {sp_n($2); sp_p($3); sp_p($4); sp_p($5); sp_p($7); new_pesp M_unknown (if $8.any.expr = [] then P_tok else P_call_no_paren) (call(Deref(I_func, $1.any), anonymous_sub None (new_esp (M_ref M_hash) [ hash_ref $4; Semi_colon ] $4 $4) :: $8.any.expr)) $1 $8} /* map { { foo }; } @bar */

| term ARROW word_or_scalar parenthesized {sp_0($2); sp_0($3); sp_0($4); if $4.any.expr = [] then warn $4.pos "remove these unneeded parentheses"; new_pesp M_unknown P_tok (to_Method_call($1.any.expr, $3.any, $4.any.expr)) $1 $4} /* $foo->bar(list) */
| term ARROW word_or_scalar {sp_0($2); sp_0($3); new_pesp M_unknown P_tok (to_Method_call($1.any.expr, $3.any, [])) $1 $3} /* $foo->bar */

| NEW word { sp_n($2); new_pesp (M_ref M_unknown) P_expr (to_Method_call ($2.any, Ident(None, "new", get_pos $1), [])) $1 $2} /* new Class */
| NEW word_paren parenthesized { sp_n($2); sp_0($3); new_pesp (M_ref M_unknown) P_expr (to_Method_call($2.any, Ident(None, "new", get_pos $1), $3.any.expr)) $1 $3} /* new Class(...) */
| NEW word terminal { die_rule "you must parenthesize parameters: \"new Class(...)\" instead of \"new Class ...\"" }
| NEW word variable { die_rule "you must parenthesize parameters: \"new Class(...)\" instead of \"new Class ...\"" }

| PRINT { to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren $1.any (var_STDOUT :: [ var_dollar_ (get_pos $1) ]) $1 $1}
| PRINT argexpr {check_parenthesized_first_argexpr  $1.any $2; to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren $1.any (var_STDOUT :: $2.any.expr) $1 $2}
| PRINT_TO_SCALAR         { to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren (fst $1.any) (var_STDOUT :: [ Deref(I_scalar, Ident(None, snd $1.any, get_pos $1)) ]) $1 $1}
| PRINT_TO_SCALAR argexpr { to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren (fst $1.any) (Deref(I_scalar, Ident(None, snd $1.any, get_pos $1)) :: $2.any.expr) $1 $2}
| PRINT_TO_STAR           { to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren (fst $1.any) (Deref(I_star, Ident(None, snd $1.any, get_pos $1)) :: [ var_dollar_ (get_pos $1) ]) $1 $1}
| PRINT_TO_STAR argexpr   { to_Call_op_ (M_mixed [M_int; M_none]) P_call_no_paren (fst $1.any) (Deref(I_star, Ident(None, snd $1.any, get_pos $1)) :: $2.any.expr) $1 $2}

| hash PKG_SCOPE {sp_0($2); new_pesp M_hash P_tok (Call(Too_complex, [$1.any])) $1 $2} /* %main:: */

| terminal {$1}

expr_bracket_end:
| expr BRACKET_END { sp_p($2); new_esp (M_ref M_hash) (hash_ref $1) $1 $2 }
| expr BRACKET_END ARROW bracket_subscript {sp_p($2); sp_0($3); new_esp M_unknown_scalar (to_Deref_with(I_hash, I_scalar, hash_ref $1, $4.any)) $1 $4} /* { foo }->{Bar} */

terminal:
| word {word_alone $1}
| NUM {new_1pesp P_tok (Num($1.any, get_pos $1)) $1}
| STRING {new_1pesp P_tok (to_String true $1) $1}
| RAW_STRING {new_1pesp P_tok (to_Raw_string $1) $1}
| REVISION {new_1pesp P_tok (to_Raw_string $1) $1}
| COMMAND_STRING {to_Call_op_ (M_mixed[M_string; M_list]) P_tok "``" [to_String false $1] $1 $1}
| QUOTEWORDS {let l = List.map (fun s -> Raw_string(s, raw_pos2pos $1.pos)) (words $1.any) in new_pesp (M_tuple (repeat M_string (List.length l))) P_tok (List [ List l ]) $1 $1}
| HERE_DOC {new_1pesp P_tok (to_String false (new_1esp (fst $1.any) $1)) $1 }
| RAW_HERE_DOC {new_1pesp P_tok (Raw_string(fst $1.any, raw_pos2pos (snd $1.any))) $1}
| QR_PATTERN {to_Call_op_ M_string P_tok "qr//" (from_PATTERN $1) $1 $1}
| PATTERN {to_Call_op_ M_array P_expr "m//" (var_dollar_ (get_pos $1) :: from_PATTERN $1) $1 $1}
| PATTERN_SUBST {to_Call_op_ (M_mixed[M_none; M_int]) P_expr "s///" (var_dollar_ (get_pos $1) :: from_PATTERN_SUBST $1) $1 $1}
| diamond {new_1pesp P_expr $1.any $1}

diamond:
| LT GT {sp_0($2); to_Call_op (M_mixed[M_string; M_list]) "<>" [] $1 $2}
| LT term GT {sp_0($2); sp_0($3); to_Call_op (M_mixed[M_string; M_list]) "<>" [$2.any.expr] $1 $3}

subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {sp_0($2); sp_0($3); new_esp M_unknown (Call(Too_complex, [$3.any])) $1 $3} /* $foo::{something} */
| scalar bracket_subscript             {sp_0($2); check_scalar_subscripted $1; new_esp M_unknown_scalar (to_Deref_with(I_hash , I_scalar, from_scalar $1, $2.any               )) $1 $2} /* $foo{bar} */
| scalar arrayref                      {sp_0($2); check_scalar_subscripted $1; new_esp M_unknown_scalar (to_Deref_with(I_array, I_scalar, from_scalar $1, only_one_array_ref $2)) $1 $2} /* $array[$element] */
| term ARROW bracket_subscript         {sp_0($2); sp_0($3); check_arrow_needed $1 $2; new_esp M_unknown_scalar (to_Deref_with(I_hash , I_scalar, $1.any.expr, $3.any               )) $1 $3} /* somehref->{bar} */
| term ARROW arrayref                  {sp_0($2); sp_0($3); check_arrow_needed $1 $2; new_esp M_unknown_scalar (to_Deref_with(I_array, I_scalar, $1.any.expr, only_one_array_ref $3)) $1 $3} /* somearef->[$element] */
| term ARROW parenthesized             {sp_0($2); sp_0($3); new_esp M_unknown (to_Deref_with(I_func , I_scalar, $1.any.expr, List($3.any.expr))) $1 $3} /* $subref->(@args) */
| subscripted bracket_subscript        {sp_0($2); new_esp M_unknown_scalar  (to_Deref_with(I_hash , I_scalar, $1.any, $2.any               )) $1 $2} /* $foo->[bar]{baz} */
| subscripted arrayref                 {sp_0($2); new_esp M_unknown_scalar  (to_Deref_with(I_array, I_scalar, $1.any, only_one_array_ref $2)) $1 $2} /* $foo->[$bar][$baz] */
| subscripted parenthesized            {sp_0($2); new_esp M_unknown (to_Deref_with(I_func , I_scalar, $1.any, List($2.any.expr))) $1 $2} /* $foo->{bar}(@args) */

restricted_subscripted: /* Some kind of subscripted expression */
| variable PKG_SCOPE bracket_subscript {sp_0($2); sp_0($3); new_esp M_unknown (Call(Too_complex, [$3.any])) $1 $3} /* $foo::{something} */
| word_paren parenthesized {new_esp M_unknown (call(Deref(I_func, $1.any), $2.any.expr)) $1 $2}
| scalar bracket_subscript             {sp_0($2); check_scalar_subscripted $1; new_esp M_unknown_scalar (to_Deref_with(I_hash , I_scalar, from_scalar $1, $2.any               )) $1 $2} /* $foo{bar} */
| scalar arrayref                      {sp_0($2); check_scalar_subscripted $1; new_esp M_unknown_scalar (to_Deref_with(I_array, I_scalar, from_scalar $1, only_one_array_ref $2)) $1 $2} /* $array[$element] */
| restricted_subscripted ARROW bracket_subscript         {sp_0($2); sp_0($3); new_esp M_unknown_scalar (to_Deref_with(I_hash , I_scalar, $1.any, $3.any               )) $1 $3} /* somehref->{bar} */
| restricted_subscripted ARROW arrayref                  {sp_0($2); sp_0($3); new_esp M_unknown_scalar (to_Deref_with(I_array, I_scalar, $1.any, only_one_array_ref $3)) $1 $3} /* somearef->[$element] */
| restricted_subscripted ARROW parenthesized             {sp_0($2); sp_0($3); new_esp M_unknown (to_Deref_with(I_func , I_scalar, $1.any, List($3.any.expr))) $1 $3} /* $subref->(@args) */
| restricted_subscripted bracket_subscript        {sp_0($2); new_esp M_unknown_scalar  (to_Deref_with(I_hash , I_scalar, $1.any, $2.any               )) $1 $2} /* $foo->[bar]{baz} */
| restricted_subscripted arrayref                 {sp_0($2); new_esp M_unknown_scalar  (to_Deref_with(I_array, I_scalar, $1.any, only_one_array_ref $2)) $1 $2} /* $foo->[$bar][$baz] */
| restricted_subscripted parenthesized            {sp_0($2); new_esp M_unknown (to_Deref_with(I_func , I_scalar, $1.any, List($2.any.expr))) $1 $2} /* $foo->{bar}(@args) */

| restricted_subscripted ARROW word_or_scalar parenthesized {sp_0($2); sp_0($3); sp_0($4); if $4.any.expr = [] then warn $4.pos "remove these unneeded parentheses"; new_esp M_unknown (to_Method_call($1.any, $3.any, $4.any.expr)) $1 $4} /* $foo->bar(list) */
| restricted_subscripted ARROW word_or_scalar {sp_0($2); sp_0($3); new_esp M_unknown (to_Method_call($1.any, $3.any, [])) $1 $3} /* $foo->bar */

arrayref:
| arrayref_start ARRAYREF_END {sp_0($2); new_esp (M_ref M_array) $1.any $1 $2}
| arrayref_start expr ARRAYREF_END {sp_0($3); new_esp (M_ref M_array) ($1.any @ [$2.any.expr]) $1 $3}
| arrayref_start BRACKET expr BRACKET_END ARRAYREF_END {sp_p($2); sp_p($4); sp_0($5); new_esp (M_ref M_hash) ($1.any @ [hash_ref $3]) $1 $5}
parenthesized:
| parenthesized_start PAREN_END {sp_0_or_cr($2); new_pesp (if $1.any = [] then M_list else $1.mcontext) (if $1.any = [] then P_tok else P_paren P_comma) $1.any $1 $2}
| parenthesized_start expr PAREN_END {sp_0_or_cr($3); (if $1.any = [] then sp_0_or_cr else sp_p)($2); new_pesp (if $1.any = [] then $2.mcontext else M_list) (P_paren (if $1.any = [] then $2.any.priority else P_comma)) ($1.any @ [(if $1.any = [] then prio_lo P_loose else prio_lo_after P_comma) $2]) $1 $3}
| parenthesized_start BRACKET expr BRACKET_END PAREN_END {sp_p($4); sp_0_or_cr($5); new_pesp (if $1.any = [] then M_ref M_hash else M_list) (P_paren (if $1.any = [] then P_expr else P_comma)) ($1.any @ [hash_ref $3]) $1 $5}

arrayref_start:
| ARRAYREF {new_1esp [] $1 }
| arrayref_start BRACKET expr BRACKET_END comma {sp_p($2); sp_p($3); sp_p($4); new_esp M_special ($1.any @ [hash_ref $3]) $1 $5}
parenthesized_start:
| PAREN {new_1esp [] $1 }
| parenthesized_start BRACKET expr BRACKET_END comma {(if $1.any = [] then sp_0_or_cr else sp_p)($2); sp_p($3); sp_p($4); new_esp (M_ref M_hash) ($1.any @ [hash_ref $3]) $1 $5}

my_our: /* Things that can be "my"'d */
| my_our_paren     PAREN_END {sp_0($2); new_esp (M_mixed [ $1.mcontext ; M_none ]) (My_our(sndfst $1.any, snd $1.any, get_pos $1)) $1 $2}
| my_our_paren SCALAR_IDENT PAREN_END {check_my_our_paren $1 $2; new_esp (M_mixed [ mtuple_context_concat $1.mcontext M_unknown_scalar; M_none ]) (My_our(sndfst $1.any, snd $1.any @ [I_scalar, snd $2.any], pos_range $1 $3)) $1 $3}
| my_our_paren HASH_IDENT   PAREN_END {check_my_our_paren $1 $2; new_esp (M_mixed [ M_list ; M_none ]) (My_our(sndfst $1.any, snd $1.any @ [I_hash,   snd $2.any], pos_range $1 $3)) $1 $3}
| my_our_paren ARRAY_IDENT  PAREN_END {check_my_our_paren $1 $2; new_esp (M_mixed [ M_list ; M_none ]) (My_our(sndfst $1.any, snd $1.any @ [I_array,  snd $2.any], pos_range $1 $3)) $1 $3}
| MY_OUR SCALAR_IDENT {new_esp (M_mixed [M_unknown_scalar; M_none]) (My_our($1.any, [I_scalar, snd $2.any], get_pos $2)) $1 $2}
| MY_OUR HASH_IDENT   {new_esp (M_mixed [M_hash  ; M_none]) (My_our($1.any, [I_hash,   snd $2.any], get_pos $2)) $1 $2}
| MY_OUR ARRAY_IDENT  {new_esp (M_mixed [M_array ; M_none]) (My_our($1.any, [I_array,  snd $2.any], get_pos $2)) $1 $2}

my_our_paren:
| MY_OUR PAREN {sp_1($2); new_esp (M_tuple []) ((true, $1.any), []) $1 $2}
| my_our_paren comma {if fstfst $1.any then die_rule "syntax error"; new_esp $1.mcontext ((true, sndfst $1.any), snd $1.any) $1 $2}
| my_our_paren BAREWORD {check_my_our_paren $1 $2; if $2.any <> "undef" then die_rule "scalar expected"; new_esp (mtuple_context_concat $1.mcontext M_none) ((false, sndfst $1.any), snd $1.any @ [I_raw, $2.any]) $1 $2}
| my_our_paren SCALAR_IDENT {check_my_our_paren $1 $2; new_esp (mtuple_context_concat $1.mcontext M_unknown_scalar) ((false, sndfst $1.any), snd $1.any @ [I_scalar, snd $2.any]) $1 $2}
| my_our_paren HASH_IDENT   {check_my_our_paren $1 $2; new_esp M_list ((false, sndfst $1.any), snd $1.any @ [I_hash,   snd $2.any]) $1 $2}
| my_our_paren ARRAY_IDENT  {check_my_our_paren $1 $2; new_esp M_list ((false, sndfst $1.any), snd $1.any @ [I_array,  snd $2.any]) $1 $2}

termdo: /* Things called with "do" */
| DO term %prec UNIOP { die_rule "\"do EXPR\" not allowed" } /* do $filename */
| DO BRACKET lines BRACKET_END %prec PREC_HIGH {sp_n($2); check_block_sub $3 $4; new_esp $3.mcontext (Block $3.any) $1 $4} /* do { code */

bracket_subscript:
| BRACKET expr BRACKET_END {sp_0($1); sp_same $2 $3; check_hash_subscript $2; new_esp M_special (only_one_in_List $2) $1 $3}
| COMPACT_HASH_SUBSCRIPT {sp_0($1); new_1esp (to_Raw_string $1) $1 }

variable:
| scalar   %prec PREC_HIGH {$1}
| star     %prec PREC_HIGH {$1}
| hash     %prec PREC_HIGH {$1}
| array    %prec PREC_HIGH {$1}
| arraylen %prec PREC_HIGH {$1} /* $#x, $#{ something } */
| func     %prec PREC_HIGH {$1} /* &foo; */

word:
| bareword { $1 }
| RAW_IDENT { new_1esp (to_Ident $1) $1 }

comma: COMMA {new_esp M_special true $1 $1} | RIGHT_ARROW {sp_p($1); new_1esp false $1 }

semi_colon: SEMI_COLON {sp_0($1); $1}

word_or_scalar:
| word      {$1}
| scalar    {$1}
| word_paren {$1}
| MULT_L_STR { new_1esp (Ident(None, "x", get_pos $1)) $1 }
| FOR { new_1esp (Ident(None, $1.any, get_pos $1)) $1 }

bareword:
| NEW { new_1esp (Ident(None, "new", get_pos $1)) $1 }
| BAREWORD { new_1esp (Ident(None, $1.any, get_pos $1)) $1 }

word_paren:
| BAREWORD_PAREN { new_1esp (Ident(None, $1.any, get_pos $1)) $1 }
| RAW_IDENT_PAREN { new_1esp (to_Ident $1) $1 }
| PO_COMMENT word_paren { po_comment($1); new_esp M_special $2.any $1 $2 }


arraylen: ARRAYLEN_IDENT {new_esp M_int     (deref_arraylen (to_Ident $1)) $1 $1} | ARRAYLEN  scalar {sp_0($2); new_esp M_int     (deref_arraylen  $2.any ) $1 $1 } | ARRAYLEN  bracket_subscript {new_esp M_int     (deref_arraylen      $2.any) $1 $2}
scalar:   SCALAR_IDENT   {new_esp M_unknown_scalar  (Deref(I_scalar, to_Ident $1)) $1 $1} | DOLLAR    scalar {sp_0($2); new_esp M_unknown_scalar  (Deref(I_scalar, $2.any)) $1 $1 } | DOLLAR    bracket_subscript {new_esp M_unknown_scalar  (deref_raw I_scalar  $2.any) $1 $2} | DOLLAR BRACKET BRACKET expr BRACKET_END BRACKET_END {sp_0($2); sp_0($3); sp_p($5); sp_0($6); new_esp M_unknown_scalar (Deref(I_scalar, hash_ref $4)) $1 $6}
func:     FUNC_IDENT     {new_esp M_unknown (Deref(I_func  , to_Ident $1)) $1 $1} | AMPERSAND scalar {sp_0($2); new_esp M_unknown (Deref(I_func  , $2.any)) $1 $1 } | AMPERSAND bracket_subscript {new_esp M_unknown (deref_raw I_func    $2.any) $1 $2}
array:    ARRAY_IDENT    {new_esp M_array   (Deref(I_array , to_Ident $1)) $1 $1} | AT        scalar {sp_0($2); new_esp M_array   (Deref(I_array , $2.any)) $1 $1 } | AT        bracket_subscript {new_esp M_array   (deref_raw I_array   $2.any) $1 $2}
hash:     HASH_IDENT     {new_esp M_hash    (Deref(I_hash  , to_Ident $1)) $1 $1} | PERCENT   scalar {sp_0($2); new_esp M_hash    (Deref(I_hash  , $2.any)) $1 $1 } | PERCENT   bracket_subscript {new_esp M_hash    (deref_raw I_hash    $2.any) $1 $2}
star:     STAR_IDENT     {new_esp M_unknown (Deref(I_star  , to_Ident $1)) $1 $1} | STAR      scalar {sp_0($2); new_esp M_unknown (Deref(I_star  , $2.any)) $1 $1 } | STAR      bracket_subscript {new_esp M_unknown (deref_raw I_star    $2.any) $1 $2}

expr_or_empty: {default_esp (Block [])} | expr {new_1esp $1.any.expr $1 }

%%

prog_ref := Some prog
;;
