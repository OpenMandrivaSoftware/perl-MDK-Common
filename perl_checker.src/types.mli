exception TooMuchRParen

type raw_pos = int * int

type pos = string * int * int

type spaces =
  | Space_0
  | Space_1
  | Space_n
  | Space_cr
  | Space_none

type context = I_scalar | I_hash | I_array | I_func | I_raw | I_star

type maybe_context =
  | M_none

  (* scalars *)
  | M_bool | M_int | M_float
  | M_revision
  | M_string
  | M_ref of maybe_context
  | M_undef
  | M_unknown_scalar

  | M_tuple of maybe_context list
  | M_list
  | M_array
  | M_hash
  | M_sub

  | M_special
  | M_unknown
  | M_mixed of maybe_context list

type sub_declaration_kind = Real_sub_declaration | Glob_assign

type fromparser = 
   | Undef
   | Ident of string option * string * pos
   | Num of string * pos
   | Raw_string of string * pos
   | String of (string * fromparser) list * pos

   | Ref of context * fromparser
   | Deref of context * fromparser
   | Deref_with of context * context * fromparser * fromparser (* from_context, to_context, ref, para *)

   | Diamond of fromparser option

   | List of fromparser list
   | Block of fromparser list

   | Call_op of string * fromparser list * pos
   | Call of fromparser * fromparser list
   | Method_call of fromparser * fromparser * fromparser list

   | Anonymous_sub of string option * fromparser * pos (* prototype, expr, pos *)
   | My_our of string * (context * string) list * pos
   | Use of fromparser * fromparser list
   | Sub_declaration of fromparser * string option * fromparser * sub_declaration_kind (* name, prototype, body, kind *)
   | Package of fromparser
   | Label of string
   | Perl_checker_comment of string * pos

   | Too_complex
   | Semi_colon

type priority = 
| P_tok
| P_tight
| P_mul
| P_add
| P_cmp
| P_eq
| P_expr
| P_bit
| P_tight_and
| P_tight_or
| P_ternary
| P_assign
| P_comma
| P_call_no_paren
| P_and
| P_or
| P_loose

| P_paren_wanted of priority
| P_paren of priority

| P_none

type 'a any_spaces_pos = {
    any : 'a ;
    spaces : spaces ;
    pos : int * int ;
    mcontext : maybe_context ;
  }

type 'a prio_anyexpr = {
    priority : priority ;
    expr : 'a
  }

type prio_expr_spaces_pos = fromparser prio_anyexpr any_spaces_pos
type prio_lexpr_spaces_pos = fromparser list prio_anyexpr any_spaces_pos
