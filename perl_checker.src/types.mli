exception TooMuchRParen

type raw_pos = int * int

type pos = string * int * int

type spaces =
  | Space_0
  | Space_1
  | Space_n
  | Space_cr
  | Space_none

type context = I_scalar | I_hash | I_array | I_func | I_raw | I_star | I_arraylen

type fromparser = 
   | Ident of string option * string * pos
   | Num of string * pos
   | Raw_string of string * pos
   | String of (string * fromparser) list * pos

   | Ref of context * fromparser
   | Deref of context * fromparser
   | Deref_with of context * fromparser * fromparser

   | Diamond of fromparser option
   | Binop of string * fromparser * fromparser
   | If_then_else of string * (fromparser * fromparser) list * fromparser option

   | List of fromparser list
   | Block of fromparser list

   | Call_op of string * fromparser list
   | Call of fromparser * fromparser list
   | CallP of fromparser * fromparser list
   | Method_call of fromparser * fromparser * fromparser list
   | Method_callP of fromparser * fromparser * fromparser list

   | Anonymous_sub of fromparser list
   | My of fromparser
   | Local of fromparser
   | Use of fromparser * fromparser list
   | Sub_declaration of fromparser * string * fromparser list (* name, prototype, body *)
   | Package of fromparser
   | Label of string

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
| P_tight_and
| P_tight_or
| P_ternary
| P_assign
| P_call_no_paren
| P_comma
| P_and
| P_or
| P_loose

| P_paren_wanted of priority
| P_paren of priority
