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
   | String of string * pos

   | Ref of context * fromparser
   | Deref of context * fromparser
   | Deref_with of context * fromparser * fromparser

   | Diamond of fromparser option
   | Binop of string * fromparser * fromparser
   | If_then_else of string * (fromparser * fromparser) list * fromparser option

   | List of fromparser list
   | Block of fromparser list

   | Call of fromparser * fromparser list
   | Call_op of string * fromparser list
   | Method_call of fromparser * fromparser * fromparser list

   | Anonymous_sub of fromparser
   | My of fromparser
   | Local of fromparser
   | Use of fromparser * fromparser list
   | Sub_declaration of fromparser * string * fromparser list (* name, prototype, body *)
   | Package of fromparser
   | Label of string

   | Too_complex
   | Semi_colon
