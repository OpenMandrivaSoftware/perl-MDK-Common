exception TooMuchRParen

type pos = string * int * int

type ident_type = I_scalar | I_hash | I_array | I_func | I_raw | I_star

type fromparser = 
   | Ident of ident_type * string option * string * pos

   | Num of float * pos
   | String of string * pos
   | Nil

   | Binary of string * fromparser * fromparser
   | If_then_else of string * (fromparser * fromparser) list * fromparser option

