val bpos : int * int
val msg_with_pos : int * int -> string -> string
val die_with_pos : int * int -> string -> 'a
val warn : int * int -> string -> unit
val die_rule : string -> 'a
val debug : string -> unit
val raw_pos2pos : 'a * 'b -> string * 'a * 'b
val get_pos : 'a * ('b * ('c * 'd)) -> string * 'c * 'd
val warn_too_many_space : int -> unit
val warn_no_space : int -> unit
val warn_cr : int -> unit
val warn_space : int -> unit
val sp_0 : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_0_or_cr : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_1 : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_n : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_p : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_cr : 'a * (Types.spaces * (int * 'b)) -> unit
val not_complex : Types.fromparser -> bool
val string_of_Ident : Types.fromparser -> string
val check_parenthesized_first_argexpr :
  string -> Types.fromparser list * (Types.spaces * (int * 'a)) -> unit
val check_foreach : string * ('a * (int * int)) -> unit
val check_for : string * ('a * (int * int)) -> unit
val check_no_paren : string -> Types.fromparser * ('a * (int * int)) -> unit
val to_Ident :
  (string option * string) * ('a * (int * int)) -> Types.fromparser
val to_String : string * ('a * (int * int)) -> Types.fromparser
val only_one : Types.fromparser list * ('a * (int * int)) -> Types.fromparser
val only_one_in_List :
  Types.fromparser * ('a * (int * int)) -> Types.fromparser
val array_ident_to_hash_ident :
  Types.fromparser * ('a * (int * int)) -> Types.fromparser
val from_PATTERN :
  (string * string) * ('a * (int * int)) -> Types.fromparser list
val from_PATTERN_SUBST :
  (string * string * string) * ('a * (int * int)) -> Types.fromparser list
val to_List : Types.fromparser list -> Types.fromparser
val sub_declaration :
  Types.fromparser * string -> Types.fromparser list -> Types.fromparser
val var_dollar_ : Types.fromparser
val var_STDOUT : Types.fromparser
