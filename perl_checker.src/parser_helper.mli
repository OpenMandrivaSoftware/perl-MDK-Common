val bpos : int * int
val raw_pos2pos : 'a * 'b -> string * 'a * 'b
val pos_range :
  'a * ('b * (int * int)) -> 'c * ('d * (int * int)) -> string * int * int
val sp_pos_range :
  'a * ('b * (int * int)) -> 'c * ('d * (int * int)) -> 'b * (int * int)
val get_pos : 'a * ('b * ('c * 'd)) -> string * 'c * 'd
val var_dollar_ : Types.fromparser
val var_STDOUT : Types.fromparser
val is_var_dollar_ : Types.fromparser -> bool
val is_var_number_match : Types.fromparser -> bool
val is_parenthesized : Types.fromparser -> bool
val un_parenthesize : Types.fromparser -> Types.fromparser
val un_parenthesize_full : Types.fromparser -> Types.fromparser
val not_complex : Types.fromparser -> bool
val not_simple : Types.fromparser -> bool
val string_of_Ident : Types.fromparser -> string
val context2s : Types.context -> string
val variable2s : Types.context * string -> string
val non_scalar_context : Types.context -> bool
val is_same_fromparser : Types.fromparser -> Types.fromparser -> bool
val from_scalar : Types.fromparser * 'a -> Types.fromparser
val from_array : Types.fromparser * 'a -> Types.fromparser
val msg_with_rawpos : int * int -> string -> string
val die_with_rawpos : int * int -> string -> 'a
val warn : int * int -> string -> unit
val die_rule : string -> 'a
val warn_rule : string -> unit
val debug : string -> unit
val warn_verb : int -> string -> unit
val warn_too_many_space : int -> unit
val warn_no_space : int -> unit
val warn_cr : int -> unit
val warn_space : int -> unit
val prio_less : Types.priority * Types.priority -> bool
val prio_lo :
  Types.priority ->
  (Types.priority * Types.fromparser) * ('a * (int * int)) ->
  Types.fromparser
val prio_lo_after :
  Types.priority ->
  (Types.priority * Types.fromparser) * ('a * (int * int)) ->
  Types.fromparser
val prio_lo_concat :
  (Types.priority * Types.fromparser) * ('a * (int * int)) ->
  Types.fromparser
val sp_0 : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_0_or_cr : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_1 : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_n : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_p : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_cr : 'a * (Types.spaces * (int * 'b)) -> unit
val sp_same :
  'a * (Types.spaces * (int * 'b)) ->
  'c * (Types.spaces * (int * 'd)) -> unit
val check_word_alone : Types.fromparser * 'a -> Types.fromparser
val check_parenthesized_first_argexpr :
  string ->
  ('a * Types.fromparser list) * (Types.spaces * (int * 'b)) -> unit
val check_hash_subscript :
  ('a * Types.fromparser) * ('b * (int * int)) -> unit
val check_arrow_needed :
  ('a * Types.fromparser) * 'b -> 'c * ('d * (int * int)) -> unit
val check_ternary_paras :
  Types.fromparser * Types.fromparser * Types.fromparser ->
  Types.fromparser list
val check_unneeded_var_dollar_ :
  ('a * Types.fromparser) * ('b * (int * int)) -> unit
val check_unneeded_var_dollar_not :
  ('a * Types.fromparser) * ('b * (int * int)) -> unit
val check_unneeded_var_dollar_s :
  ('a * Types.fromparser) * ('b * (int * int)) -> unit
val check_MULT_is_x : string * 'a -> unit
val check_my : string * 'a -> unit
val check_foreach : string * ('a * (int * int)) -> unit
val check_for : string * ('a * (int * int)) -> unit
val check_for_foreach :
  string * ('a * (int * int)) -> ('b * Types.fromparser) * 'c -> unit
val check_block_sub :
  Types.fromparser list * (Types.spaces * (int * int)) ->
  'a * (Types.spaces * (int * 'b)) -> unit
val check_block_ref :
  Types.fromparser list * (Types.spaces * (int * int)) ->
  'a * (Types.spaces * (int * 'b)) -> unit
val check_my_our_paren : ((bool * 'a) * 'b) * 'c -> unit
val only_one : Types.fromparser list * ('a * (int * int)) -> Types.fromparser
val only_one_in_List :
  ('a * Types.fromparser) * ('b * (int * int)) -> Types.fromparser
val is_only_one_in_List : Types.fromparser list -> bool
val is_not_a_scalar : Types.fromparser -> bool
val maybe_to_Raw_string : Types.fromparser -> Types.fromparser
val to_List : Types.fromparser list -> Types.fromparser
val deref_arraylen : Types.fromparser -> Types.fromparser
val to_Ident :
  (string option * string) * ('a * (int * int)) -> Types.fromparser
val to_Raw_string : string * ('a * (int * int)) -> Types.fromparser
val to_Method_call :
  Types.fromparser * Types.fromparser * Types.fromparser list ->
  Types.fromparser
val to_Deref_with :
  Types.context * Types.context * Types.fromparser * Types.fromparser ->
  Types.fromparser
val to_Local :
  ('a * Types.fromparser) * ('b * (int * int)) -> Types.fromparser
val op : 'a -> 'b -> 'c * 'd -> 'a * ((unit * 'd) * 'b)
val op_p :
  'a ->
  'b ->
  'c * (Types.spaces * (int * 'd)) ->
  'a * ((unit * (Types.spaces * (int * 'd))) * 'b)
val sub_declaration :
  Types.fromparser * string -> Types.fromparser list -> Types.fromparser
val anonymous_sub : Types.fromparser list -> Types.fromparser
val cook_call_op :
  string * Types.fromparser list * (int * int) -> Types.fromparser
val call_op_ :
  ('a * (('b * (Types.spaces * (int * 'c))) * string)) *
  ('d * (Types.spaces * (int * 'e))) * Types.fromparser list ->
  'f * (int * int) -> ('a * Types.fromparser) * ('f * (int * int))
val to_Call_op :
  string * Types.fromparser list ->
  'a * (int * int) -> Types.fromparser * ('a * (int * int))
val to_Call_op_ :
  'a * string * Types.fromparser list ->
  'b * (int * int) -> ('a * Types.fromparser) * ('b * (int * int))
val followed_by_comma :
  ('a * Types.fromparser list) * 'b -> bool * 'c -> Types.fromparser list
val call_func :
  bool -> Types.fromparser * Types.fromparser list -> Types.fromparser
val call : Types.fromparser * Types.fromparser list -> Types.fromparser
val call_one_scalar_para :
  string * ('a * (int * int)) ->
  Types.fromparser list -> Types.priority * Types.fromparser
val call_op_if_infix :
  Types.fromparser ->
  Types.fromparser ->
  'a * (int * int) -> Types.fromparser * ('a * (int * int))
val call_op_unless_infix :
  Types.fromparser ->
  Types.fromparser ->
  'a * (int * int) -> Types.fromparser * ('a * (int * int))
val current_lexbuf : Lexing.lexbuf option ref
val list2tokens : ((int * int) * 'a) list -> Lexing.lexbuf -> 'a
val parse_tokens :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b list) ->
  ((int * int) * 'a) list -> Lexing.lexbuf option -> 'b list
val parse_interpolated :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  (string * ((int * int) * 'a) list) list -> (string * Types.fromparser) list
val to_String :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  bool ->
  (string * ((int * int) * 'a) list) list * ('b * (int * int)) ->
  Types.fromparser
val from_PATTERN :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  ((string * ((int * int) * 'a) list) list * string) * ('b * (int * int)) ->
  Types.fromparser list
val from_PATTERN_SUBST :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  ((string * ((int * int) * 'a) list) list *
   (string * ((int * int) * 'a) list) list * string) *
  ('b * (int * int)) -> Types.fromparser list
