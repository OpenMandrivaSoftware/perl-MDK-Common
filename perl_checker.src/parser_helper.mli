val bpos : int * int
val raw_pos2pos : 'a * 'b -> string * 'a * 'b
val raw_pos_range :
  'a Types.any_spaces_pos -> 'b Types.any_spaces_pos -> int * int
val pos_range :
  'a Types.any_spaces_pos -> 'b Types.any_spaces_pos -> string * int * int
val get_pos : 'a Types.any_spaces_pos -> string * int * int
val get_pos_start : 'a Types.any_spaces_pos -> int
val get_pos_end : 'a Types.any_spaces_pos -> int
val var_dollar_ : Types.pos -> Types.fromparser
val var_STDOUT : Types.fromparser
val new_any :
  Types.maybe_context ->
  'a -> Types.spaces -> int * int -> 'a Types.any_spaces_pos
val new_any_ : 'a -> Types.spaces -> int * int -> 'a Types.any_spaces_pos
val new_esp :
  Types.maybe_context ->
  'a ->
  'b Types.any_spaces_pos ->
  'c Types.any_spaces_pos -> 'a Types.any_spaces_pos
val new_1esp : 'a -> 'b Types.any_spaces_pos -> 'a Types.any_spaces_pos
val new_pesp :
  Types.maybe_context ->
  Types.priority ->
  'a ->
  'b Types.any_spaces_pos ->
  'c Types.any_spaces_pos -> 'a Types.prio_anyexpr Types.any_spaces_pos
val new_1pesp :
  Types.priority ->
  'a -> 'b Types.any_spaces_pos -> 'a Types.prio_anyexpr Types.any_spaces_pos
val default_esp : 'a -> 'a Types.any_spaces_pos
val default_pesp :
  Types.priority -> 'a -> 'a Types.prio_anyexpr Types.any_spaces_pos
val split_name_or_fq_name : string -> string option * string
val is_var_dollar_ : Types.fromparser -> bool
val is_var_number_match : Types.fromparser -> bool
val non_scalar_context : Types.context -> bool
val is_scalar_context : Types.context -> bool
val is_not_a_scalar : Types.fromparser -> bool
val is_a_scalar : Types.fromparser -> bool
val is_a_string : Types.fromparser -> bool
val is_parenthesized : Types.fromparser -> bool
val un_parenthesize : Types.fromparser -> Types.fromparser
val un_parenthesize_full : Types.fromparser -> Types.fromparser
val un_parenthesize_full_l : Types.fromparser list -> Types.fromparser list
val is_always_true : Types.fromparser -> bool
val is_always_false : Types.fromparser -> bool
val is_lvalue : Types.fromparser -> bool
val not_complex : Types.fromparser -> bool
val not_simple : Types.fromparser -> bool
val context2s : Types.context -> string
val variable2s : Types.context * string -> string
val string_of_fromparser : Types.fromparser -> string
val lstring_of_fromparser : Types.fromparser list -> string
val lstring_of_fromparser_parentheses : Types.fromparser list -> string
val is_same_fromparser : Types.fromparser -> Types.fromparser -> bool
val from_scalar : Types.fromparser Types.any_spaces_pos -> Types.fromparser
val from_array : Types.fromparser Types.any_spaces_pos -> Types.fromparser
val get_pos_from_expr : Types.fromparser -> Types.pos
val msg_with_rawpos : int * int -> string -> string
val die_with_rawpos : int * int -> string -> 'a
val warn : Types.warning list -> int * int -> string -> unit
val die_rule : string -> 'a
val warn_rule : Types.warning list -> string -> unit
val warn_verb : Types.warning list -> int -> string -> unit
val warn_too_many_space : int -> unit
val warn_no_space : int -> unit
val warn_cr : int -> unit
val warn_space : int -> unit
val prio_less : Types.priority * Types.priority -> bool
val prio_lo_check :
  Types.priority -> Types.priority -> int * int -> Types.fromparser -> unit
val prio_lo :
  Types.priority ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val prio_lo_after :
  Types.priority ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val prio_lo_concat :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val hash_ref :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val sp_0 : 'a Types.any_spaces_pos -> unit
val sp_0_or_cr : 'a Types.any_spaces_pos -> unit
val sp_1 : 'a Types.any_spaces_pos -> unit
val sp_n : 'a Types.any_spaces_pos -> unit
val sp_p : 'a Types.any_spaces_pos -> unit
val sp_cr : 'a Types.any_spaces_pos -> unit
val sp_same : 'a Types.any_spaces_pos -> 'b Types.any_spaces_pos -> unit
val function_to_context : bool -> string -> Types.maybe_context
val word_alone :
  Types.fromparser Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val check_parenthesized_first_argexpr :
  string ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_parenthesized_first_argexpr_with_Ident :
  Types.fromparser ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_hash_subscript :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_arrow_needed : 'a Types.any_spaces_pos -> Types.fromparser -> unit
val check_scalar_subscripted : Types.fromparser Types.any_spaces_pos -> unit
val negatable_ops : (string * string) list
val check_negatable_expr :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_ternary_paras :
  Types.fromparser * Types.fromparser * Types.fromparser ->
  Types.fromparser list
val check_unneeded_var_dollar_ :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_unneeded_var_dollar_not :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_unneeded_var_dollar_s :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_my : string Types.any_spaces_pos -> unit
val check_foreach : string Types.any_spaces_pos -> unit
val check_for : string Types.any_spaces_pos -> unit
val check_for_foreach :
  string Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val check_block_expr :
  bool ->
  Types.fromparser ->
  'a Types.any_spaces_pos -> 'b Types.any_spaces_pos -> unit
val check_block_lines :
  (Types.fromparser list * bool) Types.any_spaces_pos ->
  'a Types.any_spaces_pos -> unit
val check_unless_else :
  'a list Types.any_spaces_pos -> 'b list Types.any_spaces_pos -> unit
val check_my_our_paren :
  ((bool * 'a) * 'b list) Types.any_spaces_pos ->
  'c Types.any_spaces_pos -> unit
val check_simple_pattern : Types.fromparser list -> unit
val only_one : Types.fromparser list Types.any_spaces_pos -> Types.fromparser
val only_one_array_ref :
  Types.fromparser list Types.any_spaces_pos -> Types.fromparser
val only_one_in_List :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val is_only_one_in_List : Types.fromparser list -> bool
val maybe_to_Raw_string : Types.fromparser -> Types.fromparser
val to_List : Types.fromparser list -> Types.fromparser
val deref_arraylen : Types.fromparser -> Types.fromparser
val deref_raw : Types.context -> Types.fromparser -> Types.fromparser
val to_Ident :
  (string option * string) Types.any_spaces_pos -> Types.fromparser
val to_Raw_string : string Types.any_spaces_pos -> Types.fromparser
val to_Method_call :
  Types.fromparser * Types.fromparser * Types.fromparser list ->
  Types.fromparser
val to_Deref_with :
  Types.context * Types.context * Types.fromparser * Types.fromparser ->
  Types.fromparser
val to_Deref_with_arrow :
  'a Types.any_spaces_pos ->
  Types.context * Types.context * Types.fromparser * Types.fromparser ->
  Types.fromparser
val lines_to_Block :
  (Types.fromparser list * bool) Types.any_spaces_pos ->
  'a Types.any_spaces_pos -> Types.fromparser
val to_Local :
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser
val sub_declaration :
  Types.fromparser * string option ->
  Types.fromparser list -> Types.sub_declaration_kind -> Types.fromparser
val anonymous_sub :
  string option ->
  (Types.fromparser list * bool) Types.any_spaces_pos ->
  'a Types.any_spaces_pos -> Types.fromparser
val call_with_same_para_special : Types.fromparser -> Types.fromparser
val remove_call_with_same_para_special : Types.fromparser -> Types.fromparser
val check_My_under_condition : string -> Types.fromparser -> unit
val cook_call_op :
  string -> Types.fromparser list -> int * int -> Types.fromparser
val to_Call_op :
  Types.maybe_context ->
  string ->
  Types.fromparser list ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos -> Types.fromparser Types.any_spaces_pos
val to_Call_op_ :
  Types.maybe_context ->
  Types.priority ->
  string ->
  Types.fromparser list ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val to_Call_assign_op_ :
  Types.maybe_context ->
  Types.priority ->
  string ->
  Types.fromparser ->
  Types.fromparser ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val followed_by_comma :
  Types.fromparser list -> bool -> Types.fromparser list
val pot_strings : (string, (string * int * int) * string list) Hashtbl.t
val po_comments : string list ref
val po_comment : string Types.any_spaces_pos -> unit
val check_format_a_la_printf : string -> int -> Types.maybe_context list
val generate_pot : string -> unit
val check_system_call : string list -> unit
val call_raw :
  bool -> Types.fromparser * Types.fromparser list -> Types.fromparser
val call : Types.fromparser * Types.fromparser list -> Types.fromparser
val check_return :
  Types.fromparser Types.any_spaces_pos ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos -> unit
val call_and_context :
  Types.fromparser * Types.fromparser list ->
  bool ->
  Types.priority ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val call_no_paren :
  Types.fromparser Types.any_spaces_pos ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val call_with_paren :
  Types.fromparser Types.any_spaces_pos ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val call_func :
  Types.fromparser Types.any_spaces_pos ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
val call_one_scalar_para :
  string Types.any_spaces_pos ->
  Types.fromparser list ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
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
  (string * ((int * int) * 'a) list) list Types.any_spaces_pos ->
  Types.fromparser
val from_PATTERN :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  ((string * ((int * int) * 'a) list) list * string) Types.any_spaces_pos ->
  Types.fromparser list
val from_PATTERN_SUBST :
  ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> Types.fromparser list) ->
  ((string * ((int * int) * 'a) list) list *
   (string * ((int * int) * 'a) list) list * string)
  Types.any_spaces_pos -> Types.fromparser list
val mcontext2s : Types.maybe_context -> string
val mcontext_lower : Types.maybe_context -> Types.maybe_context -> bool
val mcontext_is_scalar : Types.maybe_context -> bool
val mcontext_to_scalar : Types.maybe_context -> Types.maybe_context
val mcontext_merge_raw :
  Types.maybe_context -> Types.maybe_context -> Types.maybe_context option
val mcontext_lmerge_add :
  Types.maybe_context list -> Types.maybe_context -> Types.maybe_context list
val mcontext_lmerge : Types.maybe_context list -> Types.maybe_context
val mcontext_merge :
  Types.maybe_context -> Types.maybe_context -> Types.maybe_context
val mcontext_lmaybe :
  'a list Types.any_spaces_pos -> Types.maybe_context list
val mcontext_check_raw : Types.maybe_context -> Types.maybe_context -> unit
val mcontext_check :
  Types.maybe_context ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos -> unit
val mcontext_check_unop_l :
  Types.maybe_context ->
  Types.fromparser list Types.prio_anyexpr Types.any_spaces_pos -> unit
val mcontext_check_non_none : 'a Types.any_spaces_pos -> unit
val mcontext_check_none :
  string -> Types.fromparser list -> 'a Types.any_spaces_pos -> unit
val mcontext_float_or_int : Types.maybe_context list -> Types.maybe_context
val mcontext_op_assign :
  'a Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.maybe_context
val mtuple_context_concat :
  Types.maybe_context -> Types.maybe_context -> Types.maybe_context
val call_op_if_infix :
  Types.fromparser ->
  Types.fromparser ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos -> Types.fromparser Types.any_spaces_pos
val call_op_unless_infix :
  Types.fromparser ->
  Types.fromparser ->
  'a Types.any_spaces_pos ->
  'b Types.any_spaces_pos -> Types.fromparser Types.any_spaces_pos
val symops :
  Types.priority ->
  Types.maybe_context ->
  Types.maybe_context ->
  string ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  'a Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos ->
  Types.fromparser Types.prio_anyexpr Types.any_spaces_pos
