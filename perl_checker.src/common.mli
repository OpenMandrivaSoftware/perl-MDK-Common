exception Found
exception Not_comparable
exception GraphSort_circular_deps
type ('a, 'b) either = Left of 'a | Right of 'b
and ('a, 'b) or_option = Or_some of 'a | Or_error of 'b
val internal_error : string -> 'a
val id : 'a -> 'a
val double : 'a -> 'a * 'a
val swap : 'a * 'b -> 'b * 'a
val safe_tl : 'a list -> 'a list
val fstfst : ('a * 'b) * 'c -> 'a
val sndfst : ('a * 'b) * 'c -> 'b
val fstsnd : 'a * ('b * 'c) -> 'b
val sndsnd : 'a * ('b * 'c) -> 'c
val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val ter3 : 'a * 'b * 'c -> 'c
val sndter3 : 'a * 'b * 'c -> 'b * 'c
val o : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val is_int : float -> bool
val uncons : 'a list -> 'a * 'a list
val has_env : string -> bool
val some : 'a option -> 'a
val some_or : 'a option -> 'a -> 'a
val option2l : 'a option -> 'a list
val prefer_some : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
val collect_accu : ('a -> 'b list) -> 'b list -> 'a list -> 'b list
val collect : ('a -> 'b list) -> 'a list -> 'b list
val merge_some : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
val uniq : 'a list -> 'a list
val uniq_ : ('a -> 'a -> bool) -> 'a list -> 'a list
val non_uniq : 'a list -> 'a list
val member_ : ('a -> 'b -> bool) -> 'a -> 'b list -> bool
val find_some : ('a -> 'b option) -> 'a list -> 'b
val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val find_index : 'a -> 'a list -> int
val find_some_ : ('a -> 'b option) -> 'a list -> 'b option
val fpartition : ('a -> 'b option) -> 'a list -> 'b list * 'a list
val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
val keep_best : ('a * 'a -> 'a option) -> 'a list -> 'a list
val keep_bests : ('a * 'a -> 'a option) -> 'a list -> 'a list
val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val for_all2_ : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val maxl : 'a list -> 'a
val stack2list : 'a Stack.t -> 'a list
val stack_exists : ('a -> bool) -> 'a Stack.t -> bool
val queue2list : 'a Queue.t -> 'a list
val fix_point : ('a -> 'a) -> 'a -> 'a
val fix_point_withenv : ('a -> 'b -> 'b * 'a) -> 'a -> 'b -> 'b * 'a
val fix_point_ : int -> ('a -> 'a) -> 'a -> 'a * int
val group_by_2 : 'a list -> ('a * 'a) list
val do0_withenv :
  (('a -> unit) -> 'b -> 'c) -> ('d -> 'a -> 'd) -> 'd -> 'b -> 'd
val do0_withenv2 :
  (('a -> 'b -> unit) -> 'c -> 'd) ->
  ('e -> 'a -> 'b -> 'e) -> 'e -> 'c -> 'e
val do_withenv :
  (('a -> 'b) -> 'c -> 'd) -> ('e -> 'a -> 'b * 'e) -> 'e -> 'c -> 'd * 'e
val do2_withenv :
  (('a -> 'b -> 'c) -> 'd -> 'e -> 'f) ->
  ('g -> 'a -> 'b -> 'c * 'g) -> 'g -> 'd -> 'e -> 'f * 'g
val do_collect :
  (('a -> 'b -> unit) -> 'c -> 'd) -> ('a -> 'b -> 'e list) -> 'c -> 'e list
val map_withitself : ('a list -> 'a -> 'a) -> 'a list -> 'a list
val map_t2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val map_t3 : ('a -> 'b) -> 'a * 'a * 'a -> 'b * 'b * 'b
val map_option : ('a -> 'b) -> 'a option -> 'b option
val map_optionoption : ('a -> 'b option) -> 'a option -> 'b option
val t2_option2option_t2 : 'a option * 'b option -> ('a * 'b) option
val l_option2option_l : 'a option list -> 'a list option
val map_option_env : ('a -> 'b) -> 'a option * 'c -> 'b option * 'c
val t2_to_list : 'a * 'a -> 'a list
val t3_to_list : 'a * 'a * 'a -> 'a list
val if_some : bool -> 'a -> 'a option
val fold_left_option : ('a -> 'b -> 'a option) -> 'a -> 'b list -> 'a option
val collect_some_withenv :
  ('a -> 'b -> 'c option * 'a) -> 'a -> 'b list -> 'c list * 'a
val for_all_option_withenv :
  ('a list -> 'b) ->
  ('c -> 'd -> 'a option * 'c) -> 'c -> 'd list -> 'b option * 'c
val for_all2_option_withenv :
  ('a list -> 'b) ->
  ('c -> 'd -> 'e -> 'a option * 'c) ->
  'c -> 'd list -> 'e list -> 'b option * 'c
val map_or_option : ('a -> 'b) -> ('a, 'c) or_option -> ('b, 'c) or_option
val map_index : ('a -> int -> 'b) -> 'a list -> 'b list
val filter_index : ('a -> int -> bool) -> 'a list -> 'a list
val iter_index : ('a -> int -> 'b) -> 'a list -> unit
val map_fst : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val map_snd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
val map_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a
val find_withenv : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> 'b * 'a
val filter_withenv : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> 'b list * 'a
val exists_withenv : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> bool * 'a
val map_t2_withenv : ('a -> 'b -> 'c * 'a) -> 'a -> 'b * 'b -> ('c * 'c) * 'a
val for_all_withenv : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> bool * 'a
val collect_withenv :
  ('a -> 'b -> 'c list * 'a) -> 'a -> 'b list -> 'c list * 'a
val partition_either_withenv :
  ('a -> 'b -> ('c, 'd) either * 'a) ->
  'a -> 'b list -> ('c list * 'd list) * 'a
val map2_withenv :
  ('a -> 'b -> 'c -> 'd * 'a) -> 'a -> 'b list -> 'c list -> 'd list * 'a
val for_all2_withenv :
  ('a -> 'b -> 'c -> bool * 'a) -> 'a -> 'b list -> 'c list -> bool * 'a
val take : int -> 'a list -> 'a list
val last_n : int -> 'a list -> 'a list
val last : 'a list -> 'a
val skipfirst : 'a -> 'a list -> 'a list
val removelast : 'a list -> 'a list
val split_last : 'a list -> 'a list * 'a
val iter_assoc_val : ('a -> unit) -> ('b * 'a) list -> unit
val map_assoc_val : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val assoc_or_fail : 'a -> ('a * 'b) list -> 'b
val assoc_by : ('a -> 'b -> bool) -> 'a -> ('b * 'c) list -> 'c
val update_assoc_by :
  ('a -> 'b -> bool) -> ('c -> 'c) -> 'a -> ('b * 'c) list -> ('b * 'c) list
val update_assoc : ('a -> 'a) -> 'b -> ('b * 'a) list -> ('b * 'a) list
val update_assoc_by_with_default :
  'a ->
  ('b -> 'b -> bool) -> ('a -> 'a) -> 'b -> ('b * 'a) list -> ('b * 'a) list
val update_all_assoc_by :
  ('a -> 'b -> bool) -> ('c -> 'c) -> 'a -> ('b * 'c) list -> ('b * 'c) list
val rassoc : 'a -> ('b * 'a) list -> 'b
val all_assoc : 'a -> ('a * 'b) list -> 'b list
val all_assoc_by : ('a -> 'b -> bool) -> 'a -> ('b * 'c) list -> 'c list
val prepare_want_all_assoc : ('a * 'b) list -> ('a * 'b list) list
val prepare_want_all_assoc_by :
  ('a -> 'a -> bool) -> ('a * 'a) list -> ('a * 'a list) list
val prepare_want_all_assoc_by_ :
  ('a -> 'a -> bool) ->
  ('b -> 'b -> bool) -> ('a * 'b) list -> ('a * 'b list) list
val count_uniq : 'a list -> ('a * int) list
val repeat : 'a -> int -> 'a list
val inits : 'a list -> 'a list list
val tails : 'a list -> 'a list list
val apply : ('a -> 'b) -> 'a -> 'b
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val filter2 : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list * 'b list
val break_at : ('a -> bool) -> 'a list -> 'a list * 'a list
val break : 'a -> 'a list -> 'a list * 'a list
val break_at_indice : int -> 'a list -> 'a list * 'a list
val rev_nth : 'a -> 'a list -> int
val getset_nth : 'a list -> int -> ('a -> 'a) -> 'a list
val set_nth : 'a list -> int -> 'a -> 'a list
val adjustModDown : int -> int -> int
val adjustModUp : int -> int -> int
val hashtbl_find : ('a -> 'b -> bool) -> ('a, 'b) Hashtbl.t -> 'a
val hashtbl_map : ('a -> 'b -> 'b) -> ('a, 'b) Hashtbl.t -> unit
val hashtbl_values : ('a, 'b) Hashtbl.t -> 'b list
val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a list
val hashtbl_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hashtbl_collect : ('a -> 'b -> 'c list) -> ('a, 'b) Hashtbl.t -> 'c list
val hashtbl_exists : ('a -> 'b -> bool) -> ('a, 'b) Hashtbl.t -> bool
val memoize : ('a -> 'b) -> 'a -> 'b
val array_shift : 'a array -> 'a array
val array_last_n : int -> 'a array -> 'a array
val array_collect : ('a -> 'b list) -> 'a array -> 'b list
val lvector_product : 'a list list -> 'a list list
val vector_product2 : 'a list -> 'a list -> ('a * 'a) list
val transpose : 'a list list -> 'a list list
val range : int -> int -> int list
val sum : int list -> int
val filter_some_with : ('a -> 'b option) -> 'a list -> 'b list
val filter_some : 'a option list -> 'a list
val difference : 'a list -> 'a list -> 'a list
val difference_ : ('a -> 'b -> bool) -> 'b list -> 'a list -> 'b list
val intersection_by : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
val intersection_and_differences :
  ('a -> 'b -> bool) -> 'b list -> 'a list -> 'a list * 'b list * 'a list
val triangularize : 'a list -> ('a * 'a list) list
val diagonalize : 'a list -> ('a * 'a list) list
val list_of_nonempty_sublists : 'a list -> 'a list list
val graph_is_sorted_by : ('a -> 'b -> bool) -> ('b * 'a list) list -> bool
val graph_closure_by :
  ('a -> 'a -> bool) ->
  ('a * 'a list) list -> (('a * 'a list) list, 'a * 'a) or_option
val graph_sort_by :
  ('a -> 'a -> bool) ->
  ('a * 'a list) list -> (('a * 'a list) list, 'a * 'a) or_option
val int_sort : int list -> int list
val str_begins_with : string -> string -> bool
val strstr : string -> string -> int
val str_contains : string -> string -> bool
val str_ends_with : string -> string -> bool
val chop : string -> string
val chomps : string -> string
val times : 'a -> int -> 'a list
val skip_n_char_ : int -> int -> string -> string
val skip_n_char : int -> string -> string
val non_index_from : string -> int -> char -> int
val non_index : string -> char -> int
val non_rindex_from : string -> int -> char -> int
val non_rindex : string -> char -> int
val explode_string : string -> char list
val count_matching_char : string -> char -> int
val is_uppercase : char -> bool
val is_lowercase : char -> bool
val char_is_alphanumerical : char -> bool
val char_is_alphanumerical_ : char -> bool
val char_is_alpha : char -> bool
val char_is_number : char -> bool
val count_chars_in_string : string -> char -> int
val string_fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
val string_forall_with : (char -> bool) -> int -> string -> bool
val starts_with_non_lowercase : string -> bool
val fold_lines : ('a -> string -> 'a) -> 'a -> in_channel -> 'a
val readlines : in_channel -> string list
val split_at : char -> string -> string list
val split_at2 : char -> char -> string -> string list
val words : string -> string list
val to_CamelCase : string -> string option
val concat_symlink : string -> string -> string
val expand_symlinks : string -> string
val mtime : string -> int
val updir : string -> int -> string
val string_of_ref : 'a ref -> string
val print_endline_flush_quiet : bool ref
val print_endline_flush : string -> unit
val print_endline_flush_always : string -> unit
val is_int : float -> bool
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int
val compare_best : int -> int -> int
val combine_comparison_list : int list -> int
val min_with_cmp : ('a -> 'a -> bool) -> 'a -> 'a -> 'a
val max_with_cmp : ('a -> 'a -> bool) -> 'a -> 'a -> 'a
val fold_left2_compare :
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val exists_compare : ('a -> 'b) -> 'a list -> 'b
val forall_compare : ('a -> int) -> 'a list -> int
val forall2_compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int
val exists2_compare : int -> ('a -> 'b -> int) -> 'a list -> 'b list -> int
val compare_sorted_sets : ('a -> 'b -> bool) -> 'a list -> 'b list -> int
val scan_list_while_modifying :
  ('a list -> 'a list -> 'a -> ('a list * 'a list) option) ->
  'a list -> 'a list
val bools2compare : bool * bool -> int
val lpush : 'a list ref -> 'a -> unit
module OrderedString : sig type t = string val compare : 'a -> 'a -> int end
module StringSet :
  sig
    type elt = OrderedString.t
    and t = Set.Make(OrderedString).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
  end
val stringSet_to_list : StringSet.t -> StringSet.elt list
val stringSet_add : StringSet.t -> StringSet.elt -> StringSet.t
val stringSet_difference : StringSet.t -> StringSet.t -> StringSet.t
val list_to_StringSet : StringSet.elt list -> StringSet.t
