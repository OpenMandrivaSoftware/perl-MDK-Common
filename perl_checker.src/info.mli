val lines_starts : (string, int list) Hashtbl.t
val current_file_lines_starts : int list ref
val current_file_current_line : int ref
val current_file : string ref
val start_a_new_file : string -> unit
val add_a_file : string -> int list -> unit
val get_lines_starts_for_file : string -> int list
val file_to_absolute_file : string -> string
val absolute_file_to_file : string -> string
val raw_pos2raw_line : string -> int -> int * int
val pos2line : string * int * int -> string * int * int * int
val pos2s : string * int * int -> string
val pos2sfull : string * int * int -> string
val is_on_same_line : string -> int * int -> bool
val is_on_same_line_current : int * int -> bool
val pos2sfull_current : int -> int -> string
