val lines_starts : (string * int list ref) list ref
val current_file_lines_starts : int list ref
val current_file : string ref
val start_a_new_file : string -> unit
val pos2line : string * int * int -> string * int * int * int
val pos2sfull : string * int * int -> string
val pos2sfull_current : int -> int -> string
