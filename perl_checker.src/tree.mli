open Types

type special_export = Re_export_all | Export_all

type exports = {
  export_ok : (context * string) list;
  export_auto : (context * string) list;
  export_tags : (string * (context * string) list) list;
  special_export : special_export option;
} 


type uses = (string * ((context * string) list option * pos)) list

type per_package = {
    file_name : string ;
    package_name : string ; has_package_name : bool ;
    vars_declared : (context * string, pos) Hashtbl.t;
    imported : ((context * string) * string) list option ref;
    exports : exports ;
    uses : uses ;
    body : fromparser list;
  }
type state = {
    per_package : (string * per_package) list;
    files_parsed : string list;
    global_vars_declared : (context * string * string, pos) Hashtbl.t;
    global_vars_used : ((context * string * string) * pos) list ref;
  } 

val ignored_packages : string list ref

val default_state : state
val get_global_info_from_package : fromparser list -> per_package list * (string * pos) list
val get_vars_declaration : state -> per_package -> unit
val check_tree : state -> per_package -> unit

val die_with_pos : string * int * int -> string -> 'a
val warn_with_pos : string * int * int -> string -> unit
val add_package_to_state : state -> per_package -> state