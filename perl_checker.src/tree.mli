open Types

type special_export = Re_export_all | Fake_export_all

type exports = {
  export_ok : (context * string) list;
  export_auto : (context * string) list;
  export_tags : (string * (context * string) list) list;
  special_export : special_export option;
} 


type uses = (string * ((context * string) list option * pos)) list

type prototype = {
    proto_nb_min : int ;
    proto_nb_max : int option ;
  }

type per_package = {
    file_name : string ;
    package_name : string ; has_package_name : bool ;
    vars_declared : (context * string, pos * bool ref * prototype option) Hashtbl.t;
    imported : ((context * string) * (string * bool ref * prototype option)) list option ref;
    exports : exports ;
    uses : uses ;
    required_packages : (string * pos) list ;
    body : fromparser list;
    isa : (string * pos) list option ;
    lines_starts : int list ;
    build_time : int ;
    from_cache : bool ;
    from_basedir : bool ;
  }

val ignore_package : string -> unit
val use_lib : string list ref
val uses_external_package : string -> bool
val findfile : string list -> string -> string

val get_global_info_from_package : bool -> int -> fromparser list -> per_package list

val has_proto : string option -> fromparser -> ((context * string) list * pos * fromparser list) option
val get_vars_declaration : (context * string * string, pos * prototype option) Hashtbl.t -> per_package -> unit

val die_with_pos : string * int * int -> string -> 'a
val warn_with_pos : string * int * int -> string -> unit

val fold_tree : ('a -> fromparser -> 'a option) -> 'a -> fromparser -> 'a
val from_qw : fromparser -> (context * string) list
