open Types
open Tree

type state = {
    per_package : (string, per_package) Hashtbl.t;
    methods : (string, (pos * bool ref * prototype option) list) Hashtbl.t ;
    global_vars_declared : (context * string * string, pos * prototype option) Hashtbl.t;
    global_vars_used : ((context * string * string) * pos) list ref;
    packages_being_classes : (string, unit) Hashtbl.t ;
  } 

val default_state : unit -> state
val check_tree : state -> per_package -> unit
val add_package_to_state : state -> per_package -> unit
val check_unused_vars : per_package -> unit
val arrange_global_vars_declared : state -> state
val get_methods_available : state -> state

val read_packages_from_cache : state -> string -> unit
val write_packages_cache : state -> string -> unit
