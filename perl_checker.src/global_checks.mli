open Types
open Tree

type state = {
    per_files : (string, per_file) Hashtbl.t ;
    per_packages : (string, per_package) Hashtbl.t ;
    methods : (string, (pos * variable_used ref * prototype option) list) Hashtbl.t ;
    global_vars_used : ((context * string * string) * pos) list ref ;
    packages_being_classes : (string, unit) Hashtbl.t ;
  }

val default_per_files : unit -> (string, per_file) Hashtbl.t
val default_state : (string, per_file) Hashtbl.t -> state
val check_tree : state -> per_package -> unit
val add_file_to_files : (string, per_file) Hashtbl.t -> per_file -> unit
val add_package_to_state : state -> per_package -> unit
val check_unused_vars : per_package -> unit
val arrange_global_vars_declared : (context * string * string, pos * Tree.prototype option) Hashtbl.t -> state -> state
val get_methods_available : state -> state

val read_packages_from_cache : (string, per_file) Hashtbl.t -> string -> unit
val write_packages_cache : (string, per_file) Hashtbl.t -> string -> unit
