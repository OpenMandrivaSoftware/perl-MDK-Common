type config_file = { basedir : int option; } 
val ignored_packages : string list ref
val default : config_file
val config_cache : (string, config_file) Hashtbl.t
val read : string -> config_file
val read_any : string -> int -> unit
