val verbose : bool ref
val quiet : bool ref
val generate_pot : bool ref
val expand_tabs : int option ref
val no_cache : bool ref
val check_unused_global_vars : bool ref
val check_white_space : bool ref
val check_suggest_simpler : bool ref
val check_void : bool ref
val check_context : bool ref
val check_strange : bool ref
val check_traps : bool ref
val check_complex_expressions : bool ref
val normalized_expressions : bool ref
val check_help_perl_checker : bool ref
val suggest_functional : bool ref
val check_prototypes : bool ref
val check_names : bool ref
val check_import_export : bool ref
val allow_MDK_Common : bool ref
val is_warning_type_set : Types.warning -> bool
val are_warning_types_set : Types.warning list -> bool
