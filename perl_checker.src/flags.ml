open Common
open Types

let verbose = ref false
let quiet = ref false
let generate_pot = ref false
let expand_tabs = ref (Some 8)
let no_cache = ref false

let check_unused_global_vars = ref false
let check_white_space = ref true
let check_suggest_simpler = ref true
let check_void = ref true
let check_context = ref true
let check_strange = ref true
let check_traps = ref true
let check_complex_expressions = ref true
let normalized_expressions = ref true
let check_help_perl_checker = ref true
let suggest_functional = ref true
let check_prototypes = ref true
let check_names = ref true
let check_import_export = ref true
let allow_MDK_Common = ref true

let is_warning_type_set = function
  | Warn_white_space -> !check_white_space
  | Warn_suggest_simpler -> !check_suggest_simpler
  | Warn_unused_global_vars -> !check_unused_global_vars
  | Warn_void -> !check_void
  | Warn_context -> !check_context
  | Warn_strange -> !check_strange
  | Warn_traps -> !check_traps
  | Warn_complex_expressions -> !check_complex_expressions
  | Warn_normalized_expressions -> !normalized_expressions
  | Warn_suggest_functional -> !suggest_functional
  | Warn_prototypes -> !check_prototypes
  | Warn_names -> !check_names
  | Warn_import_export -> !check_import_export
  | Warn_MDK_Common -> !allow_MDK_Common
  | Warn_help_perl_checker -> !check_help_perl_checker

let are_warning_types_set l = not !quiet && List.for_all is_warning_type_set l
