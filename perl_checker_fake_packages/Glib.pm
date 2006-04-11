
package Glib;
our @ISA = qw();
sub CHECK_VERSION { my ($_class, $_required_major, $_required_minor, $_required_micro) = @_ }
sub GET_VERSION_INFO { my ($_class) = @_ }
sub MAJOR_VERSION() {}
sub MICRO_VERSION() {}
sub MINOR_VERSION() {}
sub critical { my ($_class, $_domain, $_message) = @_ }
sub error { my ($_class, $_domain, $_message) = @_ }
sub filename_display_basename { my ($_filename) = @_ }
sub filename_display_name { my ($_filename) = @_ }
sub filename_from_unicode { my ($_class_or_filename, $_o_filename) = @_ }
sub filename_from_uri { my (@_more_paras) = @_ }
sub filename_to_unicode { my ($_class_or_filename, $_o_filename) = @_ }
sub filename_to_uri { my (@_more_paras) = @_ }
sub get_application_name() {}
sub get_home_dir() {}
sub get_language_names() {}
sub get_real_name() {}
sub get_system_config_dirs() {}
sub get_system_data_dirs() {}
sub get_tmp_dir() {}
sub get_user_cache_dir() {}
sub get_user_config_dir() {}
sub get_user_data_dir() {}
sub get_user_name() {}
sub install_exception_handler { my ($_class, $_func, $_o_data) = @_ }
sub log { my ($_class, $_log_domain, $_log_level, $_message) = @_ }
sub main_depth() {}
sub major_version() {}
sub message { my ($_class, $_domain, $_message) = @_ }
sub micro_version() {}
sub minor_version() {}
sub remove_exception_handler { my ($_class, $_tag) = @_ }
sub set_application_name { my ($_application_name) = @_ }
sub warning { my ($_class, $_domain, $_message) = @_ }

package Glib::Boxed;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub copy { my ($_sv) = @_ }

package Glib::Error;
our @ISA = qw();
sub code { my ($_error) = @_ }
sub domain { my ($_error) = @_ }
sub location { my ($_error) = @_ }
sub matches { my ($_error, $_domain, $_code) = @_ }
sub message { my ($_error) = @_ }
sub new { my ($_class, $_code, $_message) = @_ }
sub register { my ($_package, $_enum_package) = @_ }
sub throw { my ($_class, $_code, $_message) = @_ }
sub value { my ($_error) = @_ }

package Glib::Flags;
our @ISA = qw();
sub all { my ($_a, $_b, $_swap) = @_ }
sub as_arrayref { my ($_a, $_b, $_swap) = @_ }
sub bool { my ($_a, $_b, $_swap) = @_ }
sub Glib::Flags::eq { my ($_a, $_b, $_swap) = @_ }
sub Glib::Flags::ge { my ($_a, $_b, $_swap) = @_ }
sub intersect { my ($_a, $_b, $_swap) = @_ }
sub Glib::Flags::sub { my ($_a, $_b, $_swap) = @_ }
sub union { my ($_a, $_b, $_swap) = @_ }
sub Glib::Flags::xor { my ($_a, $_b, $_swap) = @_ }

package Glib::IO;
our @ISA = qw();
sub add_watch { my ($_class, $_fd, $_condition, $_callback, $_o_data, $_o_priority) = @_ }

package Glib::Idle;
our @ISA = qw();
sub add { my ($_class, $_callback, $_o_data, $_o_priority) = @_ }

package Glib::KeyFile;
our @ISA = qw();
sub DESTROY { my ($_key_file) = @_ }
sub get_boolean { my ($_key_file, $_group_name, $_key) = @_ }
sub get_boolean_list { my ($_key_file, $_group_name, $_key) = @_ }
sub get_comment { my ($_key_file, $_o_group_name, $_o_key) = @_ }
sub get_groups { my ($_key_file) = @_ }
sub get_integer { my ($_key_file, $_group_name, $_key) = @_ }
sub get_integer_list { my ($_key_file, $_group_name, $_key) = @_ }
sub get_keys { my ($_key_file, $_group_name) = @_ }
sub get_locale_string { my ($_key_file, $_group_name, $_key, $_o_locale) = @_ }
sub get_locale_string_list { my ($_key_file, $_group_name, $_key, $_locale) = @_ }
sub get_start_group { my ($_key_file) = @_ }
sub get_string { my ($_key_file, $_group_name, $_key) = @_ }
sub get_string_list { my ($_key_file, $_group_name, $_key) = @_ }
sub get_value { my ($_key_file, $_group_name, $_key) = @_ }
sub has_group { my ($_key_file, $_group_name) = @_ }
sub has_key { my ($_key_file, $_group_name, $_key) = @_ }
sub load_from_data { my ($_key_file, $_buf, $_flags) = @_ }
sub load_from_data_dirs { my ($_key_file, $_file, $_flags) = @_ }
sub load_from_file { my ($_key_file, $_file, $_flags) = @_ }
sub new { my ($_class) = @_ }
sub remove_comment { my ($_key_file, $_o_group_name, $_o_key) = @_ }
sub remove_group { my ($_key_file, $_group_name) = @_ }
sub remove_key { my ($_key_file, $_group_name, $_key) = @_ }
sub set_boolean { my ($_key_file, $_group_name, $_key, $_value) = @_ }
sub set_boolean_list { my ($_key_file, $_group_name, $_key, @_more_paras) = @_ }
sub set_comment { my ($_key_file, $_group_name, $_key, $_comment) = @_ }
sub set_integer { my ($_key_file, $_group_name, $_key, $_value) = @_ }
sub set_integer_list { my ($_key_file, $_group_name, $_key, @_more_paras) = @_ }
sub set_list_separator { my ($_key_file, $_separator) = @_ }
sub set_locale_string { my ($_key_file, $_group_name, $_key, $_locale, $_string) = @_ }
sub set_locale_string_list { my ($_key_file, $_group_name, $_key, $_locale, @_more_paras) = @_ }
sub set_string { my ($_key_file, $_group_name, $_key, $_value) = @_ }
sub set_string_list { my ($_key_file, $_group_name, $_key, @_more_paras) = @_ }
sub set_value { my ($_key_file, $_group_name, $_key, $_value) = @_ }
sub to_data { my ($_key_file) = @_ }

package Glib::Log;
our @ISA = qw();
sub remove_handler { my ($_class, $_log_domain, $_handler_id) = @_ }
sub set_always_fatal { my ($_class, $_fatal_mask) = @_ }
sub set_fatal_mask { my ($_class, $_log_domain, $_fatal_mask) = @_ }
sub set_handler { my ($_class, $_log_domain, $_log_levels, $_log_func, $_o_user_data) = @_ }

package Glib::MainContext;
our @ISA = qw();
sub DESTROY { my ($_maincontext) = @_ }
sub default { my ($_class) = @_ }
sub iteration { my ($_context, $_may_block) = @_ }
sub new { my ($_class) = @_ }
sub pending { my ($_context) = @_ }

package Glib::MainLoop;
our @ISA = qw();
sub DESTROY { my ($_mainloop) = @_ }
sub get_context { my ($_loop) = @_ }
sub is_running { my ($_loop) = @_ }
sub new { my ($_class, $_o_context, $_o_is_running) = @_ }
sub quit { my ($_loop) = @_ }
sub run { my ($_loop) = @_ }

package Glib::Markup;
our @ISA = qw();
sub escape_text { my ($_text) = @_ }

package Glib::Object;
our @ISA = qw();
sub CLONE { my ($_class) = @_ }
sub DESTROY { my ($_sv) = @_ }
sub freeze_notify { my ($_object) = @_ }
sub get { my ($_object, @_more_paras) = @_ }
sub get_data { my ($_object, $_key) = @_ }
sub get_pointer { my ($_object) = @_ }
sub get_property { my ($_object, @_more_paras) = @_ }
sub list_properties { my ($_object_or_class_name) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub new_from_pointer { my ($_class, $_pointer, $_o_noinc) = @_ }
sub notify { my ($_object, $_property_name) = @_ }
sub set { my ($_object, @_more_paras) = @_ }
sub set_data { my ($_object, $_key, $_data) = @_ }
sub set_property { my ($_object, @_more_paras) = @_ }
sub set_threadsafe { my ($_class, $_threadsafe) = @_ }
sub signal_add_emission_hook { my ($_object_or_class_name, $_detailed_signal, $_hook_func, $_o_hook_data) = @_ }
sub signal_chain_from_overridden { my ($_instance, @_more_paras) = @_ }
sub signal_connect { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_connect_after { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_connect_swapped { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_emit { my ($_instance, $_name, @_more_paras) = @_ }
sub signal_handler_block { my ($_object, $_handler_id) = @_ }
sub signal_handler_disconnect { my ($_object, $_handler_id) = @_ }
sub signal_handler_is_connected { my ($_object, $_handler_id) = @_ }
sub signal_handler_unblock { my ($_object, $_handler_id) = @_ }
sub signal_handlers_block_by_func { my ($_instance, $_func, $_o_data) = @_ }
sub signal_handlers_disconnect_by_func { my ($_instance, $_func, $_o_data) = @_ }
sub signal_handlers_unblock_by_func { my ($_instance, $_func, $_o_data) = @_ }
sub signal_query { my ($_object_or_class_name, $_name) = @_ }
sub signal_remove_emission_hook { my ($_object_or_class_name, $_signal_name, $_hook_id) = @_ }
sub signal_stop_emission_by_name { my ($_instance, $_detailed_signal) = @_ }
sub thaw_notify { my ($_object) = @_ }
sub tie_properties { my ($_object, $_o_all) = @_ }

package Glib::Object::_LazyLoader;
our @ISA = qw();
sub _load { my ($_package) = @_ }

package Glib::Param::Boolean;
our @ISA = qw();
sub get_default_value { my ($_pspec_boolean) = @_ }

package Glib::Param::Char;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Double;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_epsilon { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Enum;
our @ISA = qw();
sub get_default_value { my ($_pspec_enum) = @_ }
sub get_enum_class { my ($_pspec_enum) = @_ }

package Glib::Param::Flags;
our @ISA = qw();
sub get_default_value { my ($_pspec_flags) = @_ }
sub get_flags_class { my ($_pspec_flags) = @_ }

package Glib::Param::Float;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_epsilon { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Int;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Int64;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Long;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::String;
our @ISA = qw();
sub get_default_value { my ($_pspec_string) = @_ }

package Glib::Param::UChar;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::UInt;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::UInt64;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::ULong;
our @ISA = qw();
sub get_default_value { my ($_pspec) = @_ }
sub get_maximum { my ($_pspec) = @_ }
sub get_minimum { my ($_pspec) = @_ }

package Glib::Param::Unichar;
our @ISA = qw();
sub get_default_value { my ($_pspec_unichar) = @_ }

package Glib::ParamSpec;
our @ISA = qw();
sub DESTROY { my ($_pspec) = @_ }
sub IV { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub UV { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub boolean { my ($_class, $_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }
sub boxed { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub char { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub double { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub enum { my ($_class, $_name, $_nick, $_blurb, $_enum_type, $_default_value, $_flags) = @_ }
sub flags { my ($_class, $_name, $_nick, $_blurb, $_flags_type, $_default_value, $_flags) = @_ }
sub float { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub get_blurb { my ($_pspec) = @_ }
sub get_flags { my ($_pspec) = @_ }
sub get_name { my ($_pspec) = @_ }
sub get_nick { my ($_pspec) = @_ }
sub get_owner_type { my ($_pspec) = @_ }
sub get_value_type { my ($_pspec) = @_ }
sub int { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub int64 { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub long { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub object { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub param_spec { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub scalar { my ($_class, $_name, $_nick, $_blurb, $_flags) = @_ }
sub string { my ($_class, $_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }
sub uchar { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub uint { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub uint64 { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub ulong { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub unichar { my ($_class, $_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }

package Glib::Source;
our @ISA = qw();
sub remove { my ($_class, $_tag) = @_ }

package Glib::Timeout;
our @ISA = qw();
sub add { my ($_class, $_interval, $_callback, $_o_data, $_o_priority) = @_ }

package Glib::Type;
our @ISA = qw();
sub list_ancestors { my ($_class, $_package) = @_ }
sub list_interfaces { my ($_class, $_package) = @_ }
sub list_signals { my ($_class, $_package) = @_ }
sub list_values { my ($_class, $_package) = @_ }
sub package_from_cname { my ($_class, $_cname) = @_ }
sub register { my ($_class, $_parent_class, $_new_class, @_more_paras) = @_ }
sub register_enum { my ($_class, $_name, @_more_paras) = @_ }
sub register_flags { my ($_class, $_name, @_more_paras) = @_ }
sub register_object { my ($_class, $_parent_package, $_new_package, @_more_paras) = @_ }
