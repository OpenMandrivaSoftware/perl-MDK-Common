
package Glib;
our @ISA = qw();
sub critical { my ($_class, $_domain, $_message) = @_ }
sub error { my ($_class, $_domain, $_message) = @_ }
sub filename_from_unicode { my ($_class_or_filename, $_o_filename) = @_ }
sub filename_to_unicode { my ($_class_or_filename, $_o_filename) = @_ }
sub install_exception_handler { my ($_class, $_func, $_o_data) = @_ }
sub log { my ($_class, $_log_domain, $_log_level, $_message) = @_ }
sub message { my ($_class, $_domain, $_message) = @_ }
sub remove_exception_handler { my ($_class, $_tag) = @_ }
sub warning { my ($_class, $_domain, $_message) = @_ }

package Glib::Boxed;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub copy { my ($_sv) = @_ }

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

package Glib::Object;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub do_stuff_by_func { my ($_instance, $_func, $_o_data) = @_ }
sub get { my ($_object, @_more_paras) = @_ }
sub get_data { my ($_object, $_key) = @_ }
sub get_pointer { my ($_object) = @_ }
sub get_property { my ($_object, @_more_paras) = @_ }
sub list_properties { my ($_object_or_class_name) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub new_from_pointer { my ($_class, $_pointer, $_o_noinc) = @_ }
sub set { my ($_object, @_more_paras) = @_ }
sub set_data { my ($_object, $_key, $_data) = @_ }
sub set_property { my ($_object, @_more_paras) = @_ }
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
sub signal_stop_emission_by_name { my ($_instance, $_detailed_signal) = @_ }

package Glib::ParamSpec;
our @ISA = qw();
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
sub get_name { my ($_pspec) = @_ }
sub get_nick { my ($_pspec) = @_ }
sub int { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub int64 { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub long { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub object { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub param_spec { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub string { my ($_class, $_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }
sub typed { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub uchar { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub uint { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub uint64 { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub ulong { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub unichar { my ($_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }

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
