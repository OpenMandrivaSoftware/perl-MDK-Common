
package Glib::Boxed;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Glib::IO;
our @ISA = qw();
sub add_watch { my ($_class, $_fd, $_condition, $_callback, $_o_data, $_o_priority) = @_ }

package Glib::Idle;
our @ISA = qw();
sub add { my ($_class, $_callback, $_o_data, $_o_priority) = @_ }

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
sub get { my ($_object, @_more_paras) = @_ }
sub get_data { my ($_object, $_key) = @_ }
sub get_pointer { my ($_object) = @_ }
sub get_property { my ($_object, @_more_paras) = @_ }
sub list_properties { my ($_object) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub new_from_pointer { my ($_class, $_pointer, $_o_noinc) = @_ }
sub set { my ($_object, @_more_paras) = @_ }
sub set_data { my ($_object, $_key, $_data) = @_ }
sub set_property { my ($_object, @_more_paras) = @_ }
sub signal_connect { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_connect_after { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_connect_swapped { my ($_instance, $_detailed_signal, $_callback, $_o_data) = @_ }
sub signal_emit { my ($_instance, $_name, @_more_paras) = @_ }
sub signal_handler_block { my ($_object, $_handler_id) = @_ }
sub signal_handler_disconnect { my ($_object, $_handler_id) = @_ }
sub signal_handler_unblock { my ($_object, $_handler_id) = @_ }
sub signal_stop_emission_by_name { my ($_instance, $_detailed_signal) = @_ }

package Glib::ParamSpec;
our @ISA = qw();
sub IV { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub UV { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub boolean { my ($_class, $_name, $_nick, $_blurb, $_default_value, $_flags) = @_ }
sub boxed { my ($_class, $_name, $_nick, $_blurb, $_package, $_flags) = @_ }
sub char { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
sub double { my ($_class, $_name, $_nick, $_blurb, $_minimum, $_maximum, $_default_value, $_flags) = @_ }
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

package Glib::Source;
our @ISA = qw();
sub remove { my ($_class, $_tag) = @_ }

package Glib::Timeout;
our @ISA = qw();
sub add { my ($_class, $_interval, $_callback, $_o_data, $_o_priority) = @_ }

package Glib::Type;
our @ISA = qw();
sub register { my ($_class, $_parent_package, $_new_package, @_more_paras) = @_ }
