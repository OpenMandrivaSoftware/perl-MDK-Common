package Gtk2;
use Glib;

package Gtk2;
our @ISA = qw();
sub check_version { my ($_class, $_required_major, $_required_minor, $_required_micro) = @_ }
sub disable_setlocale { my ($_class) = @_ }
sub events_pending { my ($_class) = @_ }
sub get_current_event { my ($_class) = @_ }
sub get_current_event_state { my ($_class) = @_ }
sub get_current_event_time { my ($_class) = @_ }
sub get_default_language { my ($_class) = @_ }
sub get_event_widget { my ($_class, $_event) = @_ }
sub get_version_info { my ($_class) = @_ }
sub grab_add { my ($_class, $_widget) = @_ }
sub grab_get_current { my ($_class) = @_ }
sub grab_remove { my ($_class, $_widget) = @_ }
sub init { my ($_class) = @_ }
sub init_add { my ($_class, $_function, $_o_data) = @_ }
sub init_check { my ($_class) = @_ }
sub key_snooper_install { my ($_class, $_snooper, $_o_func_data) = @_ }
sub key_snooper_remove { my ($_class, $_snooper_handler_id) = @_ }
sub main { my ($_class) = @_ }
sub main_iteration { my ($_class) = @_ }
sub main_iteration_do { my ($_class, $_blocking) = @_ }
sub main_level { my ($_class) = @_ }
sub main_quit { my ($_class) = @_ }
sub quit_add { my ($_class, $_main_level, $_function, $_o_data) = @_ }
sub quit_add_destroy { my ($_class, $_main_level, $_object) = @_ }
sub quit_remove { my ($_class, $_quit_handler_id) = @_ }
sub set_locale { my ($_class) = @_ }

package Gtk2::AccelGroup;
our @ISA = qw();
sub connect { my ($_accel_group, $_accel_key, $_accel_mods, $_accel_flags, $_func) = @_ }
sub connect_by_path { my ($_accel_group, $_accel_path, $_func) = @_ }
sub disconnect { my ($_accel_group, $_func) = @_ }
sub disconnect_key { my ($_accel_group, $_accel_key, $_accel_mods) = @_ }
sub lock { my ($_accel_group) = @_ }
sub new { my ($_class) = @_ }
sub unlock { my ($_accel_group) = @_ }

package Gtk2::AccelGroups;
our @ISA = qw();
sub activate { my ($_class, $_object, $_accel_key, $_accel_mods) = @_ }
sub from_object { my ($_class, $_object) = @_ }

package Gtk2::AccelLabel;
our @ISA = qw();
sub get_accel_widget { my ($_accel_label) = @_ }
sub get_accel_width { my ($_accel_label) = @_ }
sub new { my ($_class, $_string) = @_ }
sub refetch { my ($_accel_label) = @_ }
sub set_accel_widget { my ($_accel_label, $_accel_widget) = @_ }

package Gtk2::AccelMap;
our @ISA = qw();
sub add_entry { my ($_class, $_accel_path, $_accel_key, $_accel_mods) = @_ }
sub add_filter { my ($_class, $_filter_pattern) = @_ }
sub change_entry { my ($_class, $_accel_path, $_accel_key, $_accel_mods, $_replace) = @_ }
sub Gtk2::AccelMap::foreach { my ($_class, $_data, $_foreach_func) = @_ }
sub foreach_unfiltered { my ($_class, $_data, $_foreach_func) = @_ }
sub load { my ($_class, $_file_name) = @_ }
sub load_fd { my ($_class, $_fd) = @_ }
sub lookup_entry { my ($_class, $_accel_path) = @_ }
sub save { my ($_class, $_file_name) = @_ }
sub save_fd { my ($_class, $_fd) = @_ }

package Gtk2::Accelerator;
our @ISA = qw();
sub get_default_mod_mask { my ($_class) = @_ }
sub name { my ($_class, $_accelerator_key, $_accelerator_mods) = @_ }
sub parse { my ($_class, $_accelerator) = @_ }
sub set_default_mod_mask { my ($_class, $_default_mod_mask) = @_ }
sub valid { my ($_class, $_keyval, $_modifiers) = @_ }

package Gtk2::Adjustment;
our @ISA = qw();
sub changed { my ($_adjustment) = @_ }
sub clamp_page { my ($_adjustment, $_lower, $_upper) = @_ }
sub get_value { my ($_adjustment) = @_ }
sub lower { my ($_adjustment, $_o_newval) = @_ }
sub new { my ($_class, $_value, $_lower, $_upper, $_step_increment, $_page_increment, $_page_size) = @_ }
sub page_increment { my ($_adjustment, $_o_newval) = @_ }
sub page_size { my ($_adjustment, $_o_newval) = @_ }
sub set_value { my ($_adjustment, $_value) = @_ }
sub step_increment { my ($_adjustment, $_o_newval) = @_ }
sub upper { my ($_adjustment, $_o_newval) = @_ }
sub value { my ($_adjustment, $_o_newval) = @_ }
sub value_changed { my ($_adjustment) = @_ }

package Gtk2::Alignment;
our @ISA = qw();
sub new { my ($_class, $_xalign, $_yalign, $_xscale, $_yscale) = @_ }
sub set { my ($_alignment, $_xalign, $_yalign, $_xscale, $_yscale) = @_ }

package Gtk2::Allocation;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Gtk2::Arrow;
our @ISA = qw();
sub new { my ($_class, $_arrow_type, $_shadow_type) = @_ }
sub set { my ($_arrow, $_arrow_type, $_shadow_type) = @_ }

package Gtk2::AspectFrame;
our @ISA = qw();
sub new { my ($_class, $_label, $_xalign, $_yalign, $_ratio, $_obey_child) = @_ }
sub set_params { my ($_aspect_frame, $_xalign, $_yalign, $_ratio, $_obey_child) = @_ }

package Gtk2::Bin;
our @ISA = qw();
sub child { my ($_bin) = @_ }
sub get_child { my ($_bin) = @_ }

package Gtk2::Box;
our @ISA = qw();
sub get_homogeneous { my ($_box) = @_ }
sub get_spacing { my ($_box) = @_ }
sub pack_end { my ($_box, $_child, $_expand, $_fill, $_padding) = @_ }
sub pack_end_defaults { my ($_box, $_widget) = @_ }
sub pack_start { my ($_box, $_child, $_expand, $_fill, $_padding) = @_ }
sub pack_start_defaults { my ($_box, $_widget) = @_ }
sub query_child_packing { my ($_box, $_child) = @_ }
sub reorder_child { my ($_box, $_child, $_position) = @_ }
sub set_child_packing { my ($_box, $_child, $_expand, $_fill, $_padding, $_pack_type) = @_ }
sub set_homogeneous { my ($_box, $_homogeneous) = @_ }
sub set_spacing { my ($_box, $_spacing) = @_ }

package Gtk2::Button;
our @ISA = qw();
sub clicked { my ($_button) = @_ }
sub enter { my ($_button) = @_ }
sub get_label { my ($_button) = @_ }
sub get_relief { my ($_button) = @_ }
sub get_use_stock { my ($_button) = @_ }
sub get_use_underline { my ($_button) = @_ }
sub leave { my ($_button) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_from_stock { my ($_class, $_stock_id) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }
sub pressed { my ($_button) = @_ }
sub released { my ($_button) = @_ }
sub set_label { my ($_button, $_label) = @_ }
sub set_relief { my ($_button, $_newstyle) = @_ }
sub set_use_stock { my ($_button, $_use_stock) = @_ }
sub set_use_underline { my ($_button, $_use_underline) = @_ }

package Gtk2::ButtonBox;
our @ISA = qw();
sub get_layout { my ($_widget) = @_ }
sub set_child_secondary { my ($_widget, $_child, $_is_secondary) = @_ }
sub set_layout { my ($_widget, $_layout_style) = @_ }

package Gtk2::Calendar;
our @ISA = qw();
sub clear_marks { my ($_calendar) = @_ }
sub display_options { my ($_calendar, $_flags) = @_ }
sub freeze { my ($_calendar) = @_ }
sub get_date { my ($_calendar) = @_ }
sub mark_day { my ($_calendar, $_day) = @_ }
sub marked_date { my ($_cal) = @_ }
sub month { my ($_cal) = @_ }
sub new { my ($_class) = @_ }
sub num_marked_dates { my ($_cal) = @_ }
sub select_day { my ($_calendar, $_day) = @_ }
sub select_month { my ($_calendar, $_month, $_year) = @_ }
sub selected_day { my ($_cal) = @_ }
sub thaw { my ($_calendar) = @_ }
sub unmark_day { my ($_calendar, $_day) = @_ }
sub year { my ($_cal) = @_ }

package Gtk2::CellEditable;
our @ISA = qw();
sub editing_done { my ($_cell_editable) = @_ }
sub remove_widget { my ($_cell_editable) = @_ }
sub start_editing { my ($_cell_editable, $_event) = @_ }

package Gtk2::CellRenderer;
our @ISA = qw();
sub _install_overrides { my ($_package) = @_ }
sub activate { my ($_cell, $_event, $_widget, $_path, $_background_area, $_cell_area, $_flags) = @_ }
sub get_fixed_size { my ($_cell) = @_ }
sub get_size { my ($_cell, $_widget) = @_ }
sub parent_activate { my ($_cell, @_more_paras) = @_ }
sub parent_get_size { my ($_cell, @_more_paras) = @_ }
sub parent_render { my ($_cell, @_more_paras) = @_ }
sub parent_start_editing { my ($_cell, @_more_paras) = @_ }
sub render { my ($_cell, $_drawable, $_widget, $_background_area, $_cell_area, $_expose_area, $_flags) = @_ }
sub set_fixed_size { my ($_cell, $_width, $_height) = @_ }
sub start_editing { my ($_cell, $_event, $_widget, $_path, $_background_area, $_cell_area, $_flags) = @_ }

package Gtk2::CellRendererPixbuf;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::CellRendererText;
our @ISA = qw();
sub new { my ($_class) = @_ }
sub set_fixed_height_from_font { my ($_renderer, $_number_of_rows) = @_ }

package Gtk2::CellRendererToggle;
our @ISA = qw();
sub get_active { my ($_toggle) = @_ }
sub get_radio { my ($_toggle) = @_ }
sub new { my ($_class) = @_ }
sub set_active { my ($_toggle, $_setting) = @_ }
sub set_radio { my ($_toggle, $_radio) = @_ }

package Gtk2::CheckButton;
our @ISA = qw();
sub new { my ($_class, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }

package Gtk2::CheckMenuItem;
our @ISA = qw();
sub get_active { my ($_check_menu_item) = @_ }
sub get_inconsistent { my ($_check_menu_item) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }
sub set_active { my ($_check_menu_item, $_is_active) = @_ }
sub set_inconsistent { my ($_check_menu_item, $_setting) = @_ }
sub set_show_toggle { my ($_menu_item, $_always) = @_ }
sub toggled { my ($_check_menu_item) = @_ }

package Gtk2::Clipboard;
our @ISA = qw();
sub clear { my ($_clipboard) = @_ }
sub get { my ($_class, $_selection) = @_ }
sub get_display { my ($_clipboard) = @_ }
sub get_for_display { my ($_class, $_display, $_selection) = @_ }
sub get_owner { my ($_clipboard) = @_ }
sub request_contents { my ($_clipboard, $_target, $_callback, $_user_data) = @_ }
sub request_text { my ($_clipboard, $_callback, $_user_data) = @_ }
sub set_text { my ($_clipboard, $_text, $_text) = @_ }
sub set_with_data { my ($_clipboard, $_get_func, $_clear_func, $_user_data, @_more_paras) = @_ }
sub set_with_owner { my ($_clipboard, $_get_func, $_clear_func, $_owner, @_more_paras) = @_ }
sub wait_for_contents { my ($_clipboard, $_target) = @_ }
sub wait_for_text { my ($_clipboard) = @_ }
sub wait_is_text_available { my ($_clipboard) = @_ }

package Gtk2::ColorSelection;
our @ISA = qw();
sub get_current_alpha { my ($_colorsel) = @_ }
sub get_current_color { my ($_colorsel) = @_ }
sub get_has_opacity_control { my ($_colorsel) = @_ }
sub get_has_palette { my ($_colorsel) = @_ }
sub get_previous_alpha { my ($_colorsel) = @_ }
sub get_previous_color { my ($_colorsel) = @_ }
sub is_adjusting { my ($_colorsel) = @_ }
sub new { my ($_class) = @_ }
sub palette_from_string { my ($_class, $_string) = @_ }
sub palette_to_string { my ($_class, @_more_paras) = @_ }
sub set_current_alpha { my ($_colorsel, $_alpha) = @_ }
sub set_current_color { my ($_colorsel, $_color) = @_ }
sub set_has_opacity_control { my ($_colorsel, $_has_opacity) = @_ }
sub set_has_palette { my ($_colorsel, $_has_palette) = @_ }
sub set_previous_alpha { my ($_colorsel, $_alpha) = @_ }
sub set_previous_color { my ($_colorsel, $_color) = @_ }

package Gtk2::ColorSelectionDialog;
our @ISA = qw();
sub cancel_button { my ($_dialog) = @_ }
sub colorsel { my ($_dialog) = @_ }
sub help_button { my ($_dialog) = @_ }
sub new { my ($_class, $_title) = @_ }
sub ok_button { my ($_dialog) = @_ }

package Gtk2::Combo;
our @ISA = qw();
sub disable_activate { my ($_combo) = @_ }
sub entry { my ($_combo) = @_ }
sub list { my ($_combo) = @_ }
sub new { my ($_class) = @_ }
sub set_case_sensitive { my ($_combo, $_val) = @_ }
sub set_item_string { my ($_combo, $_item, $_item_value) = @_ }
sub set_popdown_strings { my ($_combo, @_more_paras) = @_ }
sub set_use_arrows { my ($_combo, $_val) = @_ }
sub set_use_arrows_always { my ($_combo, $_val) = @_ }
sub set_value_in_list { my ($_combo, $_val, $_ok_if_empty) = @_ }

package Gtk2::Container;
our @ISA = qw();
sub add { my ($_container, $_widget) = @_ }
sub add_with_properties { my ($_container, $_widget, @_more_paras) = @_ }
sub check_resize { my ($_container) = @_ }
sub child_get { my ($_container, $_child, @_more_paras) = @_ }
sub child_get_property { my ($_container, $_child, @_more_paras) = @_ }
sub child_set { my ($_container, $_child, @_more_paras) = @_ }
sub child_set_property { my ($_container, $_child, @_more_paras) = @_ }
sub child_type { my ($_container) = @_ }
sub Gtk2::Container::foreach { my ($_container, $_callback, $_o_callback_data) = @_ }
sub get_border_width { my ($_container) = @_ }
sub get_children { my ($_container) = @_ }
sub get_focus_chain { my ($_container) = @_ }
sub get_focus_hadjustment { my ($_container) = @_ }
sub get_focus_vadjustment { my ($_container) = @_ }
sub get_resize_mode { my ($_container) = @_ }
sub propagate_expose { my ($_container, $_child, $_event) = @_ }
sub remove { my ($_container, $_widget) = @_ }
sub resize_children { my ($_container) = @_ }
sub set_border_width { my ($_container, $_border_width) = @_ }
sub set_focus_chain { my ($_container, @_more_paras) = @_ }
sub set_focus_child { my ($_container, $_child) = @_ }
sub set_focus_hadjustment { my ($_container, $_adjustment) = @_ }
sub set_focus_vadjustment { my ($_container, $_adjustment) = @_ }
sub set_reallocate_redraws { my ($_container, $_needs_redraws) = @_ }
sub set_resize_mode { my ($_container, $_resize_mode) = @_ }
sub unset_focus_chain { my ($_container) = @_ }

package Gtk2::Curve;
our @ISA = qw();
sub get_vector { my ($_curve, $_o_veclen) = @_ }
sub new { my ($_class) = @_ }
sub reset { my ($_curve) = @_ }
sub set_curve_type { my ($_curve, $_type) = @_ }
sub set_gamma { my ($_curve, $_gamma) = @_ }
sub set_range { my ($_curve, $_min_x, $_max_x, $_min_y, $_max_y) = @_ }
sub set_vector { my ($_curve, @_more_paras) = @_ }

package Gtk2::Dialog;
our @ISA = qw();
sub action_area { my ($_dialog) = @_ }
sub add_action_widget { my ($_dialog, $_child, $_response_id) = @_ }
sub add_button { my ($_dialog, $_button_text, $_response_id) = @_ }
sub add_buttons { my ($_dialog, @_more_paras) = @_ }
sub get_has_separator { my ($_dialog) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub new_with_buttons { my ($_class, @_more_paras) = @_ }
sub response { my ($_dialog, $_response_id) = @_ }
sub run { my ($_dialog) = @_ }
sub set_default_response { my ($_dialog, $_response_id) = @_ }
sub set_has_separator { my ($_dialog, $_setting) = @_ }
sub set_response_sensitive { my ($_dialog, $_response_id, $_setting) = @_ }
sub vbox { my ($_dialog) = @_ }

package Gtk2::Drag;
our @ISA = qw();
sub begin { my ($_class, $_widget, $_targets, $_actions, $_button, $_event) = @_ }

package Gtk2::DrawingArea;
our @ISA = qw();
sub new { my ($_class) = @_ }
sub size { my ($_darea, $_width, $_height) = @_ }

package Gtk2::Editable;
our @ISA = qw();
sub copy_clipboard { my ($_editable) = @_ }
sub cut_clipboard { my ($_editable) = @_ }
sub delete_selection { my ($_editable) = @_ }
sub delete_text { my ($_editable, $_start_pos, $_end_pos) = @_ }
sub get_chars { my ($_editable, $_start_pos, $_end_pos) = @_ }
sub get_editable { my ($_editable) = @_ }
sub get_position { my ($_editable) = @_ }
sub get_selection_bounds { my ($_editable) = @_ }
sub insert_text { my ($_editable, $_new_text, $_new_text_length, $_position) = @_ }
sub paste_clipboard { my ($_editable) = @_ }
sub select_region { my ($_editable, $_start, $_end) = @_ }
sub set_editable { my ($_editable, $_is_editable) = @_ }
sub set_position { my ($_editable, $_position) = @_ }

package Gtk2::Entry;
our @ISA = qw();
sub append_text { my ($_entry, $_text) = @_ }
sub get_activates_default { my ($_entry) = @_ }
sub get_has_frame { my ($_entry) = @_ }
sub get_invisible_char { my ($_entry) = @_ }
sub get_layout { my ($_entry) = @_ }
sub get_layout_offsets { my ($_entry) = @_ }
sub get_max_length { my ($_entry) = @_ }
sub get_text { my ($_entry) = @_ }
sub get_visibility { my ($_entry) = @_ }
sub get_width_chars { my ($_entry) = @_ }
sub new { my ($_class) = @_ }
sub new_with_max_length { my ($_class, $_max) = @_ }
sub prepend_text { my ($_entry, $_text) = @_ }
sub select_region { my ($_entry, $_start, $_end) = @_ }
sub set_activates_default { my ($_entry, $_setting) = @_ }
sub set_editable { my ($_entry, $_editable) = @_ }
sub set_has_frame { my ($_entry, $_setting) = @_ }
sub set_invisible_char { my ($_entry, $_ch) = @_ }
sub set_max_length { my ($_entry, $_max) = @_ }
sub set_position { my ($_entry, $_position) = @_ }
sub set_text { my ($_entry, $_text) = @_ }
sub set_visibility { my ($_entry, $_visible) = @_ }
sub set_width_chars { my ($_entry, $_n_chars) = @_ }

package Gtk2::EventBox;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::FileSelection;
our @ISA = qw();
sub action_area { my ($_fs) = @_ }
sub button_area { my ($_fs) = @_ }
sub cancel_button { my ($_fs) = @_ }
sub complete { my ($_filesel, $_pattern) = @_ }
sub dir_list { my ($_fs) = @_ }
sub file_list { my ($_fs) = @_ }
sub fileop_c_dir { my ($_fs) = @_ }
sub fileop_del_file { my ($_fs) = @_ }
sub fileop_dialog { my ($_fs) = @_ }
sub fileop_entry { my ($_fs) = @_ }
sub fileop_file { my ($_fs) = @_ }
sub fileop_ren_file { my ($_fs) = @_ }
sub get_filename { my ($_filesel) = @_ }
sub get_select_multiple { my ($_filesel) = @_ }
sub get_selections { my ($_filesel) = @_ }
sub help_button { my ($_fs) = @_ }
sub hide_fileop_buttons { my ($_filesel) = @_ }
sub history_menu { my ($_fs) = @_ }
sub history_pulldown { my ($_fs) = @_ }
sub main_vbox { my ($_fs) = @_ }
sub new { my ($_class, $_title) = @_ }
sub ok_button { my ($_fs) = @_ }
sub selection_entry { my ($_fs) = @_ }
sub selection_text { my ($_fs) = @_ }
sub set_filename { my ($_filesel, $_filename) = @_ }
sub set_select_multiple { my ($_filesel, $_select_multiple) = @_ }
sub show_fileop_buttons { my ($_filesel) = @_ }

package Gtk2::Fixed;
our @ISA = qw();
sub get_has_window { my ($_fixed) = @_ }
sub move { my ($_fixed, $_widget, $_x, $_y) = @_ }
sub new { my ($_class) = @_ }
sub put { my ($_fixed, $_widget, $_x, $_y) = @_ }
sub set_has_window { my ($_fixed, $_has_window) = @_ }

package Gtk2::FontSelection;
our @ISA = qw();
sub get_font { my ($_fontsel) = @_ }
sub get_font_name { my ($_fontsel) = @_ }
sub get_preview_text { my ($_fontsel) = @_ }
sub new { my ($_class) = @_ }
sub set_font_name { my ($_fontsel, $_fontname) = @_ }
sub set_preview_text { my ($_fontsel, $_text) = @_ }

package Gtk2::FontSelectionDialog;
our @ISA = qw();
sub apply_button { my ($_fsd) = @_ }
sub cancel_button { my ($_fsd) = @_ }
sub get_font { my ($_fsd) = @_ }
sub get_font_name { my ($_fsd) = @_ }
sub get_preview_text { my ($_fsd) = @_ }
sub new { my ($_class, $_title) = @_ }
sub ok_button { my ($_fsd) = @_ }
sub set_font_name { my ($_fsd, $_fontname) = @_ }
sub set_preview_text { my ($_fsd, $_text) = @_ }

package Gtk2::Frame;
our @ISA = qw();
sub get_label { my ($_frame) = @_ }
sub get_label_align { my ($_frame) = @_ }
sub get_label_widget { my ($_frame) = @_ }
sub get_shadow_type { my ($_frame) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub set_label { my ($_frame, $_o_label) = @_ }
sub set_label_align { my ($_frame, $_xalign, $_yalign) = @_ }
sub set_label_widget { my ($_frame, $_label_widget) = @_ }
sub set_shadow_type { my ($_frame, $_type) = @_ }

package Gtk2::GammaCurve;
our @ISA = qw();
sub curve { my ($_gamma) = @_ }
sub new { my ($_class) = @_ }

package Gtk2::Gdk;
our @ISA = qw();
sub SELECTION_CLIPBOARD { my ($_class) = @_ }
sub SELECTION_PRIMARY { my ($_class) = @_ }
sub SELECTION_SECONDARY { my ($_class) = @_ }
sub SELECTION_TYPE_ATOM { my ($_class) = @_ }
sub SELECTION_TYPE_BITMAP { my ($_class) = @_ }
sub SELECTION_TYPE_COLORMAP { my ($_class) = @_ }
sub SELECTION_TYPE_DRAWABLE { my ($_class) = @_ }
sub SELECTION_TYPE_INTEGER { my ($_class) = @_ }
sub SELECTION_TYPE_PIXMAP { my ($_class) = @_ }
sub SELECTION_TYPE_STRING { my ($_class) = @_ }
sub SELECTION_TYPE_WINDOW { my ($_class) = @_ }
sub TARGET_BITMAP { my ($_class) = @_ }
sub TARGET_COLORMAP { my ($_class) = @_ }
sub TARGET_DRAWABLE { my ($_class) = @_ }
sub TARGET_PIXMAP { my ($_class) = @_ }
sub TARGET_STRING { my ($_class) = @_ }
sub beep { my ($_class) = @_ }
sub error_trap_pop { my ($_class) = @_ }
sub error_trap_push { my ($_class) = @_ }
sub flush { my ($_class) = @_ }
sub get_default_root_window { my ($_class) = @_ }
sub get_display { my ($_class) = @_ }
sub get_display_arg_name { my ($_class) = @_ }
sub get_program_class { my ($_class) = @_ }
sub get_show_events { my ($_class) = @_ }
sub keyboard_grab { my ($_class, $_window, $_owner_events, $_time_) = @_ }
sub keyboard_ungrab { my ($_class, $_time_) = @_ }
sub notify_startup_complete { my ($_class) = @_ }
sub pointer_grab { my ($_class, $_window, $_owner_events, $_event_mask, $_confine_to, $_cursor, $_time_) = @_ }
sub pointer_is_grabbed { my ($_class) = @_ }
sub pointer_ungrab { my ($_class, $_time_) = @_ }
sub screen_height { my ($_class) = @_ }
sub screen_height_mm { my ($_class) = @_ }
sub screen_width { my ($_class) = @_ }
sub screen_width_mm { my ($_class) = @_ }
sub set_locale { my ($_class) = @_ }
sub set_program_class { my ($_class, $_program_class) = @_ }
sub set_show_events { my ($_class, $_show_events) = @_ }
sub set_sm_client_id { my ($_class, $_sm_client_id) = @_ }
sub setting_get { my ($_class, $_name) = @_ }

package Gtk2::Gdk::Atom;
our @ISA = qw();
sub Gtk2::Gdk::Atom::eq { my ($_left, $_right, $_o_swap) = @_ }
sub intern { my ($_class, $_atom_name, $_o_only_if_exists) = @_ }
sub name { my ($_atom) = @_ }
sub new { my ($_class, $_atom_name, $_o_only_if_exists) = @_ }

package Gtk2::Gdk::Bitmap;
our @ISA = qw();
sub create_from_data { my ($_class, $_drawable, $_data, $_width, $_height) = @_ }

package Gtk2::Gdk::Color;
our @ISA = qw();
sub blue { my ($_color) = @_ }
sub equal { my ($_colora, $_colorb) = @_ }
sub green { my ($_color) = @_ }
sub hash { my ($_colora) = @_ }
sub new { my ($_class, $_red, $_green, $_blue) = @_ }
sub parse { my ($_class, $_spec) = @_ }
sub pixel { my ($_color) = @_ }
sub red { my ($_color) = @_ }

package Gtk2::Gdk::Colormap;
our @ISA = qw();
sub alloc_color { my ($_colormap, $_color, $_writeable, $_best_match) = @_ }
sub alloc_colors { my ($_colormap, $_writeable, $_best_match, @_more_paras) = @_ }
sub free_colors { my ($_colormap, @_more_paras) = @_ }
sub get_screen { my ($_cmap) = @_ }
sub get_system { my ($_class) = @_ }
sub get_visual { my ($_colormap) = @_ }
sub new { my ($_class, $_visual, $_allocate) = @_ }
sub query_color { my ($_colormap, $_pixel) = @_ }
sub rgb_find_color { my ($_colormap, $_color) = @_ }

package Gtk2::Gdk::Cursor;
our @ISA = qw();
sub get_display { my ($_cursor) = @_ }
sub new { my ($_class, $_cursor_type) = @_ }
sub new_for_display { my ($_class, $_display, $_cursor_type) = @_ }
sub new_from_pixmap { my ($_class, $_source, $_mask, $_fg, $_bg, $_x, $_y) = @_ }

package Gtk2::Gdk::Display;
our @ISA = qw();
sub beep { my ($_display) = @_ }
sub close { my ($_display) = @_ }
sub get_core_pointer { my ($_display) = @_ }
sub get_default { my ($_class) = @_ }
sub get_default_screen { my ($_display) = @_ }
sub get_event { my ($_display) = @_ }
sub get_n_screens { my ($_display) = @_ }
sub get_name { my ($_display) = @_ }
sub get_pointer { my ($_display) = @_ }
sub get_screen { my ($_display, $_screen_num) = @_ }
sub get_window_at_pointer { my ($_display) = @_ }
sub grab { my ($_display) = @_ }
sub keyboard_ungrab { my ($_display, $_time_) = @_ }
sub list_devices { my ($_display) = @_ }
sub open { my ($_class, $_display_name) = @_ }
sub peek_event { my ($_display) = @_ }
sub pointer_is_grabbed { my ($_display) = @_ }
sub pointer_ungrab { my ($_display, $_time_) = @_ }
sub put_event { my ($_display, $_event) = @_ }
sub set_double_click_time { my ($_display, $_msec) = @_ }
sub sync { my ($_display) = @_ }
sub ungrab { my ($_display) = @_ }

package Gtk2::Gdk::DisplayManager;
our @ISA = qw();
sub get { my ($_class) = @_ }
sub get_default_display { my ($_display_manager) = @_ }
sub list_displays { my ($_display_manager) = @_ }
sub set_default_display { my ($_display_manager, $_display) = @_ }

package Gtk2::Gdk::DragContext;
our @ISA = qw();
sub abort { my ($_context, $_time_) = @_ }
sub action { my ($_dc) = @_ }
sub actions { my ($_dc) = @_ }
sub begin { my ($_class, $_window, @_more_paras) = @_ }
sub dest_window { my ($_dc) = @_ }
sub drop { my ($_context, $_time_) = @_ }
sub find_window { my ($_context, $_drag_window, $_x_root, $_y_root) = @_ }
sub find_window_for_screen { my ($_context, $_drag_window, $_screen, $_x_root, $_y_root) = @_ }
sub finish { my ($_context, $_success, $_del, $_time_) = @_ }
sub gdk_drop_finish { my ($_context, $_success, $_o_time_) = @_ }
sub gdk_drop_reply { my ($_context, $_ok, $_o_time_) = @_ }
sub get_protocol { my ($_class, $_xid) = @_ }
sub get_protocol_for_display { my ($_class, $_display, $_xid) = @_ }
sub get_selection { my ($_context) = @_ }
sub get_source_widget { my ($_context) = @_ }
sub is_source { my ($_dc) = @_ }
sub motion { my ($_context, $_dest_window, $_protocol, $_x_root, $_y_root, $_suggested_action, $_possible_actions, $_time_) = @_ }
sub new { my ($_class) = @_ }
sub protocol { my ($_dc) = @_ }
sub set_icon_default { my ($_context) = @_ }
sub set_icon_pixbuf { my ($_context, $_pixbuf, $_hot_x, $_hot_y) = @_ }
sub set_icon_pixmap { my ($_context, $_colormap, $_pixmap, $_mask, $_hot_x, $_hot_y) = @_ }
sub set_icon_stock { my ($_context, $_stock_id, $_hot_x, $_hot_y) = @_ }
sub set_icon_widget { my ($_context, $_widget, $_hot_x, $_hot_y) = @_ }
sub source_window { my ($_dc) = @_ }
sub start_time { my ($_dc) = @_ }
sub status { my ($_context, $_action, $_o_time_) = @_ }
sub suggested_action { my ($_dc) = @_ }
sub targets { my ($_dc) = @_ }

package Gtk2::Gdk::Drawable;
our @ISA = qw();
sub XID { my ($_drawable) = @_ }
sub XWINDOW { my ($_drawable) = @_ }
sub draw_arc { my ($_drawable, $_gc, $_filled, $_x, $_y, $_width, $_height, $_angle1, $_angle2) = @_ }
sub draw_drawable { my ($_drawable, $_gc, $_src, $_xsrc, $_ysrc, $_xdest, $_ydest, $_width, $_height) = @_ }
sub draw_gray_image { my ($_drawable, $_gc, $_x, $_y, $_width, $_height, $_dith, $_buf, $_rowstride) = @_ }
sub draw_image { my ($_drawable, $_gc, $_image, $_xsrc, $_ysrc, $_xdest, $_ydest, $_width, $_height) = @_ }
sub draw_layout { my ($_drawable, $_gc, $_x, $_y, $_layout) = @_ }
sub draw_layout_with_colors { my ($_drawable, $_gc, $_x, $_y, $_layout, $_foreground, $_background) = @_ }
sub draw_line { my ($_drawable, $_gc, $_x1_, $_y1_, $_x2_, $_y2_) = @_ }
sub draw_lines { my ($_drawable, $_gc, $_x1, $_y1, @_more_paras) = @_ }
sub draw_pixbuf { my ($_drawable, $_gc, $_pixbuf, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height, $_dither, $_x_dither, $_y_dither) = @_ }
sub draw_points { my ($_drawable, $_gc, $_x1, $_y1, @_more_paras) = @_ }
sub draw_polygon { my ($_drawable, $_gc, $_filled, $_x1, $_y1, @_more_paras) = @_ }
sub draw_rectangle { my ($_drawable, $_gc, $_filled, $_x, $_y, $_width, $_height) = @_ }
sub draw_rgb_32_image { my ($_drawable, $_gc, $_x, $_y, $_width, $_height, $_dith, $_buf, $_rowstride) = @_ }
sub draw_rgb_32_image_dithalign { my ($_drawable, $_gc, $_x, $_y, $_width, $_height, $_dith, $_rgb_buf, $_rowstride, $_xdith, $_ydith) = @_ }
sub draw_rgb_image { my ($_drawable, $_gc, $_x, $_y, $_width, $_height, $_dith, $_buf, $_rowstride) = @_ }
sub draw_rgb_image_dithalign { my ($_drawable, $_gc, $_x, $_y, $_width, $_height, $_dith, $_rgb_buf, $_rowstride, $_xdith, $_ydith) = @_ }
sub draw_segments { my ($_drawable, $_gc, $_x1, $_y1, $_x2, $_y2, @_more_paras) = @_ }
sub get_colormap { my ($_drawable) = @_ }
sub get_depth { my ($_drawable) = @_ }
sub get_display { my ($_drawable) = @_ }
sub get_image { my ($_drawable, $_x, $_y, $_width, $_height) = @_ }
sub get_screen { my ($_drawable) = @_ }
sub get_size { my ($_drawable) = @_ }
sub get_visual { my ($_drawable) = @_ }
sub get_xid { my ($_drawable) = @_ }
sub set_colormap { my ($_drawable, $_colormap) = @_ }

package Gtk2::Gdk::Event;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub axis { my ($_event, $_axis_use) = @_ }
sub coords { my ($_event) = @_ }
sub copy { my ($_event) = @_ }
sub gdk_events_pending { my ($_class) = @_ }
sub get { my ($_class) = @_ }
sub get_coords { my ($_event) = @_ }
sub get_graphics_expose { my ($_window) = @_ }
sub get_root_coords { my ($_event) = @_ }
sub get_screen { my ($_event) = @_ }
sub get_state { my ($_event) = @_ }
sub get_time { my ($_event) = @_ }
sub new { my ($_class, $_type) = @_ }
sub peek { my ($_class) = @_ }
sub put { my ($_class, $_event) = @_ }
sub root_coords { my ($_event) = @_ }
sub send_event { my ($_event) = @_ }
sub set_screen { my ($_event, $_screen) = @_ }
sub state { my ($_event) = @_ }
sub time { my ($_event) = @_ }
sub type { my ($_event) = @_ }
sub window { my ($_event) = @_ }
sub Gtk2::Gdk::Event::x { my ($_event) = @_ }
sub x_root { my ($_event) = @_ }
sub Gtk2::Gdk::Event::y { my ($_event) = @_ }
sub y_root { my ($_event) = @_ }

package Gtk2::Gdk::Event::Button;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub button { my ($_eventbutton) = @_ }
sub device { my ($_eventbutton) = @_ }

package Gtk2::Gdk::Event::Client;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Gtk2::Gdk::Event::Configure;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub height { my ($_eventconfigure) = @_ }
sub width { my ($_eventconfigure) = @_ }

package Gtk2::Gdk::Event::Crossing;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub detail { my ($_eventcrossing) = @_ }
sub focus { my ($_eventcrossing) = @_ }
sub mode { my ($_eventcrossing) = @_ }

package Gtk2::Gdk::Event::DND;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub context { my ($_eventdnd) = @_ }

package Gtk2::Gdk::Event::Expose;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub area { my ($_eventexpose) = @_ }
sub count { my ($_eventexpose) = @_ }
sub region { my ($_eventexpose) = @_ }

package Gtk2::Gdk::Event::Focus;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub in { my ($_eventfocus) = @_ }

package Gtk2::Gdk::Event::Key;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub keyval { my ($_eventkey) = @_ }
sub Gtk2::Gdk::Event::Key::length { my ($_eventkey) = @_ }
sub string { my ($_eventkey) = @_ }

package Gtk2::Gdk::Event::Motion;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub device { my ($_eventmotion) = @_ }
sub is_hint { my ($_eventmotion) = @_ }

package Gtk2::Gdk::Event::NoExpose;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Gtk2::Gdk::Event::Property;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Gtk2::Gdk::Event::Proximity;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub device { my ($_eventproximity) = @_ }

package Gtk2::Gdk::Event::Scroll;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub device { my ($_eventscroll) = @_ }
sub direction { my ($_eventscroll) = @_ }

package Gtk2::Gdk::Event::Selection;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }

package Gtk2::Gdk::Event::Setting;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub action { my ($_eventsetting) = @_ }
sub name { my ($_eventsetting) = @_ }

package Gtk2::Gdk::Event::Visibility;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub state { my ($_eventvisibility) = @_ }

package Gtk2::Gdk::Event::WindowState;
our @ISA = qw();
sub DESTROY { my ($_sv) = @_ }
sub changed_mask { my ($_eventwindowstate) = @_ }
sub new_window_state { my ($_eventwindowstate) = @_ }

package Gtk2::Gdk::GC;
our @ISA = qw();
sub copy { my ($_dst_gc, $_src_gc) = @_ }
sub get_screen { my ($_gc) = @_ }
sub get_values { my ($_gc) = @_ }
sub new { my ($_class, $_drawable, $_o_values) = @_ }
sub new_with_values { my ($_class, $_drawable, $_o_values) = @_ }
sub offset { my ($_gc, $_x_offset, $_y_offset) = @_ }
sub rgb_gc_set_background { my ($_gc, $_rgb) = @_ }
sub rgb_gc_set_foreground { my ($_gc, $_rgb) = @_ }
sub set_background { my ($_gc, $_color) = @_ }
sub set_clip_mask { my ($_gc, $_mask) = @_ }
sub set_clip_origin { my ($_gc, $_x, $_y) = @_ }
sub set_clip_rectangle { my ($_gc, $_rectangle) = @_ }
sub set_clip_region { my ($_gc, $_region) = @_ }
sub set_colormap { my ($_gc, $_colormap) = @_ }
sub set_dashes { my ($_gc, $_dash_offset, @_more_paras) = @_ }
sub set_exposures { my ($_gc, $_exposures) = @_ }
sub set_fill { my ($_gc, $_fill) = @_ }
sub set_font { my ($_gc, $_font) = @_ }
sub set_foreground { my ($_gc, $_color) = @_ }
sub set_function { my ($_gc, $_function) = @_ }
sub set_line_attributes { my ($_gc, $_line_width, $_line_style, $_cap_style, $_join_style) = @_ }
sub set_rgb_background { my ($_gc, $_rgb) = @_ }
sub set_rgb_bg_color { my ($_gc, $_color) = @_ }
sub set_rgb_fg_color { my ($_gc, $_color) = @_ }
sub set_rgb_foreground { my ($_gc, $_rgb) = @_ }
sub set_stipple { my ($_gc, $_stipple) = @_ }
sub set_subwindow { my ($_gc, $_mode) = @_ }
sub set_tile { my ($_gc, $_tile) = @_ }
sub set_ts_origin { my ($_gc, $_x, $_y) = @_ }

package Gtk2::Gdk::Geometry;
our @ISA = qw();
sub base_height { my ($_object, $_o_newvalue) = @_ }
sub base_width { my ($_object, $_o_newvalue) = @_ }
sub constrain_size { my ($_geometry_ref, @_more_paras) = @_ }
sub gravity { my ($_object, $_o_newvalue) = @_ }
sub height_inc { my ($_object, $_o_newvalue) = @_ }
sub max_aspect { my ($_object, $_o_newvalue) = @_ }
sub max_height { my ($_object, $_o_newvalue) = @_ }
sub max_width { my ($_object, $_o_newvalue) = @_ }
sub min_aspect { my ($_object, $_o_newvalue) = @_ }
sub min_height { my ($_object, $_o_newvalue) = @_ }
sub min_width { my ($_object, $_o_newvalue) = @_ }
sub new { my ($_class) = @_ }
sub width_inc { my ($_object, $_o_newvalue) = @_ }
sub win_gravity { my ($_object, $_o_newvalue) = @_ }

package Gtk2::Gdk::Keymap;
our @ISA = qw();
sub gdk_keymap_get_default { my ($_class) = @_ }
sub gdk_keymap_get_direction { my ($_keymap) = @_ }
sub gdk_keymap_get_for_display { my ($_class, $_display) = @_ }
sub gdk_keymap_translate_keyboard_state { my ($_keymap, $_hardware_keycode, $_state, $_group) = @_ }

package Gtk2::Gdk::Keyval;
our @ISA = qw();
sub gdk_keyval_convert_case { my ($_class, $_symbol) = @_ }
sub gdk_keyval_from_name { my ($_class, $_keyval_name) = @_ }
sub gdk_keyval_is_lower { my ($_class, $_keyval) = @_ }
sub gdk_keyval_is_upper { my ($_class, $_keyval) = @_ }
sub gdk_keyval_name { my ($_class, $_keyval) = @_ }
sub gdk_keyval_to_lower { my ($_class, $_keyval) = @_ }
sub gdk_keyval_to_unicode { my ($_class, $_keyval) = @_ }
sub gdk_keyval_to_upper { my ($_class, $_keyval) = @_ }
sub gdk_unicode_to_keyval { my ($_class, $_wc) = @_ }

package Gtk2::Gdk::Pixbuf;
our @ISA = qw();
sub add_alpha { my ($_pixbuf, $_substitute_color, $_r, $_g, $_b) = @_ }
sub composite { my ($_src, $_dest, $_dest_x, $_dest_y, $_dest_width, $_dest_height, $_offset_x, $_offset_y, $_scale_x, $_scale_y, $_interp_type, $_overall_alpha) = @_ }
sub composite_color { my ($_src, $_dest, $_dest_x, $_dest_y, $_dest_width, $_dest_height, $_offset_x, $_offset_y, $_scale_x, $_scale_y, $_interp_type, $_overall_alpha, $_check_x, $_check_y, $_check_size, $_color1, $_color2) = @_ }
sub composite_color_simple { my ($_src, $_dest_width, $_dest_height, $_interp_type, $_overall_alpha, $_check_size, $_color1, $_color2) = @_ }
sub copy { my ($_pixbuf) = @_ }
sub copy_area { my ($_src_pixbuf, $_src_x, $_src_y, $_width, $_height, $_dest_pixbuf, $_dest_x, $_dest_y) = @_ }
sub fill { my ($_pixbuf, $_pixel) = @_ }
sub get_bits_per_sample { my ($_pixbuf) = @_ }
sub get_colorspace { my ($_pixbuf) = @_ }
sub get_formats { my ($_o_class) = @_ }
sub get_from_drawable { my ($_dest_or_class, $_src, $_cmap, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height) = @_ }
sub get_from_image { my ($_dest_or_class, $_src, $_cmap, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height) = @_ }
sub get_has_alpha { my ($_pixbuf) = @_ }
sub get_height { my ($_pixbuf) = @_ }
sub get_n_channels { my ($_pixbuf) = @_ }
sub get_pixels { my ($_pixbuf) = @_ }
sub get_rowstride { my ($_pixbuf) = @_ }
sub get_width { my ($_pixbuf) = @_ }
sub new { my ($_class, $_colorspace, $_has_alpha, $_bits_per_sample, $_width, $_height) = @_ }
sub new_from_data { my ($_class, $_data, $_colorspace, $_has_alpha, $_bits_per_sample, $_width, $_height, $_rowstride) = @_ }
sub new_from_file { my ($_class, $_filename) = @_ }
sub new_from_inline { my ($_class, $_data, $_o_copy_pixels) = @_ }
sub new_from_xpm_data { my ($_class, @_more_paras) = @_ }
sub new_subpixbuf { my ($_src_pixbuf, $_src_x, $_src_y, $_width, $_height) = @_ }
sub render_pixmap_and_mask { my ($_pixbuf, $_alpha_threshold) = @_ }
sub render_pixmap_and_mask_for_colormap { my ($_pixbuf, $_colormap, $_alpha_threshold) = @_ }
sub render_threshold_alpha { my ($_pixbuf, $_bitmap, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height, $_alpha_threshold) = @_ }
sub render_to_drawable { my ($_pixbuf, $_drawable, $_gc, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height, $_dither, $_x_dither, $_y_dither) = @_ }
sub render_to_drawable_alpha { my ($_pixbuf, $_drawable, $_src_x, $_src_y, $_dest_x, $_dest_y, $_width, $_height, $_alpha_mode, $_alpha_threshold, $_dither, $_x_dither, $_y_dither) = @_ }
sub saturate_and_pixelate { my ($_src, $_dest, $_saturation, $_pixelate) = @_ }
sub save { my ($_pixbuf, $_filename, $_type, @_more_paras) = @_ }
sub scale { my ($_src, $_dest, $_dest_x, $_dest_y, $_dest_width, $_dest_height, $_offset_x, $_offset_y, $_scale_x, $_scale_y, $_interp_type) = @_ }
sub scale_simple { my ($_src, $_dest_width, $_dest_height, $_interp_type) = @_ }

package Gtk2::Gdk::PixbufAnimation;
our @ISA = qw();
sub get_height { my ($_animation) = @_ }
sub get_iter { my ($_animation, $_o_start_time_seconds, $_o_start_time_microseconds) = @_ }
sub get_static_image { my ($_animation) = @_ }
sub get_width { my ($_animation) = @_ }
sub is_static_image { my ($_animation) = @_ }
sub new_from_file { my ($_class, $_filename) = @_ }

package Gtk2::Gdk::PixbufAnimationIter;
our @ISA = qw();
sub advance { my ($_iter, $_o_current_time_seconds, $_o_current_time_microseconds) = @_ }
sub get_delay_time { my ($_iter) = @_ }
sub get_pixbuf { my ($_iter) = @_ }
sub on_currently_loading_frame { my ($_iter) = @_ }

package Gtk2::Gdk::PixbufLoader;
our @ISA = qw();
sub close { my ($_loader) = @_ }
sub get_animation { my ($_loader) = @_ }
sub get_pixbuf { my ($_loader) = @_ }
sub new { my ($_class) = @_ }
sub new_with_type { my ($_image_type) = @_ }
sub set_size { my ($_loader, $_width, $_height) = @_ }
sub write { my ($_loader, $_buf) = @_ }

package Gtk2::Gdk::Pixmap;
our @ISA = qw();
sub colormap_create_from_xpm { my ($_class, $_drawable, $_colormap, $_transparent_color, $_filename) = @_ }
sub colormap_create_from_xpm_d { my ($_class, $_drawable, $_colormap, $_transparent_color, $_data, @_more_paras) = @_ }
sub create_from_data { my ($_class, $_drawable, $_data, $_width, $_height, $_depth, $_fg, $_bg) = @_ }
sub create_from_xpm { my ($_class, $_drawable, $_transparent_color, $_filename) = @_ }
sub create_from_xpm_d { my ($_class, $_drawable, $_transparent_color, $_data, @_more_paras) = @_ }
sub new { my ($_class, $_drawable, $_width, $_height, $_depth) = @_ }

package Gtk2::Gdk::Rectangle;
our @ISA = qw();
sub height { my ($_rectangle, $_o_newvalue) = @_ }
sub intersect { my ($_src1, $_src2) = @_ }
sub new { my ($_class, $_x, $_y, $_width, $_height) = @_ }
sub union { my ($_src1, $_src2) = @_ }
sub values { my ($_rectangle) = @_ }
sub width { my ($_rectangle, $_o_newvalue) = @_ }
sub Gtk2::Gdk::Rectangle::x { my ($_rectangle, $_o_newvalue) = @_ }
sub Gtk2::Gdk::Rectangle::y { my ($_rectangle, $_o_newvalue) = @_ }

package Gtk2::Gdk::Region;
our @ISA = qw();
sub empty { my ($_region) = @_ }
sub equal { my ($_region1, $_region2) = @_ }
sub get_clipbox { my ($_region) = @_ }
sub get_rectangles { my ($_region) = @_ }
sub intersect { my ($_source1, $_source2) = @_ }
sub new { my ($_class) = @_ }
sub offset { my ($_region, $_dx, $_dy) = @_ }
sub point_in { my ($_region, $_x, $_y) = @_ }
sub polygon { my ($_class, $_points_ref, $_fill_rule) = @_ }
sub rect_in { my ($_region, $_rect) = @_ }
sub rectangle { my ($_class, $_rectangle) = @_ }
sub shrink { my ($_region, $_dx, $_dy) = @_ }
sub spans_intersect_foreach { my ($_region, $_spans_ref, $_sorted, $_func, $_o_data) = @_ }
sub subtract { my ($_source1, $_source2) = @_ }
sub union { my ($_source1, $_source2) = @_ }
sub union_with_rect { my ($_region, $_rect) = @_ }
sub Gtk2::Gdk::Region::xor { my ($_source1, $_source2) = @_ }

package Gtk2::Gdk::Rgb;
our @ISA = qw();
sub ditherable { my ($_class) = @_ }
sub set_install { my ($_class, $_install) = @_ }
sub set_min_colors { my ($_class, $_min_colors) = @_ }
sub set_verbose { my ($_class, $_verbose) = @_ }

package Gtk2::Gdk::Screen;
our @ISA = qw();
sub broadcast_client_message { my ($_screen, $_event) = @_ }
sub get_default { my ($_class) = @_ }
sub get_default_colormap { my ($_screen) = @_ }
sub get_display { my ($_screen) = @_ }
sub get_height { my ($_screen) = @_ }
sub get_height_mm { my ($_screen) = @_ }
sub get_monitor_at_point { my ($_screen, $_x, $_y) = @_ }
sub get_monitor_at_window { my ($_screen, $_window) = @_ }
sub get_monitor_geometry { my ($_screen, $_monitor_num) = @_ }
sub get_n_monitors { my ($_screen) = @_ }
sub get_number { my ($_screen) = @_ }
sub get_rgb_colormap { my ($_screen) = @_ }
sub get_rgb_visual { my ($_screen) = @_ }
sub get_root_window { my ($_screen) = @_ }
sub get_setting { my ($_screen, $_name) = @_ }
sub get_system_colormap { my ($_screen) = @_ }
sub get_system_visual { my ($_screen) = @_ }
sub get_toplevel_windows { my ($_screen) = @_ }
sub get_width { my ($_screen) = @_ }
sub get_width_mm { my ($_screen) = @_ }
sub list_visuals { my ($_screen) = @_ }
sub make_display_name { my ($_screen) = @_ }
sub set_default_colormap { my ($_screen, $_colormap) = @_ }

package Gtk2::Gdk::Selection;
our @ISA = qw();
sub convert { my ($_class, $_requestor, $_selection, $_target, $_time_) = @_ }
sub owner_get { my ($_class, $_selection) = @_ }
sub owner_get_for_display { my ($_class, $_display, $_selection) = @_ }
sub owner_set { my ($_class, $_owner, $_selection, $_time_, $_send_event) = @_ }
sub owner_set_for_display { my ($_class, $_display, $_owner, $_selection, $_time_, $_send_event) = @_ }
sub property_get { my ($_class, $_requestor) = @_ }
sub send_notify { my ($_class, $_requestor, $_selection, $_target, $_property, $_time_) = @_ }
sub send_notify_for_display { my ($_class, $_display, $_requestor, $_selection, $_target, $_property, $_time_) = @_ }

package Gtk2::Gdk::Threads;
our @ISA = qw();
sub enter { my ($_class) = @_ }
sub init { my ($_class) = @_ }
sub leave { my ($_class) = @_ }

package Gtk2::Gdk::Window;
our @ISA = qw();
sub at_pointer { my ($_class) = @_ }
sub begin_move_drag { my ($_window, $_button, $_root_x, $_root_y, $_timestamp) = @_ }
sub begin_paint_rect { my ($_window, $_rectangle) = @_ }
sub begin_paint_region { my ($_window, $_region) = @_ }
sub begin_resize_drag { my ($_window, $_edge, $_button, $_root_x, $_root_y, $_timestamp) = @_ }
sub clear { my ($_window) = @_ }
sub clear_area { my ($_window, $_x, $_y, $_width, $_height) = @_ }
sub clear_area_e { my ($_window, $_x, $_y, $_width, $_height) = @_ }
sub deiconify { my ($_window) = @_ }
sub destroy { my ($_window) = @_ }
sub end_paint { my ($_window) = @_ }
sub focus { my ($_window, $_timestamp) = @_ }
sub foreign_new { my ($_class, $_anid) = @_ }
sub foreign_new_for_display { my ($_class, $_display, $_anid) = @_ }
sub freeze_updates { my ($_window) = @_ }
sub fullscreen { my ($_window) = @_ }
sub gdk_set_sm_client_id { my ($_sm_client_id) = @_ }
sub get_children { my ($_window) = @_ }
sub get_decorations { my ($_window) = @_ }
sub get_events { my ($_window) = @_ }
sub get_frame_extents { my ($_window) = @_ }
sub get_geometry { my ($_window) = @_ }
sub get_internal_paint_info { my ($_window) = @_ }
sub get_origin { my ($_window) = @_ }
sub get_parent { my ($_window) = @_ }
sub get_pointer { my ($_window) = @_ }
sub get_position { my ($_window) = @_ }
sub get_root_origin { my ($_window) = @_ }
sub get_state { my ($_window) = @_ }
sub get_toplevel { my ($_window) = @_ }
sub get_toplevels { my ($_class) = @_ }
sub get_update_area { my ($_window) = @_ }
sub get_user_data { my ($_window) = @_ }
sub get_window_type { my ($_window) = @_ }
sub hide { my ($_window) = @_ }
sub iconify { my ($_window) = @_ }
sub invalidate_maybe_recurse { my ($_window, $_region, $_func, $_o_data) = @_ }
sub invalidate_rect { my ($_window, $_rectangle, $_invalidate_children) = @_ }
sub invalidate_region { my ($_window, $_region, $_invalidate_children) = @_ }
sub is_viewable { my ($_window) = @_ }
sub is_visible { my ($_window) = @_ }
sub lookup { my ($_class, $_anid) = @_ }
sub lookup_for_display { my ($_class, $_display, $_anid) = @_ }
sub lower { my ($_window) = @_ }
sub maximize { my ($_window) = @_ }
sub merge_child_shapes { my ($_window) = @_ }
sub move { my ($_window, $_x, $_y) = @_ }
sub move_resize { my ($_window, $_x, $_y, $_width, $_height) = @_ }
sub new { my ($_class, $_parent, $_attributes_ref) = @_ }
sub peek_children { my ($_window) = @_ }
sub process_all_updates { my ($_class_or_instance) = @_ }
sub process_updates { my ($_window, $_update_children) = @_ }
sub property_change { my ($_window, $_property, $_type, $_format, $_mode, $_data, $_nelements) = @_ }
sub property_delete { my ($_window, $_property) = @_ }
sub property_get { my ($_window, $_property, $_type, $_offset, $_length, $_pdelete) = @_ }
sub raise { my ($_window) = @_ }
sub register_dnd { my ($_window) = @_ }
sub reparent { my ($_window, $_new_parent, $_x, $_y) = @_ }
sub resize { my ($_window, $_width, $_height) = @_ }
sub scroll { my ($_window, $_dx, $_dy) = @_ }
sub set_back_pixmap { my ($_window, $_pixmap, $_o_parent_relative) = @_ }
sub set_background { my ($_window, $_color) = @_ }
sub set_child_shapes { my ($_window) = @_ }
sub set_cursor { my ($_window, $_cursor) = @_ }
sub set_debug_updates { my ($_class_or_instance, $_enable) = @_ }
sub set_decorations { my ($_window, $_decorations) = @_ }
sub set_events { my ($_window, $_event_mask) = @_ }
sub set_functions { my ($_window, $_functions) = @_ }
sub set_geometry_hints { my ($_window, $_geometry_ref, $_o_geom_mask_sv) = @_ }
sub set_group { my ($_window, $_leader) = @_ }
sub set_icon { my ($_window, $_icon_window, $_pixmap, $_mask) = @_ }
sub set_icon_list { my ($_window, @_more_paras) = @_ }
sub set_icon_name { my ($_window, $_name) = @_ }
sub set_modal_hint { my ($_window, $_modal) = @_ }
sub set_override_redirect { my ($_window, $_override_redirect) = @_ }
sub set_role { my ($_window, $_role) = @_ }
sub set_skip_pager_hint { my ($_window, $_skips_pager) = @_ }
sub set_skip_taskbar_hint { my ($_window, $_skips_taskbar) = @_ }
sub set_static_gravities { my ($_window, $_use_static) = @_ }
sub set_title { my ($_window, $_title) = @_ }
sub set_transient_for { my ($_window, $_parent) = @_ }
sub set_type_hint { my ($_window, $_hint) = @_ }
sub set_user_data { my ($_window, $_user_data) = @_ }
sub shape_combine_mask { my ($_window, $_mask, $_x, $_y) = @_ }
sub shape_combine_region { my ($_window, $_shape_region, $_offset_x, $_offset_y) = @_ }
sub show { my ($_window) = @_ }
sub show_unraised { my ($_window) = @_ }
sub stick { my ($_window) = @_ }
sub thaw_updates { my ($_window) = @_ }
sub unfullscreen { my ($_window) = @_ }
sub unmaximize { my ($_window) = @_ }
sub unstick { my ($_window) = @_ }
sub withdraw { my ($_window) = @_ }

package Gtk2::HBox;
our @ISA = qw();
sub new { my ($_class, $_o_homogeneous, $_o_spacing) = @_ }

package Gtk2::HButtonBox;
our @ISA = qw();
sub get_layout_default { my ($_class) = @_ }
sub get_spacing_default { my ($_class) = @_ }
sub new { my ($_class) = @_ }
sub set_layout_default { my ($_class, $_layout) = @_ }
sub set_spacing_default { my ($_class, $_spacing) = @_ }

package Gtk2::HPaned;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::HRuler;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::HScale;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }
sub new_with_range { my ($_class, $_min, $_max, $_step) = @_ }

package Gtk2::HScrollBar;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }

package Gtk2::HScrollbar;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }

package Gtk2::HSeparator;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::HandleBox;
our @ISA = qw();
sub get_child_detached { my ($_handle_box) = @_ }
sub get_handle_position { my ($_handle_box) = @_ }
sub get_shadow_type { my ($_handle_box) = @_ }
sub get_snap_edge { my ($_handle_box) = @_ }
sub new { my ($_class) = @_ }
sub set_handle_position { my ($_handle_box, $_position) = @_ }
sub set_shadow_type { my ($_handle_box, $_type) = @_ }
sub set_snap_edge { my ($_handle_box, $_edge) = @_ }

package Gtk2::IconFactory;
our @ISA = qw();
sub add { my ($_factory, $_stock_id, $_icon_set) = @_ }
sub add_default { my ($_factory) = @_ }
sub lookup { my ($_factory, $_stock_id) = @_ }
sub lookup_default { my ($_class, $_stock_id) = @_ }
sub new { my ($_class) = @_ }
sub remove_default { my ($_factory) = @_ }

package Gtk2::IconSet;
our @ISA = qw();
sub add_source { my ($_icon_set, $_source) = @_ }
sub get_sizes { my ($_icon_set) = @_ }
sub new { my ($_class) = @_ }
sub new_from_pixbuf { my ($_class, $_pixbuf) = @_ }
sub render_icon { my ($_icon_set, $_style, $_direction, $_state, $_size, $_widget, $_o_detail) = @_ }

package Gtk2::IconSize;
our @ISA = qw();
sub from_name { my ($_class, $_name) = @_ }
sub lookup { my ($_class, $_size) = @_ }
sub lookup_for_settings { my ($_class, $_settings, $_size, $_width, $_height) = @_ }
sub register { my ($_class, $_name, $_width, $_height) = @_ }
sub register_alias { my ($_class, $_alias, $_target) = @_ }

package Gtk2::IconSource;
our @ISA = qw();
sub get_direction { my ($_source) = @_ }
sub get_direction_wildcarded { my ($_source) = @_ }
sub get_filename { my ($_source) = @_ }
sub get_pixbuf { my ($_source) = @_ }
sub get_size { my ($_source) = @_ }
sub get_size_wildcarded { my ($_source) = @_ }
sub get_state { my ($_source) = @_ }
sub get_state_wildcarded { my ($_source) = @_ }
sub new { my ($_class) = @_ }
sub set_direction { my ($_source, $_direction) = @_ }
sub set_direction_wildcarded { my ($_source, $_setting) = @_ }
sub set_filename { my ($_source, $_filename) = @_ }
sub set_pixbuf { my ($_source, $_pixbuf) = @_ }
sub set_size { my ($_source, $_size) = @_ }
sub set_size_wildcarded { my ($_source, $_setting) = @_ }
sub set_state { my ($_source, $_state) = @_ }
sub set_state_wildcarded { my ($_source, $_setting) = @_ }

package Gtk2::Image;
our @ISA = qw();
sub get_animation { my ($_image) = @_ }
sub get_icon_set { my ($_image) = @_ }
sub get_image { my ($_image) = @_ }
sub get_pixbuf { my ($_image) = @_ }
sub get_pixmap { my ($_image) = @_ }
sub get_stock { my ($_image) = @_ }
sub get_storage_type { my ($_image) = @_ }
sub new { my ($_class) = @_ }
sub new_from_animation { my ($_class, $_animation) = @_ }
sub new_from_file { my ($_class, $_filename) = @_ }
sub new_from_icon_set { my ($_class, $_icon_set, $_size) = @_ }
sub new_from_image { my ($_class, $_image, $_mask) = @_ }
sub new_from_pixbuf { my ($_class, $_pixbuf) = @_ }
sub new_from_pixmap { my ($_class, $_pixmap, $_mask) = @_ }
sub new_from_stock { my ($_class, $_stock_id, $_size) = @_ }
sub set_from_animation { my ($_image, $_animation) = @_ }
sub set_from_file { my ($_image, $_filename) = @_ }
sub set_from_icon_set { my ($_image, $_icon_set, $_size) = @_ }
sub set_from_image { my ($_image, $_gdk_image, $_mask) = @_ }
sub set_from_pixbuf { my ($_image, $_pixbuf) = @_ }
sub set_from_pixmap { my ($_image, $_pixmap, $_mask) = @_ }
sub set_from_stock { my ($_image, $_stock_id, $_size) = @_ }

package Gtk2::ImageMenuItem;
our @ISA = qw();
sub get_image { my ($_image_menu_item) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_from_stock { my ($_class, $_stock_id, $_o_accel_group) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }
sub set_image { my ($_image_menu_item, $_image) = @_ }

package Gtk2::InputDialog;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::Invisible;
our @ISA = qw();
sub get_screen { my ($_invisible) = @_ }
sub new { my ($_class) = @_ }
sub new_for_screen { my ($_class, $_screen) = @_ }
sub set_screen { my ($_invisible, $_screen) = @_ }

package Gtk2::Item;
our @ISA = qw();
sub deselect { my ($_item) = @_ }
sub select { my ($_item) = @_ }
sub toggle { my ($_item) = @_ }

package Gtk2::ItemFactory;
our @ISA = qw();
sub create_item { my ($_ifactory, $_entry_ref, $_o_callback_data) = @_ }
sub create_items { my ($_ifactory, $_callback_data, @_more_paras) = @_ }
sub delete_entries { my ($_ifactory, @_more_paras) = @_ }
sub delete_entry { my ($_ifactory, $_entry_ref) = @_ }
sub delete_item { my ($_ifactory, $_path) = @_ }
sub from_widget { my ($_class, $_widget) = @_ }
sub get_item { my ($_ifactory, $_path) = @_ }
sub get_item_by_action { my ($_ifactory, $_action) = @_ }
sub get_widget { my ($_ifactory, $_path) = @_ }
sub get_widget_by_action { my ($_ifactory, $_action) = @_ }
sub new { my ($_class, $_container_type_package, $_path, $_o_accel_group) = @_ }
sub path_from_widget { my ($_class, $_widget) = @_ }
sub popup { my ($_ifactory, $_x, $_y, $_mouse_button, $_time_, $_o_popup_data) = @_ }
sub popup_data { my ($_ifactory) = @_ }
sub popup_data_from_widget { my ($_class, $_widget) = @_ }
sub set_translate_func { my ($_ifactory, $_func, $_o_data) = @_ }

package Gtk2::Label;
our @ISA = qw();
sub get_attributes { my ($_label) = @_ }
sub get_justify { my ($_label) = @_ }
sub get_label { my ($_label) = @_ }
sub get_layout { my ($_label) = @_ }
sub get_layout_offsets { my ($_label) = @_ }
sub get_line_wrap { my ($_label) = @_ }
sub get_mnemonic_keyval { my ($_label) = @_ }
sub get_mnemonic_widget { my ($_label) = @_ }
sub get_selectable { my ($_label) = @_ }
sub get_selection_bounds { my ($_label) = @_ }
sub get_text { my ($_label) = @_ }
sub get_use_markup { my ($_label) = @_ }
sub get_use_underline { my ($_label) = @_ }
sub new { my ($_class, $_o_str) = @_ }
sub new_with_mnemonic { my ($_class, $_str) = @_ }
sub select_region { my ($_label, $_o_start_offset, $_o_end_offset) = @_ }
sub set_attributes { my ($_label, $_attrs) = @_ }
sub set_justify { my ($_label, $_jtype) = @_ }
sub set_label { my ($_label, $_str) = @_ }
sub set_line_wrap { my ($_label, $_wrap) = @_ }
sub set_markup { my ($_label, $_str) = @_ }
sub set_markup_with_mnemonic { my ($_label, $_str) = @_ }
sub set_mnemonic_widget { my ($_label, $_widget) = @_ }
sub set_pattern { my ($_label, $_pattern) = @_ }
sub set_selectable { my ($_label, $_setting) = @_ }
sub set_text { my ($_label, $_str) = @_ }
sub set_text_with_mnemonic { my ($_label, $_str) = @_ }
sub set_use_markup { my ($_label, $_setting) = @_ }
sub set_use_underline { my ($_label, $_setting) = @_ }

package Gtk2::Layout;
our @ISA = qw();
sub freeze { my ($_layout) = @_ }
sub get_hadjustment { my ($_layout) = @_ }
sub get_size { my ($_layout) = @_ }
sub get_vadjustment { my ($_layout) = @_ }
sub move { my ($_layout, $_child_widget, $_x, $_y) = @_ }
sub new { my ($_class, $_o_hadjustment, $_o_vadjustment) = @_ }
sub put { my ($_layout, $_child_widget, $_x, $_y) = @_ }
sub set_hadjustment { my ($_layout, $_adjustment) = @_ }
sub set_size { my ($_layout, $_width, $_height) = @_ }
sub set_vadjustment { my ($_layout, $_adjustment) = @_ }
sub thaw { my ($_layout) = @_ }

package Gtk2::List;
our @ISA = qw();
sub append_items { my ($_list, @_more_paras) = @_ }
sub child_position { my ($_list, $_child) = @_ }
sub clear_items { my ($_list, $_start, $_end) = @_ }
sub end_drag_selection { my ($_list) = @_ }
sub end_selection { my ($_list) = @_ }
sub extend_selection { my ($_list, $_scroll_type, $_position, $_auto_start_selection) = @_ }
sub insert_items { my ($_list, $_position, @_more_paras) = @_ }
sub new { my ($_class) = @_ }
sub prepend_items { my ($_list, @_more_paras) = @_ }
sub remove_items { my ($_list, @_more_paras) = @_ }
sub scroll_horizontal { my ($_list, $_scroll_type, $_position) = @_ }
sub scroll_vertical { my ($_list, $_scroll_type, $_position) = @_ }
sub select_all { my ($_list) = @_ }
sub select_child { my ($_list, $_child) = @_ }
sub select_item { my ($_list, $_item) = @_ }
sub set_selection_mode { my ($_list, $_mode) = @_ }
sub start_selection { my ($_list) = @_ }
sub toggle_add_mode { my ($_list) = @_ }
sub toggle_focus_row { my ($_list) = @_ }
sub toggle_row { my ($_list, $_item) = @_ }
sub undo_selection { my ($_list) = @_ }
sub unselect_all { my ($_list) = @_ }
sub unselect_child { my ($_list, $_child) = @_ }
sub unselect_item { my ($_list, $_item) = @_ }

package Gtk2::ListItem;
our @ISA = qw();
sub deselect { my ($_list_item) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub select { my ($_list_item) = @_ }

package Gtk2::ListStore;
our @ISA = qw();
sub append { my ($_list_store) = @_ }
sub clear { my ($_list_store) = @_ }
sub insert { my ($_list_store, $_position) = @_ }
sub insert_after { my ($_list_store, $_sibling) = @_ }
sub insert_before { my ($_list_store, $_sibling) = @_ }
sub iter_is_valid { my ($_list_store, $_iter) = @_ }
sub move_after { my ($_store, $_iter, $_position) = @_ }
sub move_before { my ($_store, $_iter, $_position) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub prepend { my ($_list_store) = @_ }
sub remove { my ($_list_store, $_iter) = @_ }
sub reorder { my ($_store, @_more_paras) = @_ }
sub set { my ($_list_store, $_iter, $_col1, $_val1, @_more_paras) = @_ }
sub set_column_types { my ($_list_store, @_more_paras) = @_ }
sub set_value { my ($_list_store, $_iter, $_col1, $_val1, @_more_paras) = @_ }
sub swap { my ($_store, $_a, $_b) = @_ }

package Gtk2::Menu;
our @ISA = qw();
sub attach_to_widget { my ($_menu, $_attach_widget, $_detacher) = @_ }
sub detach { my ($_menu) = @_ }
sub get_accel_group { my ($_menu) = @_ }
sub get_active { my ($_menu) = @_ }
sub get_attach_widget { my ($_menu) = @_ }
sub get_tearoff_state { my ($_menu) = @_ }
sub get_title { my ($_menu) = @_ }
sub new { my ($_class) = @_ }
sub popdown { my ($_menu) = @_ }
sub popup { my ($_menu, $_parent_menu_shell, $_parent_menu_item, $_menu_pos_func, $_data, $_button, $_activate_time) = @_ }
sub reorder_child { my ($_menu, $_child, $_position) = @_ }
sub reposition { my ($_menu) = @_ }
sub set_accel_group { my ($_menu, $_accel_group) = @_ }
sub set_accel_path { my ($_menu, $_accel_path) = @_ }
sub set_active { my ($_menu, $_index) = @_ }
sub set_screen { my ($_menu, $_screen) = @_ }
sub set_tearoff_state { my ($_menu, $_torn_off) = @_ }
sub set_title { my ($_menu, $_title) = @_ }

package Gtk2::MenuBar;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::MenuItem;
our @ISA = qw();
sub activate { my ($_menu_item) = @_ }
sub deselect { my ($_menu_item) = @_ }
sub get_right_justified { my ($_menu_item) = @_ }
sub get_submenu { my ($_menu_item) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }
sub remove_submenu { my ($_menu_item) = @_ }
sub select { my ($_menu_item) = @_ }
sub set_accel_path { my ($_menu_item, $_accel_path) = @_ }
sub set_right_justified { my ($_menu_item, $_right_justified) = @_ }
sub set_submenu { my ($_menu_item, $_submenu) = @_ }

package Gtk2::MenuShell;
our @ISA = qw();
sub activate_item { my ($_menu_shell, $_menu_item, $_force_deactivate) = @_ }
sub append { my ($_menu_shell, $_child) = @_ }
sub deactivate { my ($_menu_shell) = @_ }
sub deselect { my ($_menu_shell) = @_ }
sub insert { my ($_menu_shell, $_child, $_position) = @_ }
sub prepend { my ($_menu_shell, $_child) = @_ }
sub select_first { my ($_menu_shell, $_search_sensitive) = @_ }
sub select_item { my ($_menu_shell, $_menu_item) = @_ }

package Gtk2::MessageDialog;
our @ISA = qw();
sub new { my ($_class, $_parent, $_flags, $_type, $_buttons, $_format, @_more_paras) = @_ }

package Gtk2::Misc;
our @ISA = qw();
sub get_alignment { my ($_misc) = @_ }
sub get_padding { my ($_misc) = @_ }
sub set_alignment { my ($_misc, $_xalign, $_yalign) = @_ }
sub set_padding { my ($_misc, $_xpad, $_ypad) = @_ }

package Gtk2::Notebook;
our @ISA = qw();
sub append_page { my ($_notebook, $_child, $_o_tab_label) = @_ }
sub append_page_menu { my ($_notebook, $_child, $_tab_label, $_menu_label) = @_ }
sub get_current_page { my ($_notebook) = @_ }
sub get_menu_label { my ($_notebook, $_child) = @_ }
sub get_menu_label_text { my ($_notebook, $_child) = @_ }
sub get_n_pages { my ($_notebook) = @_ }
sub get_nth_page { my ($_notebook, $_page_num) = @_ }
sub get_scrollable { my ($_notebook) = @_ }
sub get_show_border { my ($_notebook) = @_ }
sub get_show_tabs { my ($_notebook) = @_ }
sub get_tab_label { my ($_notebook, $_child) = @_ }
sub get_tab_label_text { my ($_notebook, $_child) = @_ }
sub get_tab_pos { my ($_notebook) = @_ }
sub insert_page { my ($_notebook, $_child, $_tab_label, $_position) = @_ }
sub insert_page_menu { my ($_notebook, $_child, $_tab_label, $_menu_label, $_position) = @_ }
sub new { my ($_class) = @_ }
sub next_page { my ($_notebook) = @_ }
sub page_num { my ($_notebook, $_child) = @_ }
sub popup_disable { my ($_notebook) = @_ }
sub popup_enable { my ($_notebook) = @_ }
sub prepend_page { my ($_notebook, $_child, $_o_tab_label) = @_ }
sub prepend_page_menu { my ($_notebook, $_child, $_tab_label, $_menu_label) = @_ }
sub prev_page { my ($_notebook) = @_ }
sub query_tab_label_packing { my ($_notebook, $_child) = @_ }
sub remove_page { my ($_notebook, $_page_num) = @_ }
sub reorder_child { my ($_notebook, $_child, $_position) = @_ }
sub set_current_page { my ($_notebook, $_page_num) = @_ }
sub set_menu_label { my ($_notebook, $_child, $_o_menu_label) = @_ }
sub set_menu_label_text { my ($_notebook, $_child, $_menu_text) = @_ }
sub set_scrollable { my ($_notebook, $_scrollable) = @_ }
sub set_show_border { my ($_notebook, $_show_border) = @_ }
sub set_show_tabs { my ($_notebook, $_show_tabs) = @_ }
sub set_tab_border { my ($_notebook, $_border_width) = @_ }
sub set_tab_hborder { my ($_notebook, $_tab_hborder) = @_ }
sub set_tab_label { my ($_notebook, $_child, $_o_tab_label) = @_ }
sub set_tab_label_packing { my ($_notebook, $_child, $_expand, $_fill, $_pack_type) = @_ }
sub set_tab_label_text { my ($_notebook, $_child, $_tab_text) = @_ }
sub set_tab_pos { my ($_notebook, $_pos) = @_ }
sub set_tab_vborder { my ($_notebook, $_tab_vborder) = @_ }

package Gtk2::Object;
our @ISA = qw();
sub destroy { my ($_object) = @_ }
sub new { my ($_class, $_object_class, @_more_paras) = @_ }

package Gtk2::OptionMenu;
our @ISA = qw();
sub get_history { my ($_option_menu) = @_ }
sub get_menu { my ($_option_menu) = @_ }
sub new { my ($_class) = @_ }
sub remove_menu { my ($_option_menu) = @_ }
sub set_history { my ($_option_menu, $_index) = @_ }
sub set_menu { my ($_option_menu, $_menu) = @_ }

package Gtk2::Paned;
our @ISA = qw();
sub add1 { my ($_paned, $_child) = @_ }
sub add2 { my ($_paned, $_child) = @_ }
sub child1 { my ($_paned) = @_ }
sub child1_resize { my ($_paned, $_o_newval) = @_ }
sub child1_shrink { my ($_paned, $_o_newval) = @_ }
sub child2 { my ($_paned) = @_ }
sub child2_resize { my ($_paned, $_o_newval) = @_ }
sub child2_shrink { my ($_paned, $_o_newval) = @_ }
sub compute_position { my ($_paned, $_allocation, $_child1_req, $_child2_req) = @_ }
sub get_position { my ($_paned) = @_ }
sub pack1 { my ($_paned, $_child, $_resize, $_shrink) = @_ }
sub pack2 { my ($_paned, $_child, $_resize, $_shrink) = @_ }
sub set_position { my ($_paned, $_position) = @_ }

package Gtk2::Pango;
our @ISA = qw();
sub PANGO_PIXELS { my ($_class, $_d) = @_ }
sub pixels { my ($_class, $_d) = @_ }
sub scale { my ($_class) = @_ }
sub scale_large { my ($_class) = @_ }
sub scale_medium { my ($_class) = @_ }
sub scale_small { my ($_class) = @_ }
sub scale_x_large { my ($_class) = @_ }
sub scale_x_small { my ($_class) = @_ }
sub scale_xx_large { my ($_class) = @_ }
sub scale_xx_small { my ($_class) = @_ }

package Gtk2::Pango::Context;
our @ISA = qw();
sub get_base_dir { my ($_context) = @_ }
sub get_font_description { my ($_context) = @_ }
sub get_language { my ($_context) = @_ }
sub get_metrics { my ($_context, $_desc, $_language) = @_ }
sub load_font { my ($_context, $_desc) = @_ }
sub load_fontset { my ($_context, $_desc, $_language) = @_ }
sub set_base_dir { my ($_context, $_direction) = @_ }
sub set_font_description { my ($_context, $_desc) = @_ }
sub set_language { my ($_context, $_language) = @_ }

package Gtk2::Pango::FontDescription;
our @ISA = qw();
sub better_match { my ($_desc, $_old_match, $_new_match) = @_ }
sub equal { my ($_desc1, $_desc2) = @_ }
sub from_string { my ($_class, $_str) = @_ }
sub get_set_fields { my ($_desc) = @_ }
sub get_size { my ($_desc) = @_ }
sub get_stretch { my ($_desc) = @_ }
sub get_style { my ($_desc) = @_ }
sub get_variant { my ($_desc) = @_ }
sub get_weight { my ($_desc) = @_ }
sub hash { my ($_desc) = @_ }
sub merge { my ($_desc, $_desc_to_merge, $_replace_existing) = @_ }
sub merge_static { my ($_desc, $_desc_to_merge, $_replace_existing) = @_ }
sub new { my ($_class) = @_ }
sub set_family { my ($_desc, $_family) = @_ }
sub set_family_static { my ($_desc, $_family) = @_ }
sub set_size { my ($_desc, $_size) = @_ }
sub set_stretch { my ($_desc, $_stretch) = @_ }
sub set_style { my ($_desc, $_style) = @_ }
sub set_variant { my ($_desc, $_variant) = @_ }
sub set_weight { my ($_desc, $_weight) = @_ }
sub to_filename { my ($_desc) = @_ }
sub to_string { my ($_desc) = @_ }
sub unset_fields { my ($_desc, $_to_unset) = @_ }

package Gtk2::Pango::FontFamily;
our @ISA = qw();
sub list_faces { my ($_family) = @_ }

package Gtk2::Pango::FontMetrics;
our @ISA = qw();
sub get_approximate_char_width { my ($_metrics) = @_ }
sub get_approximate_digit_width { my ($_metrics) = @_ }
sub get_ascent { my ($_metrics) = @_ }
sub get_descent { my ($_metrics) = @_ }
sub pango_font_get_metrics { my ($_font, $_language) = @_ }

package Gtk2::Pango::Layout;
our @ISA = qw();
sub context_changed { my ($_layout) = @_ }
sub copy { my ($_src) = @_ }
sub get_alignment { my ($_layout) = @_ }
sub get_attributes { my ($_layout) = @_ }
sub get_context { my ($_layout) = @_ }
sub get_indent { my ($_layout) = @_ }
sub get_justify { my ($_layout) = @_ }
sub get_line_count { my ($_layout) = @_ }
sub get_log_attrs { my ($_layout) = @_ }
sub get_pixel_size { my ($_layout) = @_ }
sub get_single_paragraph_mode { my ($_layout) = @_ }
sub get_size { my ($_layout) = @_ }
sub get_spacing { my ($_layout) = @_ }
sub get_tabs { my ($_layout) = @_ }
sub get_text { my ($_layout) = @_ }
sub get_width { my ($_layout) = @_ }
sub get_wrap { my ($_layout) = @_ }
sub move_cursor_visually { my ($_layout, $_strong, $_old_index, $_old_trailing, $_direction) = @_ }
sub new { my ($_class, $_context) = @_ }
sub set_alignment { my ($_layout, $_alignment) = @_ }
sub set_attributes { my ($_layout, $_attrs) = @_ }
sub set_font_description { my ($_layout, $_desc) = @_ }
sub set_indent { my ($_layout, $_newval) = @_ }
sub set_justify { my ($_layout, $_newval) = @_ }
sub set_markup { my ($_layout, $_markup, $_markup) = @_ }
sub set_markup_with_accel { my ($_layout, $_markup, $_markup, $_accel_marker) = @_ }
sub set_single_paragraph_mode { my ($_layout, $_newval) = @_ }
sub set_spacing { my ($_layout, $_newval) = @_ }
sub set_tabs { my ($_layout, $_tabs) = @_ }
sub set_text { my ($_layout, $_text, $_text) = @_ }
sub set_width { my ($_layout, $_newval) = @_ }
sub set_wrap { my ($_layout, $_wrap) = @_ }
sub xy_to_index { my ($_layout, $_x, $_y) = @_ }

package Gtk2::Pango::TabArray;
our @ISA = qw();
sub get_positions_in_pixels { my ($_tab_array) = @_ }
sub get_size { my ($_tab_array) = @_ }
sub get_tab { my ($_tab_array, $_tab_index) = @_ }
sub get_tabs { my ($_tab_array) = @_ }
sub new { my ($_class, $_initial_size, $_positions_in_pixels, @_more_paras) = @_ }
sub new_with_positions { my ($_class, $_initial_size, $_positions_in_pixels, @_more_paras) = @_ }
sub resize { my ($_tab_array, $_new_size) = @_ }
sub set_tab { my ($_tab_array, $_tab_index, $_alignment, $_location) = @_ }

package Gtk2::Plug;
our @ISA = qw();
sub construct { my ($_plug, $_socket_id) = @_ }
sub construct_for_display { my ($_plug, $_display, $_socket_id) = @_ }
sub get_id { my ($_plug) = @_ }
sub new { my ($_class, $_socket_id) = @_ }
sub new_for_display { my ($_display, $_socket_id) = @_ }

package Gtk2::ProgressBar;
our @ISA = qw();
sub get_fraction { my ($_pbar) = @_ }
sub get_orientation { my ($_pbar) = @_ }
sub get_pulse_step { my ($_pbar) = @_ }
sub get_text { my ($_pbar) = @_ }
sub new { my ($_class) = @_ }
sub pulse { my ($_pbar) = @_ }
sub set_fraction { my ($_pbar, $_fraction) = @_ }
sub set_orientation { my ($_pbar, $_orientation) = @_ }
sub set_pulse_step { my ($_pbar, $_fraction) = @_ }
sub set_text { my ($_pbar, $_text) = @_ }

package Gtk2::RadioButton;
our @ISA = qw();
sub get_group { my ($_radio_button) = @_ }
sub new { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub new_from_widget { my ($_class, $_group, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub new_with_label_from_widget { my ($_class, $_group, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub new_with_mnemonic_from_widget { my ($_class, $_group, $_o_label) = @_ }
sub set_group { my ($_radio_button, $_member_or_listref) = @_ }

package Gtk2::RadioMenuItem;
our @ISA = qw();
sub get_group { my ($_radio_menu_item) = @_ }
sub new { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_member_or_listref, $_o_label) = @_ }
sub set_group { my ($_radio_menu_item, $_member_or_listref) = @_ }

package Gtk2::Range;
our @ISA = qw();
sub get_adjustment { my ($_range) = @_ }
sub get_inverted { my ($_range) = @_ }
sub get_update_policy { my ($_range) = @_ }
sub get_value { my ($_range) = @_ }
sub set_adjustment { my ($_range, $_adjustment) = @_ }
sub set_increments { my ($_range, $_step, $_page) = @_ }
sub set_inverted { my ($_range, $_setting) = @_ }
sub set_range { my ($_range, $_min, $_max) = @_ }
sub set_update_policy { my ($_range, $_policy) = @_ }
sub set_value { my ($_range, $_value) = @_ }

package Gtk2::Rc;
our @ISA = qw();
sub add_default_file { my ($_class, $_filename) = @_ }
sub get_default_files { my ($_class) = @_ }
sub get_im_module_file { my ($_class) = @_ }
sub get_im_module_path { my ($_class) = @_ }
sub get_module_dir { my ($_class) = @_ }
sub get_style { my ($_class, $_widget) = @_ }
sub get_style_by_paths { my ($_settings, $_widget_path, $_class_path, $_package) = @_ }
sub get_theme_dir { my ($_class) = @_ }
sub parse { my ($_class, $_filename) = @_ }
sub parse_string { my ($_class, $_rc_string) = @_ }
sub reparse_all { my ($_class) = @_ }
sub reparse_all_for_settings { my ($_class, $_settings, $_force_load) = @_ }
sub set_default_files { my ($_class, @_more_paras) = @_ }

package Gtk2::RcStyle;
our @ISA = qw();
sub base { my ($_style, $_state, $_o_new) = @_ }
sub bg { my ($_style, $_state, $_o_new) = @_ }
sub bg_pixmap_name { my ($_style, $_state, $_o_new) = @_ }
sub color_flags { my ($_style, $_state, $_o_new) = @_ }
sub copy { my ($_orig) = @_ }
sub fg { my ($_style, $_state, $_o_new) = @_ }
sub font_desc { my ($_style, $_o_new) = @_ }
sub name { my ($_style, $_o_new) = @_ }
sub new { my ($_class) = @_ }
sub text { my ($_style, $_state, $_o_new) = @_ }
sub xthickness { my ($_style, $_o_new) = @_ }
sub ythickness { my ($_style, $_o_new) = @_ }

package Gtk2::Requisition;
our @ISA = qw();
sub height { my ($_requisition, $_o_newval) = @_ }
sub new { my ($_class, $_o_width, $_o_height) = @_ }
sub width { my ($_requisition, $_o_newval) = @_ }

package Gtk2::Ruler;
our @ISA = qw();
sub draw_pos { my ($_ruler) = @_ }
sub draw_ticks { my ($_ruler) = @_ }
sub get_metric { my ($_ruler) = @_ }
sub get_range { my ($_ruler) = @_ }
sub set_metric { my ($_ruler, $_metric) = @_ }
sub set_range { my ($_ruler, $_lower, $_upper, $_position, $_max_size) = @_ }

package Gtk2::Scale;
our @ISA = qw();
sub get_digits { my ($_scale) = @_ }
sub get_draw_value { my ($_scale) = @_ }
sub get_value_pos { my ($_scale) = @_ }
sub set_digits { my ($_scale, $_digits) = @_ }
sub set_draw_value { my ($_scale, $_draw_value) = @_ }
sub set_value_pos { my ($_scale, $_pos) = @_ }

package Gtk2::ScrolledWindow;
our @ISA = qw();
sub add_with_viewport { my ($_scrolled_window, $_child) = @_ }
sub get_hadjustment { my ($_scrolled_window) = @_ }
sub get_placement { my ($_scrolled_window) = @_ }
sub get_policy { my ($_scrolled_window) = @_ }
sub get_shadow_type { my ($_scrolled_window) = @_ }
sub get_vadjustment { my ($_scrolled_window) = @_ }
sub new { my ($_class, $_o_hadjustment, $_o_vadjustment) = @_ }
sub set_hadjustment { my ($_scrolled_window, $_hadjustment) = @_ }
sub set_placement { my ($_scrolled_window, $_window_placement) = @_ }
sub set_policy { my ($_scrolled_window, $_hscrollbar_policy, $_vscrollbar_policy) = @_ }
sub set_shadow_type { my ($_scrolled_window, $_type) = @_ }
sub set_vadjustment { my ($_scrolled_window, $_hadjustment) = @_ }

package Gtk2::Selection;
our @ISA = qw();
sub owner_set { my ($_class, $_widget, $_selection, $_time_) = @_ }
sub owner_set_for_display { my ($_class, $_display, $_widget, $_selection, $_time_) = @_ }

package Gtk2::SelectionData;
our @ISA = qw();
sub data { my ($_d) = @_ }
sub display { my ($_d) = @_ }
sub Gtk2::SelectionData::format { my ($_d) = @_ }
sub get_row_drag_data { my ($_selection_data) = @_ }
sub get_targets { my ($_selection_data) = @_ }
sub get_text { my ($_selection_data) = @_ }
sub gtk_selection_clear { my ($_widget, $_event) = @_ }
sub Gtk2::SelectionData::length { my ($_d) = @_ }
sub selection { my ($_d) = @_ }
sub set { my ($_selection_data, $_type, $_format, $_data) = @_ }
sub set_row_drag_data { my ($_selection_data, $_tree_model, $_path) = @_ }
sub set_text { my ($_selection_data, $_str, $_o_len) = @_ }
sub target { my ($_d) = @_ }
sub targets_include_text { my ($_selection_data) = @_ }
sub type { my ($_d) = @_ }

package Gtk2::SeparatorMenuItem;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::SizeGroup;
our @ISA = qw();
sub add_widget { my ($_size_group, $_widget) = @_ }
sub get_mode { my ($_size_group) = @_ }
sub new { my ($_class, $_mode) = @_ }
sub remove_widget { my ($_size_group, $_widget) = @_ }
sub set_mode { my ($_size_group, $_mode) = @_ }

package Gtk2::Socket;
our @ISA = qw();
sub add_id { my ($_socket, $_window_id) = @_ }
sub get_id { my ($_socket) = @_ }
sub new { my ($_class) = @_ }
sub steal { my ($_socket, $_wid) = @_ }

package Gtk2::SpinButton;
our @ISA = qw();
sub configure { my ($_spin_button, $_adjustment, $_climb_rate, $_digits) = @_ }
sub get_adjustment { my ($_spin_button) = @_ }
sub get_digits { my ($_spin_button) = @_ }
sub get_increments { my ($_spin_button) = @_ }
sub get_numeric { my ($_spin_button) = @_ }
sub get_range { my ($_spin_button) = @_ }
sub get_snap_to_ticks { my ($_spin_button) = @_ }
sub get_update_policy { my ($_spin_button) = @_ }
sub get_value { my ($_spin_button) = @_ }
sub get_value_as_int { my ($_spin_button) = @_ }
sub get_wrap { my ($_spin_button) = @_ }
sub new { my ($_class, $_adjustment, $_climb_rate, $_digits) = @_ }
sub new_with_range { my ($_class, $_min, $_max, $_step) = @_ }
sub set_adjustment { my ($_spin_button, $_adjustment) = @_ }
sub set_digits { my ($_spin_button, $_digits) = @_ }
sub set_increments { my ($_spin_button, $_step, $_page) = @_ }
sub set_numeric { my ($_spin_button, $_numeric) = @_ }
sub set_range { my ($_spin_button, $_min, $_max) = @_ }
sub set_snap_to_ticks { my ($_spin_button, $_snap_to_ticks) = @_ }
sub set_update_policy { my ($_spin_button, $_policy) = @_ }
sub set_value { my ($_spin_button, $_value) = @_ }
sub set_wrap { my ($_spin_button, $_wrap) = @_ }
sub spin { my ($_spin_button, $_direction, $_increment) = @_ }
sub update { my ($_spin_button) = @_ }

package Gtk2::Statusbar;
our @ISA = qw();
sub get_context_id { my ($_statusbar, $_context_description) = @_ }
sub get_has_resize_grip { my ($_statusbar) = @_ }
sub new { my ($_class) = @_ }
sub pop { my ($_statusbar, $_context_id) = @_ }
sub push { my ($_statusbar, $_context_id, $_text) = @_ }
sub remove { my ($_statusbar, $_context_id, $_message_id) = @_ }
sub set_has_resize_grip { my ($_statusbar, $_setting) = @_ }

package Gtk2::Stock;
our @ISA = qw();
sub add { my ($_class, @_more_paras) = @_ }
sub list_ids { my ($_class) = @_ }
sub lookup { my ($_class, $_stock_id) = @_ }

package Gtk2::Style;
our @ISA = qw();
sub apply_default_background { my ($_style, $_window, $_set_bg, $_state_type, $_area, $_x, $_y, $_width, $_height) = @_ }
sub attach { my ($_style, $_window) = @_ }
sub base { my ($_style, $_state) = @_ }
sub base_gc { my ($_style, $_state) = @_ }
sub bg { my ($_style, $_state) = @_ }
sub bg_gc { my ($_style, $_state) = @_ }
sub bg_pixmap { my ($_style, $_state) = @_ }
sub black { my ($_style) = @_ }
sub black_gc { my ($_style) = @_ }
sub copy { my ($_style) = @_ }
sub dark { my ($_style, $_state) = @_ }
sub dark_gc { my ($_style, $_state) = @_ }
sub detach { my ($_style) = @_ }
sub fg { my ($_style, $_state) = @_ }
sub fg_gc { my ($_style, $_state) = @_ }
sub font_desc { my ($_style) = @_ }
sub light { my ($_style, $_state) = @_ }
sub light_gc { my ($_style, $_state) = @_ }
sub lookup_icon_set { my ($_style, $_stock_id) = @_ }
sub mid { my ($_style, $_state) = @_ }
sub mid_gc { my ($_style, $_state) = @_ }
sub new { my ($_class) = @_ }
sub paint_arrow { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_arrow_type, $_fill, $_x, $_y, $_width, $_height) = @_ }
sub paint_box { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_box_gap { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height, $_gap_side, $_gap_x, $_gap_width) = @_ }
sub paint_check { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_diamond { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_expander { my ($_style, $_window, $_state_type, $_area, $_widget, $_detail, $_x, $_y, $_expander_style) = @_ }
sub paint_extension { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height, $_gap_side) = @_ }
sub paint_flat_box { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_focus { my ($_style, $_window, $_state_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_handle { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height, $_orientation) = @_ }
sub paint_hline { my ($_style, $_window, $_state_type, $_area, $_widget, $_detail, $_x1, $_x2, $_y) = @_ }
sub paint_layout { my ($_style, $_window, $_state_type, $_use_text, $_area, $_widget, $_detail, $_x, $_y, $_layout) = @_ }
sub paint_option { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_polygon { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_fill, $_x1, $_y1, @_more_paras) = @_ }
sub paint_resize_grip { my ($_style, $_window, $_state_type, $_area, $_widget, $_detail, $_edge, $_x, $_y, $_width, $_height) = @_ }
sub paint_shadow { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_shadow_gap { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height, $_gap_side, $_gap_x, $_gap_width) = @_ }
sub paint_slider { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height, $_orientation) = @_ }
sub paint_tab { my ($_style, $_window, $_state_type, $_shadow_type, $_area, $_widget, $_detail, $_x, $_y, $_width, $_height) = @_ }
sub paint_vline { my ($_style, $_window, $_state_type, $_area, $_widget, $_detail, $_y1_, $_y2_, $_x) = @_ }
sub render_icon { my ($_style, $_source, $_direction, $_state, $_size, $_widget, $_o_detail) = @_ }
sub set_background { my ($_style, $_window, $_state_type) = @_ }
sub text { my ($_style, $_state) = @_ }
sub text_aa { my ($_style, $_state) = @_ }
sub text_aa_gc { my ($_style, $_state) = @_ }
sub text_gc { my ($_style, $_state) = @_ }
sub white { my ($_style) = @_ }
sub white_gc { my ($_style) = @_ }
sub xthickness { my ($_style) = @_ }
sub ythickness { my ($_style) = @_ }

package Gtk2::Table;
our @ISA = qw();
sub attach { my ($_table, $_child, $_left_attach, $_right_attach, $_top_attach, $_bottom_attach, $_xoptions, $_yoptions, $_xpadding, $_ypadding) = @_ }
sub attach_defaults { my ($_table, $_widget, $_left_attach, $_right_attach, $_top_attach, $_bottom_attach) = @_ }
sub get_col_spacing { my ($_table, $_column) = @_ }
sub get_default_col_spacing { my ($_table) = @_ }
sub get_default_row_spacing { my ($_table) = @_ }
sub get_homogeneous { my ($_table) = @_ }
sub get_row_spacing { my ($_table, $_row) = @_ }
sub new { my ($_class, $_rows, $_columns, $_o_homogeneous) = @_ }
sub resize { my ($_table, $_rows, $_columns) = @_ }
sub set_col_spacing { my ($_table, $_column, $_spacing) = @_ }
sub set_col_spacings { my ($_table, $_spacing) = @_ }
sub set_homogeneous { my ($_table, $_homogeneous) = @_ }
sub set_row_spacing { my ($_table, $_row, $_spacing) = @_ }
sub set_row_spacings { my ($_table, $_spacing) = @_ }

package Gtk2::TargetList;
our @ISA = qw();
sub DESTROY { my ($_list) = @_ }
sub add { my ($_list, $_target, $_flags, $_info) = @_ }
sub add_table { my ($_list, @_more_paras) = @_ }
sub find { my ($_list, $_target) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub remove { my ($_list, $_target) = @_ }

package Gtk2::TearoffMenuItem;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::TextAttributes;
our @ISA = qw();
sub copy_values { my ($_dest, $_src) = @_ }
sub new { my ($_class) = @_ }

package Gtk2::TextBuffer;
our @ISA = qw();
sub add_selection_clipboard { my ($_buffer, $_clipboard) = @_ }
sub apply_tag { my ($_buffer, $_tag, $_start, $_end) = @_ }
sub apply_tag_by_name { my ($_buffer, $_name, $_start, $_end) = @_ }
sub begin_user_action { my ($_buffer) = @_ }
sub copy_clipboard { my ($_buffer, $_clipboard) = @_ }
sub create_child_anchor { my ($_buffer, $_iter) = @_ }
sub create_mark { my ($_buffer, $_mark_name, $_where, $_left_gravity) = @_ }
sub create_tag { my ($_buffer, $_tag_name, $_property_name1, $_property_value1, @_more_paras) = @_ }
sub cut_clipboard { my ($_buffer, $_clipboard, $_default_editable) = @_ }
sub delete { my ($_buffer, $_start, $_end) = @_ }
sub delete_interactive { my ($_buffer, $_start_iter, $_end_iter, $_default_editable) = @_ }
sub delete_mark { my ($_buffer, $_mark) = @_ }
sub delete_mark_by_name { my ($_buffer, $_name) = @_ }
sub delete_selection { my ($_buffer, $_interactive, $_default_editable) = @_ }
sub end_user_action { my ($_buffer) = @_ }
sub get_bounds { my ($_buffer) = @_ }
sub get_char_count { my ($_buffer) = @_ }
sub get_end_iter { my ($_buffer) = @_ }
sub get_insert { my ($_buffer) = @_ }
sub get_iter_at_child_anchor { my ($_buffer, $_anchor) = @_ }
sub get_iter_at_line { my ($_buffer, $_line_number) = @_ }
sub get_iter_at_line_index { my ($_buffer, $_line_number, $_byte_index) = @_ }
sub get_iter_at_line_offset { my ($_buffer, $_line_number, $_char_offset) = @_ }
sub get_iter_at_mark { my ($_buffer, $_mark) = @_ }
sub get_iter_at_offset { my ($_buffer, $_char_offset) = @_ }
sub get_line_count { my ($_buffer) = @_ }
sub get_mark { my ($_buffer, $_name) = @_ }
sub get_modified { my ($_buffer) = @_ }
sub get_selection_bound { my ($_buffer) = @_ }
sub get_selection_bounds { my ($_buffer) = @_ }
sub get_slice { my ($_buffer, $_start, $_end, $_include_hidden_chars) = @_ }
sub get_start_iter { my ($_buffer) = @_ }
sub get_tag_table { my ($_buffer) = @_ }
sub get_text { my ($_buffer, $_start, $_end, $_include_hidden_chars) = @_ }
sub insert { my ($_buffer, $_iter, $_text, $_text) = @_ }
sub insert_at_cursor { my ($_buffer, $_text, $_text) = @_ }
sub insert_child_anchor { my ($_buffer, $_iter, $_anchor) = @_ }
sub insert_interactive { my ($_buffer, $_iter, $_text, $_text, $_default_editable) = @_ }
sub insert_interactive_at_cursor { my ($_buffer, $_text, $_text, $_default_editable) = @_ }
sub insert_pixbuf { my ($_buffer, $_iter, $_pixbuf) = @_ }
sub insert_range { my ($_buffer, $_iter, $_start, $_end) = @_ }
sub insert_range_interactive { my ($_buffer, $_iter, $_start, $_end, $_default_editable) = @_ }
sub insert_with_tags { my ($_buffer, $_iter, $_text, @_more_paras) = @_ }
sub insert_with_tags_by_name { my ($_buffer, $_iter, $_text, @_more_paras) = @_ }
sub move_mark { my ($_buffer, $_mark, $_where) = @_ }
sub move_mark_by_name { my ($_buffer, $_name, $_where) = @_ }
sub new { my ($_class, $_o_tagtable) = @_ }
sub paste_clipboard { my ($_buffer, $_clipboard, $_override_location, $_default_editable) = @_ }
sub place_cursor { my ($_buffer, $_where) = @_ }
sub remove_all_tags { my ($_buffer, $_start, $_end) = @_ }
sub remove_selection_clipboard { my ($_buffer, $_clipboard) = @_ }
sub remove_tag { my ($_buffer, $_tag, $_start, $_end) = @_ }
sub remove_tag_by_name { my ($_buffer, $_name, $_start, $_end) = @_ }
sub set_modified { my ($_buffer, $_setting) = @_ }
sub set_text { my ($_buffer, $_text, $_text) = @_ }

package Gtk2::TextIter;
our @ISA = qw();
sub backward_char { my ($_iter) = @_ }
sub backward_chars { my ($_iter, $_count) = @_ }
sub backward_cursor_position { my ($_iter) = @_ }
sub backward_cursor_positions { my ($_iter, $_count) = @_ }
sub backward_find_char { my ($_iter, $_pred, $_o_user_data, $_o_limit) = @_ }
sub backward_line { my ($_iter) = @_ }
sub backward_lines { my ($_iter, $_count) = @_ }
sub backward_search { my ($_iter, $_str, $_flags, $_o_limit) = @_ }
sub backward_sentence_start { my ($_iter) = @_ }
sub backward_sentence_starts { my ($_iter, $_count) = @_ }
sub backward_to_tag_toggle { my ($_iter, $_tag) = @_ }
sub backward_word_start { my ($_iter) = @_ }
sub backward_word_starts { my ($_iter, $_count) = @_ }
sub begins_tag { my ($_iter, $_tag) = @_ }
sub can_insert { my ($_iter, $_default_editability) = @_ }
sub compare { my ($_lhs, $_rhs) = @_ }
sub editable { my ($_iter, $_default_setting) = @_ }
sub ends_line { my ($_iter) = @_ }
sub ends_sentence { my ($_iter) = @_ }
sub ends_tag { my ($_iter, $_tag) = @_ }
sub ends_word { my ($_iter) = @_ }
sub equal { my ($_lhs, $_rhs) = @_ }
sub forward_char { my ($_iter) = @_ }
sub forward_chars { my ($_iter, $_count) = @_ }
sub forward_cursor_position { my ($_iter) = @_ }
sub forward_cursor_positions { my ($_iter, $_count) = @_ }
sub forward_find_char { my ($_iter, $_pred, $_o_user_data, $_o_limit) = @_ }
sub forward_line { my ($_iter) = @_ }
sub forward_lines { my ($_iter, $_count) = @_ }
sub forward_search { my ($_iter, $_str, $_flags, $_o_limit) = @_ }
sub forward_sentence_end { my ($_iter) = @_ }
sub forward_sentence_ends { my ($_iter, $_count) = @_ }
sub forward_to_end { my ($_iter) = @_ }
sub forward_to_line_end { my ($_iter) = @_ }
sub forward_to_tag_toggle { my ($_iter, $_tag) = @_ }
sub forward_word_end { my ($_iter) = @_ }
sub forward_word_ends { my ($_iter, $_count) = @_ }
sub get_attributes { my ($_iter) = @_ }
sub get_buffer { my ($_iter) = @_ }
sub get_bytes_in_line { my ($_iter) = @_ }
sub get_char { my ($_iter) = @_ }
sub get_chars_in_line { my ($_iter) = @_ }
sub get_child_anchor { my ($_iter) = @_ }
sub get_language { my ($_iter) = @_ }
sub get_line { my ($_iter) = @_ }
sub get_line_index { my ($_iter) = @_ }
sub get_line_offset { my ($_iter) = @_ }
sub get_marks { my ($_iter) = @_ }
sub get_offset { my ($_iter) = @_ }
sub get_pixbuf { my ($_iter) = @_ }
sub get_slice { my ($_start, $_end) = @_ }
sub get_tags { my ($_iter) = @_ }
sub get_text { my ($_start, $_end) = @_ }
sub get_toggled_tags { my ($_iter, $_toggled_on) = @_ }
sub get_visible_line_index { my ($_iter) = @_ }
sub get_visible_line_offset { my ($_iter) = @_ }
sub get_visible_slice { my ($_start, $_end) = @_ }
sub get_visible_text { my ($_start, $_end) = @_ }
sub has_tag { my ($_iter, $_tag) = @_ }
sub in_range { my ($_iter, $_start, $_end) = @_ }
sub inside_sentence { my ($_iter) = @_ }
sub inside_word { my ($_iter) = @_ }
sub is_cursor_position { my ($_iter) = @_ }
sub is_end { my ($_iter) = @_ }
sub is_start { my ($_iter) = @_ }
sub order { my ($_first, $_second) = @_ }
sub set_line { my ($_iter, $_line_number) = @_ }
sub set_line_index { my ($_iter, $_byte_on_line) = @_ }
sub set_line_offset { my ($_iter, $_char_on_line) = @_ }
sub set_offset { my ($_iter, $_char_offset) = @_ }
sub set_visible_line_index { my ($_iter, $_byte_on_line) = @_ }
sub set_visible_line_offset { my ($_iter, $_char_on_line) = @_ }
sub starts_line { my ($_iter) = @_ }
sub starts_sentence { my ($_iter) = @_ }
sub starts_word { my ($_iter) = @_ }
sub toggles_tag { my ($_iter, $_tag) = @_ }

package Gtk2::TextMark;
our @ISA = qw();
sub get_buffer { my ($_mark) = @_ }
sub get_deleted { my ($_mark) = @_ }
sub get_left_gravity { my ($_mark) = @_ }
sub get_name { my ($_mark) = @_ }
sub get_visible { my ($_mark) = @_ }
sub set_visible { my ($_mark, $_setting) = @_ }

package Gtk2::TextTag;
our @ISA = qw();
sub event { my ($_tag, $_event_object, $_event, $_iter) = @_ }
sub get_priority { my ($_tag) = @_ }
sub new { my ($_class, $_o_name) = @_ }
sub set_priority { my ($_tag, $_priority) = @_ }

package Gtk2::TextTagTable;
our @ISA = qw();
sub add { my ($_table, $_tag) = @_ }
sub Gtk2::TextTagTable::foreach { my ($_table, $_callback, $_o_callback_data) = @_ }
sub get_size { my ($_table) = @_ }
sub lookup { my ($_table, $_name) = @_ }
sub new { my ($_class) = @_ }
sub remove { my ($_table, $_tag) = @_ }

package Gtk2::TextView;
our @ISA = qw();
sub add_child_at_anchor { my ($_text_view, $_child, $_anchor) = @_ }
sub add_child_in_window { my ($_text_view, $_child, $_which_window, $_xpos, $_ypos) = @_ }
sub backward_display_line { my ($_text_view, $_iter) = @_ }
sub backward_display_line_start { my ($_text_view, $_iter) = @_ }
sub buffer_to_window_coords { my ($_text_view, $_win, $_buffer_x, $_buffer_y) = @_ }
sub forward_display_line { my ($_text_view, $_iter) = @_ }
sub forward_display_line_end { my ($_text_view, $_iter) = @_ }
sub get_border_window_size { my ($_text_view, $_type) = @_ }
sub get_buffer { my ($_text_view) = @_ }
sub get_cursor_visible { my ($_text_view) = @_ }
sub get_default_attributes { my ($_text_view) = @_ }
sub get_editable { my ($_text_view) = @_ }
sub get_indent { my ($_text_view) = @_ }
sub get_iter_at_location { my ($_text_view, $_x, $_y) = @_ }
sub get_iter_location { my ($_text_view, $_iter) = @_ }
sub get_justification { my ($_text_view) = @_ }
sub get_left_margin { my ($_text_view) = @_ }
sub get_line_at_y { my ($_text_view, $_target_iter, $_y) = @_ }
sub get_line_yrange { my ($_text_view, $_iter) = @_ }
sub get_pixels_above_lines { my ($_text_view) = @_ }
sub get_pixels_below_lines { my ($_text_view) = @_ }
sub get_pixels_inside_wrap { my ($_text_view) = @_ }
sub get_right_margin { my ($_text_view) = @_ }
sub get_tabs { my ($_text_view) = @_ }
sub get_visible_rect { my ($_text_view) = @_ }
sub get_window { my ($_text_view, $_win) = @_ }
sub get_window_type { my ($_text_view, $_window) = @_ }
sub get_wrap_mode { my ($_text_view) = @_ }
sub move_child { my ($_text_view, $_child, $_xpos, $_ypos) = @_ }
sub move_mark_onscreen { my ($_text_view, $_mark) = @_ }
sub move_visually { my ($_text_view, $_iter, $_count) = @_ }
sub new { my ($_class) = @_ }
sub new_with_buffer { my ($_class, $_buffer) = @_ }
sub place_cursor_onscreen { my ($_text_view) = @_ }
sub scroll_mark_onscreen { my ($_text_view, $_mark) = @_ }
sub scroll_to_iter { my ($_text_view, $_iter, $_within_margin, $_use_align, $_xalign, $_yalign) = @_ }
sub scroll_to_mark { my ($_text_view, $_mark, $_within_margin, $_use_align, $_xalign, $_yalign) = @_ }
sub set_border_window_size { my ($_text_view, $_type, $_size) = @_ }
sub set_buffer { my ($_text_view, $_buffer) = @_ }
sub set_cursor_visible { my ($_text_view, $_setting) = @_ }
sub set_editable { my ($_text_view, $_setting) = @_ }
sub set_indent { my ($_text_view, $_indent) = @_ }
sub set_justification { my ($_text_view, $_justification) = @_ }
sub set_left_margin { my ($_text_view, $_left_margin) = @_ }
sub set_pixels_above_lines { my ($_text_view, $_pixels_above_lines) = @_ }
sub set_pixels_below_lines { my ($_text_view, $_pixels_below_lines) = @_ }
sub set_pixels_inside_wrap { my ($_text_view, $_pixels_inside_wrap) = @_ }
sub set_right_margin { my ($_text_view, $_right_margin) = @_ }
sub set_tabs { my ($_text_view, $_tabs) = @_ }
sub set_wrap_mode { my ($_text_view, $_wrap_mode) = @_ }
sub starts_display_line { my ($_text_view, $_iter) = @_ }
sub window_to_buffer_coords { my ($_text_view, $_win, $_window_x, $_window_y) = @_ }

package Gtk2::ToggleButton;
our @ISA = qw();
sub get_active { my ($_toggle_button) = @_ }
sub get_inconsistent { my ($_toggle_button) = @_ }
sub get_mode { my ($_toggle_button) = @_ }
sub new { my ($_class, $_o_label) = @_ }
sub new_with_label { my ($_class, $_o_label) = @_ }
sub new_with_mnemonic { my ($_class, $_o_label) = @_ }
sub set_active { my ($_toggle_button, $_is_active) = @_ }
sub set_inconsistent { my ($_toggle_button, $_setting) = @_ }
sub set_mode { my ($_toggle_button, $_draw_indicator) = @_ }
sub toggled { my ($_toggle_button) = @_ }

package Gtk2::Toolbar;
our @ISA = qw();
sub append_element { my ($_toolbar, $_type, $_widget, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_o_callback, $_o_user_data) = @_ }
sub append_item { my ($_toolbar, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_o_callback, $_o_user_data) = @_ }
sub append_space { my ($_toolbar) = @_ }
sub append_widget { my ($_toolbar, $_widget, $_tooltip_text, $_tooltip_private_text) = @_ }
sub get_icon_size { my ($_toolbar) = @_ }
sub get_orientation { my ($_toolbar) = @_ }
sub get_style { my ($_toolbar) = @_ }
sub get_tooltips { my ($_toolbar) = @_ }
sub insert_element { my ($_toolbar, $_type, $_widget, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_callback, $_user_data, $_position) = @_ }
sub insert_item { my ($_toolbar, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_callback, $_user_data, $_position) = @_ }
sub insert_space { my ($_toolbar, $_position) = @_ }
sub insert_stock { my ($_toolbar, $_stock_id, $_tooltip_text, $_tooltip_private_text, $_callback, $_user_data, $_position) = @_ }
sub insert_widget { my ($_toolbar, $_widget, $_tooltip_text, $_tooltip_private_text, $_position) = @_ }
sub new { my ($_class) = @_ }
sub prepend_element { my ($_toolbar, $_type, $_widget, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_o_callback, $_o_user_data) = @_ }
sub prepend_item { my ($_toolbar, $_text, $_tooltip_text, $_tooltip_private_text, $_icon, $_o_callback, $_o_user_data) = @_ }
sub prepend_space { my ($_toolbar) = @_ }
sub prepend_widget { my ($_toolbar, $_widget, $_tooltip_text, $_tooltip_private_text) = @_ }
sub remove_space { my ($_toolbar, $_position) = @_ }
sub set_icon_size { my ($_toolbar, $_icon_size) = @_ }
sub set_orientation { my ($_toolbar, $_orientation) = @_ }
sub set_style { my ($_toolbar, $_style) = @_ }
sub set_tooltips { my ($_toolbar, $_enable) = @_ }
sub unset_icon_size { my ($_toolbar) = @_ }
sub unset_style { my ($_toolbar) = @_ }

package Gtk2::Tooltips;
our @ISA = qw();
sub data_get { my ($_class, $_widget) = @_ }
sub disable { my ($_tooltips) = @_ }
sub enable { my ($_tooltips) = @_ }
sub force_window { my ($_tooltips) = @_ }
sub new { my ($_class) = @_ }
sub set_tip { my ($_tooltips, $_widget, $_tip_text, $_o_tip_private) = @_ }

package Gtk2::TreeDragDest;
our @ISA = qw();
sub drag_data_received { my ($_drag_dest, $_dest, $_selection_data) = @_ }
sub row_drop_possible { my ($_drag_dest, $_dest_path, $_selection_data) = @_ }

package Gtk2::TreeDragSource;
our @ISA = qw();
sub drag_data_delete { my ($_drag_source, $_path) = @_ }
sub drag_data_get { my ($_drag_source, $_path) = @_ }
sub row_draggable { my ($_drag_source, $_path) = @_ }

package Gtk2::TreeModel;
our @ISA = qw();
sub Gtk2::TreeModel::foreach { my ($_model, $_func, $_o_user_data) = @_ }
sub get { my ($_tree_model, $_iter, @_more_paras) = @_ }
sub get_column_type { my ($_tree_model, $_index_) = @_ }
sub get_flags { my ($_tree_model) = @_ }
sub get_iter { my ($_tree_model, $_path) = @_ }
sub get_iter_first { my ($_tree_model) = @_ }
sub get_iter_from_string { my ($_tree_model, $_path_string) = @_ }
sub get_n_columns { my ($_tree_model) = @_ }
sub get_path { my ($_tree_model, $_iter) = @_ }
sub get_string_from_iter { my ($_tree_model, $_iter) = @_ }
sub get_value { my ($_tree_model, $_iter, @_more_paras) = @_ }
sub iter_children { my ($_tree_model, $_parent) = @_ }
sub iter_has_child { my ($_tree_model, $_iter) = @_ }
sub iter_n_children { my ($_tree_model, $_o_iter) = @_ }
sub iter_next { my ($_tree_model, $_iter) = @_ }
sub iter_nth_child { my ($_tree_model, $_parent, $_n) = @_ }
sub iter_parent { my ($_tree_model, $_child) = @_ }
sub row_changed { my ($_tree_model, $_path, $_iter) = @_ }
sub row_deleted { my ($_tree_model, $_path) = @_ }
sub row_has_child_toggled { my ($_tree_model, $_path, $_iter) = @_ }
sub row_inserted { my ($_tree_model, $_path, $_iter) = @_ }
sub rows_reordered { my ($_tree_model, $_path, $_iter, @_more_paras) = @_ }

package Gtk2::TreeModelSort;
our @ISA = qw();
sub convert_child_iter_to_iter { my ($_tree_model_sort, $_child_iter) = @_ }
sub convert_child_path_to_path { my ($_tree_model_sort, $_child_path) = @_ }
sub convert_iter_to_child_iter { my ($_tree_model_sort, $_sorted_iter) = @_ }
sub convert_path_to_child_path { my ($_tree_model_sort, $_sorted_path) = @_ }
sub get_model { my ($_tree_model) = @_ }
sub iter_is_valid { my ($_tree_model_sort, $_iter) = @_ }
sub new_with_model { my ($_child_model) = @_ }
sub reset_default_sort_func { my ($_tree_model_sort) = @_ }

package Gtk2::TreePath;
our @ISA = qw();
sub append_index { my ($_path, $_index_) = @_ }
sub compare { my ($_a, $_b) = @_ }
sub down { my ($_path) = @_ }
sub get_depth { my ($_path) = @_ }
sub get_indices { my ($_path) = @_ }
sub is_ancestor { my ($_path, $_descendant) = @_ }
sub is_descendant { my ($_path, $_ancestor) = @_ }
sub new { my ($_class, $_o_path) = @_ }
sub new_first { my ($_class) = @_ }
sub new_from_indices { my ($_class, $_first_index, @_more_paras) = @_ }
sub new_from_string { my ($_class, $_o_path) = @_ }
sub next { my ($_path) = @_ }
sub prepend_index { my ($_path, $_index_) = @_ }
sub prev { my ($_path) = @_ }
sub to_string { my ($_path) = @_ }
sub up { my ($_path) = @_ }

package Gtk2::TreeRowReference;
our @ISA = qw();
sub get_path { my ($_reference) = @_ }
sub new { my ($_class, $_model, $_path) = @_ }
sub valid { my ($_reference) = @_ }

package Gtk2::TreeSelection;
our @ISA = qw();
sub count_selected_rows { my ($_selection) = @_ }
sub get_mode { my ($_selection) = @_ }
sub get_selected { my ($_selection) = @_ }
sub get_selected_rows { my ($_selection) = @_ }
sub get_tree_view { my ($_selection) = @_ }
sub iter_is_selected { my ($_selection, $_iter) = @_ }
sub path_is_selected { my ($_selection, $_path) = @_ }
sub select_all { my ($_selection) = @_ }
sub select_iter { my ($_selection, $_iter) = @_ }
sub select_path { my ($_selection, $_path) = @_ }
sub select_range { my ($_selection, $_start_path, $_end_path) = @_ }
sub selected_foreach { my ($_selection, $_func, $_o_data) = @_ }
sub set_mode { my ($_selection, $_type) = @_ }
sub set_select_function { my ($_selection, $_func, $_o_data) = @_ }
sub unselect_all { my ($_selection) = @_ }
sub unselect_iter { my ($_selection, $_iter) = @_ }
sub unselect_path { my ($_selection, $_path) = @_ }
sub unselect_range { my ($_selection, $_start_path, $_end_path) = @_ }

package Gtk2::TreeSortable;
our @ISA = qw();
sub get_sort_column_id { my ($_sortable) = @_ }
sub has_default_sort_func { my ($_sortable) = @_ }
sub set_default_sort_func { my ($_sortable, $_sort_func, $_o_user_data) = @_ }
sub set_sort_column_id { my ($_sortable, $_sort_column_id, $_order) = @_ }
sub set_sort_func { my ($_sortable, $_sort_column_id, $_sort_func, $_o_user_data) = @_ }
sub sort_column_changed { my ($_sortable) = @_ }

package Gtk2::TreeStore;
our @ISA = qw();
sub append { my ($_tree_store, $_parent) = @_ }
sub clear { my ($_tree_store) = @_ }
sub insert { my ($_tree_store, $_parent, $_position) = @_ }
sub insert_after { my ($_tree_store, $_parent, $_sibling) = @_ }
sub insert_before { my ($_tree_store, $_parent, $_sibling) = @_ }
sub is_ancestor { my ($_tree_store, $_iter, $_descendant) = @_ }
sub iter_depth { my ($_tree_store, $_iter) = @_ }
sub iter_is_valid { my ($_tree_store, $_iter) = @_ }
sub move_after { my ($_tree_store, $_iter, $_position) = @_ }
sub move_before { my ($_tree_store, $_iter, $_position) = @_ }
sub new { my ($_class, @_more_paras) = @_ }
sub prepend { my ($_tree_store, $_parent) = @_ }
sub remove { my ($_tree_store, $_iter) = @_ }
sub reorder { my ($_tree_store, $_parent, @_more_paras) = @_ }
sub set { my ($_tree_store, $_iter, $_col1, $_val1, @_more_paras) = @_ }
sub set_column_types { my ($_tree_store, @_more_paras) = @_ }
sub swap { my ($_tree_store, $_a, $_b) = @_ }

package Gtk2::TreeView;
our @ISA = qw();
sub append_column { my ($_tree_view, $_column) = @_ }
sub collapse_all { my ($_tree_view) = @_ }
sub collapse_row { my ($_tree_view, $_path) = @_ }
sub columns_autosize { my ($_tree_view) = @_ }
sub enable_model_drag_dest { my ($_tree_view, $_actions, @_more_paras) = @_ }
sub enable_model_drag_source { my ($_tree_view, $_start_button_mask, $_actions, @_more_paras) = @_ }
sub expand_all { my ($_tree_view) = @_ }
sub expand_row { my ($_tree_view, $_path, $_open_all) = @_ }
sub expand_to_path { my ($_tree_view, $_path) = @_ }
sub get_background_area { my ($_tree_view, $_path, $_column) = @_ }
sub get_bin_window { my ($_tree_view) = @_ }
sub get_cell_area { my ($_tree_view, $_path, $_column) = @_ }
sub get_column { my ($_tree_view, $_n) = @_ }
sub get_columns { my ($_tree_view) = @_ }
sub get_cursor { my ($_tree_view) = @_ }
sub get_dest_row_at_pos { my ($_tree_view, $_drag_x, $_drag_y) = @_ }
sub get_drag_dest_row { my ($_tree_view) = @_ }
sub get_enable_search { my ($_tree_view) = @_ }
sub get_expander_column { my ($_tree_view) = @_ }
sub get_hadjustment { my ($_tree_view) = @_ }
sub get_headers_visible { my ($_tree_view) = @_ }
sub get_model { my ($_tree_view) = @_ }
sub get_path_at_pos { my ($_tree_view, $_x, $_y) = @_ }
sub get_reorderable { my ($_tree_view) = @_ }
sub get_rules_hint { my ($_tree_view) = @_ }
sub get_search_column { my ($_tree_view) = @_ }
sub get_selection { my ($_tree_view) = @_ }
sub get_vadjustment { my ($_tree_view) = @_ }
sub get_visible_rect { my ($_tree_view) = @_ }
sub insert_column { my ($_tree_view, $_column, $_position) = @_ }
sub insert_column_with_attributes { my ($_tree_view, $_position, $_title, $_cell, @_more_paras) = @_ }
sub insert_column_with_data_func { my ($_tree_view, $_position, $_title, $_cell, $_func, $_o_data) = @_ }
sub map_expanded_rows { my ($_tree_view, $_func, $_o_data) = @_ }
sub move_column_after { my ($_tree_view, $_column, $_base_column) = @_ }
sub new { my ($_class, $_o_model) = @_ }
sub new_with_model { my ($_class, $_model) = @_ }
sub remove_column { my ($_tree_view, $_column) = @_ }
sub row_activated { my ($_tree_view, $_path, $_column) = @_ }
sub row_expanded { my ($_tree_view, $_path) = @_ }
sub scroll_to_cell { my ($_tree_view, $_path, $_o_column, $_o_use_align, $_o_row_align, $_o_col_align) = @_ }
sub scroll_to_point { my ($_tree_view, $_tree_x, $_tree_y) = @_ }
sub set_column_drag_function { my ($_tree_view, $_func, $_o_data) = @_ }
sub set_cursor { my ($_tree_view, $_path, $_o_focus_column, $_o_start_editing) = @_ }
sub set_cursor_on_cell { my ($_tree_view, $_path, $_focus_column, $_focus_cell, $_start_editing) = @_ }
sub set_destroy_count_func { my ($_tree_view, $_func, $_o_data) = @_ }
sub set_drag_dest_row { my ($_tree_view, $_path, $_pos) = @_ }
sub set_enable_search { my ($_tree_view, $_enable_search) = @_ }
sub set_expander_column { my ($_tree_view, $_column) = @_ }
sub set_hadjustment { my ($_tree_view, $_adjustment) = @_ }
sub set_headers_clickable { my ($_tree_view, $_setting) = @_ }
sub set_headers_visible { my ($_tree_view, $_headers_visible) = @_ }
sub set_model { my ($_tree_view, $_model) = @_ }
sub set_reorderable { my ($_tree_view, $_reorderable) = @_ }
sub set_rules_hint { my ($_tree_view, $_setting) = @_ }
sub set_search_column { my ($_tree_view, $_column) = @_ }
sub set_search_equal_func { my ($_tree_view, $_func, $_o_data) = @_ }
sub set_vadjustment { my ($_tree_view, $_adjustment) = @_ }
sub tree_to_widget_coords { my ($_tree_view, $_tx, $_ty) = @_ }
sub unset_rows_drag_dest { my ($_tree_view) = @_ }
sub unset_rows_drag_source { my ($_tree_view) = @_ }
sub widget_to_tree_coords { my ($_tree_view, $_wx, $_wy) = @_ }

package Gtk2::TreeViewColumn;
our @ISA = qw();
sub add_attribute { my ($_tree_column, $_cell_renderer, $_attribute, $_column) = @_ }
sub cell_get_position { my ($_tree_column, $_cell_renderer) = @_ }
sub cell_is_visible { my ($_tree_column) = @_ }
sub clear { my ($_tree_column) = @_ }
sub clear_attributes { my ($_tree_column, $_cell_renderer) = @_ }
sub clicked { my ($_tree_column) = @_ }
sub focus_cell { my ($_tree_column, $_cell) = @_ }
sub get_alignment { my ($_tree_column) = @_ }
sub get_cell_renderers { my ($_tree_column) = @_ }
sub get_clickable { my ($_tree_column) = @_ }
sub get_fixed_width { my ($_tree_column) = @_ }
sub get_max_width { my ($_tree_column) = @_ }
sub get_min_width { my ($_tree_column) = @_ }
sub get_reorderable { my ($_tree_column) = @_ }
sub get_resizable { my ($_tree_column) = @_ }
sub get_sizing { my ($_tree_column) = @_ }
sub get_sort_column_id { my ($_tree_column) = @_ }
sub get_sort_indicator { my ($_tree_column) = @_ }
sub get_sort_order { my ($_tree_column) = @_ }
sub get_spacing { my ($_tree_column) = @_ }
sub get_title { my ($_tree_column) = @_ }
sub get_visible { my ($_tree_column) = @_ }
sub get_widget { my ($_tree_column) = @_ }
sub get_width { my ($_tree_column) = @_ }
sub new { my ($_class) = @_ }
sub new_with_attributes { my ($_class, $_title, $_cell, @_more_paras) = @_ }
sub pack_end { my ($_tree_column, $_cell, $_expand) = @_ }
sub pack_start { my ($_tree_column, $_cell, $_expand) = @_ }
sub set_alignment { my ($_tree_column, $_xalign) = @_ }
sub set_attributes { my ($_tree_column, $_cell_renderer, @_more_paras) = @_ }
sub set_cell_data_func { my ($_tree_column, $_cell_renderer, $_func, $_o_data) = @_ }
sub set_clickable { my ($_tree_column, $_clickable) = @_ }
sub set_fixed_width { my ($_tree_column, $_fixed_width) = @_ }
sub set_max_width { my ($_tree_column, $_max_width) = @_ }
sub set_min_width { my ($_tree_column, $_min_width) = @_ }
sub set_reorderable { my ($_tree_column, $_reorderable) = @_ }
sub set_resizable { my ($_tree_column, $_resizable) = @_ }
sub set_sizing { my ($_tree_column, $_type) = @_ }
sub set_sort_column_id { my ($_tree_column, $_sort_column_id) = @_ }
sub set_sort_indicator { my ($_tree_column, $_setting) = @_ }
sub set_sort_order { my ($_tree_column, $_order) = @_ }
sub set_spacing { my ($_tree_column, $_spacing) = @_ }
sub set_title { my ($_tree_column, $_title) = @_ }
sub set_visible { my ($_tree_column, $_visible) = @_ }
sub set_widget { my ($_tree_column, $_widget) = @_ }

package Gtk2::VBox;
our @ISA = qw();
sub new { my ($_class, $_o_homogeneous, $_o_spacing) = @_ }

package Gtk2::VButtonBox;
our @ISA = qw();
sub get_layout_default { my ($_class) = @_ }
sub get_spacing_default { my ($_class) = @_ }
sub new { my ($_class) = @_ }
sub set_layout_default { my ($_class, $_layout) = @_ }
sub set_spacing_default { my ($_class, $_spacing) = @_ }

package Gtk2::VPaned;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::VRuler;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::VScale;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }
sub new_with_range { my ($_class, $_min, $_max, $_step) = @_ }

package Gtk2::VScrollBar;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }

package Gtk2::VScrollbar;
our @ISA = qw();
sub new { my ($_class, $_o_adjustment) = @_ }

package Gtk2::VSeparator;
our @ISA = qw();
sub new { my ($_class) = @_ }

package Gtk2::Viewport;
our @ISA = qw();
sub get_hadjustment { my ($_viewport) = @_ }
sub get_shadow_type { my ($_viewport) = @_ }
sub get_vadjustment { my ($_viewport) = @_ }
sub new { my ($_class, $_o_hadjustment, $_o_vadjustment) = @_ }
sub set_hadjustment { my ($_viewport, $_adjustment) = @_ }
sub set_shadow_type { my ($_viewport, $_type) = @_ }
sub set_vadjustment { my ($_viewport, $_adjustment) = @_ }

package Gtk2::Widget;
our @ISA = qw();
sub activate { my ($_widget) = @_ }
sub add_accelerator { my ($_widget, $_accel_signal, $_accel_group, $_accel_key, $_accel_mods, $_flags) = @_ }
sub add_events { my ($_widget, $_events) = @_ }
sub allocation { my ($_widget) = @_ }
sub app_paintable { my ($_widget, @_more_paras) = @_ }
sub can_default { my ($_widget, @_more_paras) = @_ }
sub can_focus { my ($_widget, @_more_paras) = @_ }
sub child_notify { my ($_widget, $_child_property) = @_ }
sub class_path { my ($_widget) = @_ }
sub composite_child { my ($_widget, @_more_paras) = @_ }
sub create_pango_context { my ($_widget) = @_ }
sub create_pango_layout { my ($_widget, $_text) = @_ }
sub destroy { my ($_widget) = @_ }
sub double_buffered { my ($_widget, @_more_paras) = @_ }
sub drag_check_threshold { my ($_widget, $_start_x, $_start_y, $_current_x, $_current_y) = @_ }
sub drag_dest_find_target { my ($_widget, $_context, $_target_list) = @_ }
sub drag_dest_get_target_list { my ($_widget) = @_ }
sub drag_dest_set { my ($_widget, $_flags, $_actions, @_more_paras) = @_ }
sub drag_dest_set_proxy { my ($_widget, $_proxy_window, $_protocol, $_use_coordinates) = @_ }
sub drag_dest_set_target_list { my ($_widget, $_target_list) = @_ }
sub drag_dest_unset { my ($_widget) = @_ }
sub drag_get_data { my ($_widget, $_context, $_target, $_time_) = @_ }
sub drag_highlight { my ($_widget) = @_ }
sub drag_source_set { my ($_widget, $_start_button_mask, $_actions, @_more_paras) = @_ }
sub drag_source_set_icon { my ($_widget, $_colormap, $_pixmap, $_mask) = @_ }
sub drag_source_set_icon_pixbuf { my ($_widget, $_pixbuf) = @_ }
sub drag_source_set_icon_stock { my ($_widget, $_stock_id) = @_ }
sub drag_source_unset { my ($_widget) = @_ }
sub drag_unhighlight { my ($_widget) = @_ }
sub drawable { my ($_widget, @_more_paras) = @_ }
sub ensure_style { my ($_widget) = @_ }
sub event { my ($_widget, $_event) = @_ }
sub flags { my ($_widget) = @_ }
sub freeze_child_notify { my ($_widget) = @_ }
sub get_accessible { my ($_widget) = @_ }
sub get_ancestor { my ($_widget, $_ancestor_package) = @_ }
sub get_child_requisition { my ($_widget) = @_ }
sub get_child_visible { my ($_widget) = @_ }
sub get_clipboard { my ($_widget, $_o_selection) = @_ }
sub get_colormap { my ($_widget) = @_ }
sub get_composite_name { my ($_widget) = @_ }
sub get_default_colormap { my ($_class_or_widget) = @_ }
sub get_default_direction { my ($_class) = @_ }
sub get_default_style { my ($_class_or_widget) = @_ }
sub get_default_visual { my ($_class_or_widget) = @_ }
sub get_direction { my ($_widget) = @_ }
sub get_display { my ($_widget) = @_ }
sub get_events { my ($_widget) = @_ }
sub get_extension_events { my ($_widget) = @_ }
sub get_flags { my ($_widget) = @_ }
sub get_modifier_style { my ($_widget) = @_ }
sub get_name { my ($_widget) = @_ }
sub get_pango_context { my ($_widget) = @_ }
sub get_parent { my ($_widget) = @_ }
sub get_parent_window { my ($_widget) = @_ }
sub get_pointer { my ($_widget) = @_ }
sub get_root_window { my ($_widget) = @_ }
sub get_screen { my ($_widget) = @_ }
sub get_settings { my ($_widget) = @_ }
sub get_size_request { my ($_widget) = @_ }
sub get_style { my ($_widget) = @_ }
sub get_toplevel { my ($_widget) = @_ }
sub get_visual { my ($_widget) = @_ }
sub grab_default { my ($_widget) = @_ }
sub grab_focus { my ($_widget) = @_ }
sub has_default { my ($_widget, @_more_paras) = @_ }
sub has_focus { my ($_widget, @_more_paras) = @_ }
sub has_grab { my ($_widget, @_more_paras) = @_ }
sub has_screen { my ($_widget) = @_ }
sub hide { my ($_widget) = @_ }
sub hide_all { my ($_widget) = @_ }
sub intersect { my ($_widget, $_area) = @_ }
sub is_ancestor { my ($_widget, $_ancestor) = @_ }
sub is_focus { my ($_widget) = @_ }
sub is_sensitive { my ($_widget, @_more_paras) = @_ }
sub map { my ($_widget) = @_ }
sub mapped { my ($_widget, @_more_paras) = @_ }
sub mnemonic_activate { my ($_widget, $_group_cycling) = @_ }
sub modify_base { my ($_widget, $_state, $_color) = @_ }
sub modify_bg { my ($_widget, $_state, $_color) = @_ }
sub modify_fg { my ($_widget, $_state, $_color) = @_ }
sub modify_font { my ($_widget, $_font_desc) = @_ }
sub modify_style { my ($_widget, $_style) = @_ }
sub modify_text { my ($_widget, $_state, $_color) = @_ }
sub no_window { my ($_widget, @_more_paras) = @_ }
sub parent { my ($_widget) = @_ }
sub parent_sensitive { my ($_widget, @_more_paras) = @_ }
sub path { my ($_widget) = @_ }
sub pop_colormap { my ($_class_or_widget) = @_ }
sub pop_composite_child { my ($_class_or_widget) = @_ }
sub propagate_event { my ($_widget, $_event) = @_ }
sub push_colormap { my ($_class_or_widget, $_cmap) = @_ }
sub push_composite_child { my ($_class_or_widget) = @_ }
sub queue_draw { my ($_widget) = @_ }
sub queue_draw_area { my ($_widget, $_x, $_y, $_width, $_height) = @_ }
sub queue_resize { my ($_widget) = @_ }
sub rc_style { my ($_widget, @_more_paras) = @_ }
sub realize { my ($_widget) = @_ }
sub realized { my ($_widget, @_more_paras) = @_ }
sub receives_default { my ($_widget, @_more_paras) = @_ }
sub remove_accelerator { my ($_widget, $_accel_group, $_accel_key, $_accel_mods) = @_ }
sub render_icon { my ($_widget, $_stock_id, $_size, $_o_detail) = @_ }
sub reparent { my ($_widget, $_new_parent) = @_ }
sub reset_rc_styles { my ($_widget) = @_ }
sub reset_shapes { my ($_widget) = @_ }
sub selection_add_target { my ($_widget, $_selection, $_target, $_info) = @_ }
sub selection_add_targets { my ($_widget, $_selection, @_more_paras) = @_ }
sub selection_clear_targets { my ($_widget, $_selection) = @_ }
sub selection_convert { my ($_widget, $_selection, $_target, $_time_) = @_ }
sub selection_remove_all { my ($_widget) = @_ }
sub sensitive { my ($_widget, @_more_paras) = @_ }
sub set_accel_path { my ($_widget, $_accel_path, $_accel_group) = @_ }
sub set_app_paintable { my ($_widget, $_app_paintable) = @_ }
sub set_child_visible { my ($_widget, $_is_visible) = @_ }
sub set_colormap { my ($_widget, $_colormap) = @_ }
sub set_composite_name { my ($_widget, $_name) = @_ }
sub set_default_colormap { my ($_class_or_widget, $_colormap) = @_ }
sub set_default_direction { my ($_class, $_dir) = @_ }
sub set_direction { my ($_widget, $_dir) = @_ }
sub set_double_buffered { my ($_widget, $_double_buffered) = @_ }
sub set_events { my ($_widget, $_events) = @_ }
sub set_extension_events { my ($_widget, $_mode) = @_ }
sub set_flags { my ($_widget, $_flags) = @_ }
sub set_name { my ($_widget, $_name) = @_ }
sub set_parent { my ($_widget, $_parent) = @_ }
sub set_parent_window { my ($_widget, $_parent_window) = @_ }
sub set_redraw_on_allocate { my ($_widget, $_redraw_on_allocate) = @_ }
sub set_scroll_adjustments { my ($_widget, $_hadjustment, $_vadjustment) = @_ }
sub set_sensitive { my ($_widget, $_sensitive) = @_ }
sub set_size_request { my ($_widget, $_o_width, $_o_height) = @_ }
sub set_state { my ($_widget, $_state) = @_ }
sub set_style { my ($_widget, $_style) = @_ }
sub set_uposition { my ($_widget, $_x, $_y) = @_ }
sub shape_combine_mask { my ($_widget, $_shape_mask, $_offset_x, $_offset_y) = @_ }
sub show { my ($_widget) = @_ }
sub show_all { my ($_widget) = @_ }
sub show_now { my ($_widget) = @_ }
sub size_request { my ($_widget) = @_ }
sub state { my ($_widget) = @_ }
sub style { my ($_widget) = @_ }
sub style_get { my ($_widget, $_first_property_name, @_more_paras) = @_ }
sub style_get_property { my ($_widget, $_first_property_name, @_more_paras) = @_ }
sub thaw_child_notify { my ($_widget) = @_ }
sub toplevel { my ($_widget, @_more_paras) = @_ }
sub translate_coordinates { my ($_src_widget, $_dest_widget, $_src_x, $_src_y) = @_ }
sub unmap { my ($_widget) = @_ }
sub unparent { my ($_widget) = @_ }
sub unrealize { my ($_widget) = @_ }
sub unset_flags { my ($_widget, $_flags) = @_ }
sub visible { my ($_widget, @_more_paras) = @_ }
sub window { my ($_widget) = @_ }

package Gtk2::Window;
our @ISA = qw();
sub activate_default { my ($_window) = @_ }
sub activate_focus { my ($_window) = @_ }
sub add_accel_group { my ($_window, $_accel_group) = @_ }
sub add_embedded_xid { my ($_window, $_xid) = @_ }
sub add_mnemonic { my ($_window, $_keyval, $_target) = @_ }
sub begin_move_drag { my ($_window, $_button, $_root_x, $_root_y, $_timestamp) = @_ }
sub begin_resize_drag { my ($_window, $_edge, $_button, $_root_x, $_root_y, $_timestamp) = @_ }
sub deiconify { my ($_window) = @_ }
sub fullscreen { my ($_window) = @_ }
sub get_decorated { my ($_window) = @_ }
sub get_default_icon_list { my ($_class) = @_ }
sub get_default_size { my ($_window) = @_ }
sub get_destroy_with_parent { my ($_window) = @_ }
sub get_focus { my ($_window) = @_ }
sub get_frame_dimensions { my ($_window) = @_ }
sub get_gravity { my ($_window) = @_ }
sub get_has_frame { my ($_window) = @_ }
sub get_icon { my ($_window) = @_ }
sub get_icon_list { my ($_window) = @_ }
sub get_mnemonic_modifier { my ($_window) = @_ }
sub get_modal { my ($_window) = @_ }
sub get_position { my ($_window) = @_ }
sub get_resizable { my ($_window) = @_ }
sub get_role { my ($_window) = @_ }
sub get_screen { my ($_window) = @_ }
sub get_size { my ($_window) = @_ }
sub get_skip_pager_hint { my ($_window) = @_ }
sub get_skip_taskbar_hint { my ($_window) = @_ }
sub get_title { my ($_window) = @_ }
sub get_transient_for { my ($_window) = @_ }
sub get_type_hint { my ($_window) = @_ }
sub iconify { my ($_window) = @_ }
sub list_toplevels { my ($_class) = @_ }
sub maximize { my ($_window) = @_ }
sub mnemonic_activate { my ($_window, $_keyval, $_modifier) = @_ }
sub move { my ($_window, $_x, $_y) = @_ }
sub new { my ($_class, $_o_type) = @_ }
sub parse_geometry { my ($_window, $_geometry) = @_ }
sub present { my ($_window) = @_ }
sub remove_accel_group { my ($_window, $_accel_group) = @_ }
sub remove_embedded_xid { my ($_window, $_xid) = @_ }
sub remove_mnemonic { my ($_window, $_keyval, $_target) = @_ }
sub reshow_with_initial_size { my ($_window) = @_ }
sub resize { my ($_window, $_width, $_height) = @_ }
sub set_auto_startup_notification { my ($_class, $_setting) = @_ }
sub set_decorated { my ($_window, $_setting) = @_ }
sub set_default { my ($_window, $_default_widget) = @_ }
sub set_default_icon_from_file { my ($_class_or_instance, $_filename) = @_ }
sub set_default_icon_list { my ($_class, $_pixbuf, @_more_paras) = @_ }
sub set_default_size { my ($_window, $_width, $_height) = @_ }
sub set_destroy_with_parent { my ($_window, $_setting) = @_ }
sub set_focus { my ($_window, $_o_focus) = @_ }
sub set_frame_dimensions { my ($_window, $_left, $_top, $_right, $_bottom) = @_ }
sub set_geometry_hints { my ($_window, $_geometry_widget, $_geometry_ref, $_o_geom_mask_sv) = @_ }
sub set_gravity { my ($_window, $_gravity) = @_ }
sub set_has_frame { my ($_window, $_setting) = @_ }
sub set_icon { my ($_window, $_icon) = @_ }
sub set_icon_from_file { my ($_window, $_filename) = @_ }
sub set_icon_list { my ($_window, @_more_paras) = @_ }
sub set_mnemonic_modifier { my ($_window, $_modifier) = @_ }
sub set_modal { my ($_window, $_modal) = @_ }
sub set_position { my ($_window, $_position) = @_ }
sub set_resizable { my ($_window, $_resizable) = @_ }
sub set_role { my ($_window, $_role) = @_ }
sub set_screen { my ($_window, $_screen) = @_ }
sub set_skip_pager_hint { my ($_window, $_setting) = @_ }
sub set_skip_taskbar_hint { my ($_window, $_setting) = @_ }
sub set_title { my ($_window, $_o_title) = @_ }
sub set_transient_for { my ($_window, $_parent) = @_ }
sub set_type_hint { my ($_window, $_hint) = @_ }
sub set_wmclass { my ($_window, $_wmclass_name, $_wmclass_class) = @_ }
sub stick { my ($_window) = @_ }
sub unfullscreen { my ($_window) = @_ }
sub unmaximize { my ($_window) = @_ }
sub unstick { my ($_window) = @_ }

package Gtk2::WindowGroup;
our @ISA = qw();
sub add_window { my ($_window_group, $_window) = @_ }
sub new { my ($_class) = @_ }
sub remove_window { my ($_window_group, $_window) = @_ }
