
package Gnome2;
our @ISA = qw();
sub CHECK_VERSION { my ($_class, $_major, $_minor, $_micro) = @_ }
sub GET_VERSION_INFO { my ($_class) = @_ }
sub accelerators_sync { my ($_class) = @_ }
sub user_accels_dir_get { my ($_class) = @_ }
sub user_dir_get { my ($_class) = @_ }
sub user_private_dir_get { my ($_class) = @_ }

package Gnome2::About;
our @ISA = qw();
sub new { my ($_class, $_name, $_version, $_copyright, $_comments, $_authors, $_o_documenters, $_o_translator_credits, $_o_logo_pixbuf) = @_ }

package Gnome2::App;
our @ISA = qw();
sub accel_group { my ($_app) = @_ }
sub add_dock_item { my ($_app, $_item, $_placement, $_band_num, $_band_position, $_offset) = @_ }
sub add_docked { my ($_app, $_widget, $_name, $_behavior, $_placement, $_band_num, $_band_position, $_offset) = @_ }
sub add_toolbar { my ($_app, $_toolbar, $_name, $_behavior, $_placement, $_band_num, $_band_position, $_offset) = @_ }
sub contents { my ($_app) = @_ }
sub create_menus { my ($_app, $_uiinfo) = @_ }
sub create_toolbar { my ($_app, $_uiinfo) = @_ }
sub dock { my ($_app) = @_ }
sub enable_layout_config { my ($_app, $_enable) = @_ }
sub get_dock { my ($_app) = @_ }
sub get_dock_item_by_name { my ($_app, $_name) = @_ }
sub get_enable_layout_config { my ($_app) = @_ }
sub insert_menus { my ($_app, $_path, $_menuinfo) = @_ }
sub install_menu_hints { my ($_app, $_uiinfo) = @_ }
sub layout { my ($_app) = @_ }
sub menubar { my ($_app) = @_ }
sub new { my ($_class, $_appname, $_o_title) = @_ }
sub prefix { my ($_app) = @_ }
sub remove_menu_range { my ($_app, $_path, $_start, $_items) = @_ }
sub remove_menus { my ($_app, $_path, $_items) = @_ }
sub set_contents { my ($_app, $_contents) = @_ }
sub set_menus { my ($_app, $_menubar) = @_ }
sub set_statusbar { my ($_app, $_statusbar) = @_ }
sub set_statusbar_custom { my ($_app, $_container, $_statusbar) = @_ }
sub set_toolbar { my ($_app, $_toolbar) = @_ }
sub setup_toolbar { my ($_class, $_toolbar, $_dock_item) = @_ }
sub statusbar { my ($_app) = @_ }
sub vbox { my ($_app) = @_ }

package Gnome2::AppBar;
our @ISA = qw();
sub clear_prompt { my ($_appbar) = @_ }
sub clear_stack { my ($_appbar) = @_ }
sub get_progress { my ($_appbar) = @_ }
sub get_response { my ($_appbar) = @_ }
sub get_status { my ($_appbar) = @_ }
sub install_menu_hints { my ($_appbar, $_uiinfo) = @_ }
sub new { my ($_class, $_has_progress, $_has_status, $_interactivity) = @_ }
sub pop { my ($_appbar) = @_ }
sub push { my ($_appbar, $_status) = @_ }
sub refresh { my ($_appbar) = @_ }
sub set_default { my ($_appbar, $_default_status) = @_ }
sub set_progress_percentage { my ($_appbar, $_percentage) = @_ }
sub set_prompt { my ($_appbar, $_prompt, $_modal) = @_ }
sub set_status { my ($_appbar, $_status) = @_ }

package Gnome2::AuthenticationManager;
our @ISA = qw();
sub init { my ($_class) = @_ }

package Gnome2::Bonobo;
our @ISA = qw();
sub CHECK_VERSION { my ($_class, $_major, $_minor, $_micro) = @_ }
sub GET_VERSION_INFO { my ($_class) = @_ }

package Gnome2::Bonobo::Dock;
our @ISA = qw();
sub add_floating_item { my ($_dock, $_widget, $_x, $_y, $_orientation) = @_ }
sub add_from_layout { my ($_dock, $_layout) = @_ }
sub add_item { my ($_dock, $_item, $_placement, $_band_num, $_position, $_offset, $_in_new_band) = @_ }
sub allow_floating_items { my ($_dock, $_enable) = @_ }
sub get_client_area { my ($_dock) = @_ }
sub get_item_by_name { my ($_dock, $_name) = @_ }
sub get_layout { my ($_dock) = @_ }
sub new { my ($_class) = @_ }
sub set_client_area { my ($_dock, $_widget) = @_ }

package Gnome2::Bonobo::DockItem;
our @ISA = qw();
sub get_behavior { my ($_dock_item) = @_ }
sub get_child { my ($_dock_item) = @_ }
sub get_name { my ($_dock_item) = @_ }
sub get_orientation { my ($_dock_item) = @_ }
sub get_shadow_type { my ($_dock_item) = @_ }
sub new { my ($_class, $_name, $_behavior) = @_ }
sub set_orientation { my ($_dock_item, $_orientation) = @_ }
sub set_shadow_type { my ($_dock_item, $_type) = @_ }

package Gnome2::Client;
our @ISA = qw();
sub add_static_arg { my ($_client, @_more_paras) = @_ }
sub connect { my ($_client) = @_ }
sub connected { my ($_client) = @_ }
sub disconnect { my ($_client) = @_ }
sub flush { my ($_client) = @_ }
sub get_config_prefix { my ($_client) = @_ }
sub get_desktop_id { my ($_client) = @_ }
sub get_flags { my ($_client) = @_ }
sub get_global_config_prefix { my ($_client) = @_ }
sub get_id { my ($_client) = @_ }
sub get_previous_id { my ($_client) = @_ }
sub interaction_key_return { my ($_class, $_key, $_cancel_shutdown) = @_ }
sub master { my ($_class) = @_ }
sub new { my ($_class) = @_ }
sub new_without_connection { my ($_class) = @_ }
sub request_interaction { my ($_client, $_dialog_type, $_function, $_o_data) = @_ }
sub request_phase_2 { my ($_client) = @_ }
sub request_save { my ($_client, $_save_style, $_shutdown, $_interact_style, $_fast, $_global) = @_ }
sub save_any_dialog { my ($_client, $_dialog) = @_ }
sub save_error_dialog { my ($_client, $_dialog) = @_ }
sub set_clone_command { my ($_client, @_more_paras) = @_ }
sub set_current_directory { my ($_client, $_dir) = @_ }
sub set_discard_command { my ($_client, @_more_paras) = @_ }
sub set_environment { my ($_client, $_name, $_value) = @_ }
sub set_global_config_prefix { my ($_client, $_prefix) = @_ }
sub set_priority { my ($_client, $_priority) = @_ }
sub set_resign_command { my ($_client, @_more_paras) = @_ }
sub set_restart_command { my ($_client, @_more_paras) = @_ }
sub set_restart_style { my ($_client, $_style) = @_ }
sub set_shutdown_command { my ($_client, @_more_paras) = @_ }

package Gnome2::ColorPicker;
our @ISA = qw();
sub get_d { my ($_cp) = @_ }
sub get_dither { my ($_cp) = @_ }
sub get_i16 { my ($_cp) = @_ }
sub get_i8 { my ($_cp) = @_ }
sub get_title { my ($_cp) = @_ }
sub get_use_alpha { my ($_cp) = @_ }
sub new { my ($_class) = @_ }
sub set_d { my ($_cp, $_r, $_g, $_b, $_a) = @_ }
sub set_dither { my ($_cp, $_dither) = @_ }
sub set_i16 { my ($_cp, $_r, $_g, $_b, $_a) = @_ }
sub set_i8 { my ($_cp, $_r, $_g, $_b, $_a) = @_ }
sub set_title { my ($_cp, $_title) = @_ }
sub set_use_alpha { my ($_cp, $_use_alpha) = @_ }

package Gnome2::Config;
our @ISA = qw();
sub clean_file { my ($_class, $_path) = @_ }
sub clean_key { my ($_class, $_path) = @_ }
sub clean_section { my ($_class, $_path) = @_ }
sub drop_all { my ($_class) = @_ }
sub drop_file { my ($_class, $_path) = @_ }
sub get_bool { my ($_class, $_path) = @_ }
sub get_bool_with_default { my ($_class, $_path) = @_ }
sub get_float { my ($_class, $_path) = @_ }
sub get_float_with_default { my ($_class, $_path) = @_ }
sub get_int { my ($_class, $_path) = @_ }
sub get_int_with_default { my ($_class, $_path) = @_ }
sub get_real_path { my ($_class, $_path) = @_ }
sub get_string { my ($_class, $_path) = @_ }
sub get_string_with_default { my ($_class, $_path) = @_ }
sub get_translated_string { my ($_class, $_path) = @_ }
sub get_translated_string_with_default { my ($_class, $_path) = @_ }
sub get_vector { my ($_class, $_path) = @_ }
sub get_vector_with_default { my ($_class, $_path) = @_ }
sub has_section { my ($_class, $_path) = @_ }
sub init_iterator { my ($_class, $_path) = @_ }
sub init_iterator_sections { my ($_class, $_path) = @_ }
sub pop_prefix { my ($_class) = @_ }
sub push_prefix { my ($_class, $_path) = @_ }
sub set_bool { my ($_class, $_path, $_value) = @_ }
sub set_float { my ($_class, $_path, $_value) = @_ }
sub set_int { my ($_class, $_path, $_value) = @_ }
sub set_string { my ($_class, $_path, $_value) = @_ }
sub set_translated_string { my ($_class, $_path, $_value) = @_ }
sub set_vector { my ($_class, $_path, $_value) = @_ }
sub sync { my ($_class) = @_ }
sub sync_file { my ($_class, $_path) = @_ }

package Gnome2::Config::Iterator;
our @ISA = qw();
sub DESTROY { my ($_handle) = @_ }
sub next { my ($_handle) = @_ }

package Gnome2::Config::Private;
our @ISA = qw();
sub clean_file { my ($_class, $_path) = @_ }
sub clean_key { my ($_class, $_path) = @_ }
sub clean_section { my ($_class, $_path) = @_ }
sub drop_file { my ($_class, $_path) = @_ }
sub get_bool { my ($_class, $_path) = @_ }
sub get_bool_with_default { my ($_class, $_path) = @_ }
sub get_float { my ($_class, $_path) = @_ }
sub get_float_with_default { my ($_class, $_path) = @_ }
sub get_int { my ($_class, $_path) = @_ }
sub get_int_with_default { my ($_class, $_path) = @_ }
sub get_real_path { my ($_class, $_path) = @_ }
sub get_string { my ($_class, $_path) = @_ }
sub get_string_with_default { my ($_class, $_path) = @_ }
sub get_translated_string { my ($_class, $_path) = @_ }
sub get_translated_string_with_default { my ($_class, $_path) = @_ }
sub get_vector { my ($_class, $_path) = @_ }
sub get_vector_with_default { my ($_class, $_path) = @_ }
sub has_section { my ($_class, $_path) = @_ }
sub init_iterator { my ($_class, $_path) = @_ }
sub init_iterator_sections { my ($_class, $_path) = @_ }
sub set_bool { my ($_class, $_path, $_value) = @_ }
sub set_float { my ($_class, $_path, $_value) = @_ }
sub set_int { my ($_class, $_path, $_value) = @_ }
sub set_string { my ($_class, $_path, $_value) = @_ }
sub set_translated_string { my ($_class, $_path, $_value) = @_ }
sub set_vector { my ($_class, $_path, $_value) = @_ }
sub sync_file { my ($_class, $_path) = @_ }

package Gnome2::DateEdit;
our @ISA = qw();
sub get_flags { my ($_gde) = @_ }
sub get_initial_time { my ($_gde) = @_ }
sub get_time { my ($_gde) = @_ }
sub new { my ($_class, $_the_time, $_show_time, $_use_24_format) = @_ }
sub new_flags { my ($_class, $_the_time, $_flags) = @_ }
sub set_flags { my ($_gde, $_flags) = @_ }
sub set_popup_range { my ($_gde, $_low_hour, $_up_hour) = @_ }
sub set_time { my ($_gde, $_the_time) = @_ }

package Gnome2::Druid;
our @ISA = qw();
sub append_page { my ($_druid, $_page) = @_ }
sub back { my ($_druid) = @_ }
sub cancel { my ($_druid) = @_ }
sub finish { my ($_druid) = @_ }
sub help { my ($_druid) = @_ }
sub insert_page { my ($_druid, $_back_page, $_page) = @_ }
sub new { my ($_class) = @_ }
sub new_with_window { my ($_class, $_title, $_parent, $_close_on_cancel) = @_ }
sub next { my ($_druid) = @_ }
sub prepend_page { my ($_druid, $_page) = @_ }
sub set_buttons_sensitive { my ($_druid, $_back_sensitive, $_next_sensitive, $_cancel_sensitive, $_help_sensitive) = @_ }
sub set_page { my ($_druid, $_page) = @_ }
sub set_show_finish { my ($_druid, $_show_finish) = @_ }
sub set_show_help { my ($_druid, $_show_help) = @_ }

package Gnome2::DruidPage;
our @ISA = qw();
sub back { my ($_druid_page) = @_ }
sub cancel { my ($_druid_page) = @_ }
sub finish { my ($_druid_page) = @_ }
sub new { my ($_class) = @_ }
sub next { my ($_druid_page) = @_ }
sub prepare { my ($_druid_page) = @_ }

package Gnome2::DruidPageEdge;
our @ISA = qw();
sub new { my ($_class, $_position) = @_ }
sub new_aa { my ($_class, $_position) = @_ }
sub new_with_vals { my ($_class, $_position, $_antialiased, $_o_title, $_o_text, $_o_logo, $_o_watermark, $_o_top_watermark) = @_ }
sub set_bg_color { my ($_druid_page_edge, $_color) = @_ }
sub set_logo { my ($_druid_page_edge, $_logo_image) = @_ }
sub set_logo_bg_color { my ($_druid_page_edge, $_color) = @_ }
sub set_text { my ($_druid_page_edge, $_text) = @_ }
sub set_text_color { my ($_druid_page_edge, $_color) = @_ }
sub set_textbox_color { my ($_druid_page_edge, $_color) = @_ }
sub set_title { my ($_druid_page_edge, $_title) = @_ }
sub set_title_color { my ($_druid_page_edge, $_color) = @_ }
sub set_top_watermark { my ($_druid_page_edge, $_top_watermark_image) = @_ }
sub set_watermark { my ($_druid_page_edge, $_watermark) = @_ }

package Gnome2::DruidPageStandard;
our @ISA = qw();
sub append_item { my ($_druid_page_standard, $_question, $_item, $_additional_info) = @_ }
sub new { my ($_class) = @_ }
sub new_with_vals { my ($_class, $_title, $_o_logo, $_o_top_watermark) = @_ }
sub set_background { my ($_druid_page_standard, $_color) = @_ }
sub set_contents_background { my ($_druid_page_standard, $_color) = @_ }
sub set_logo { my ($_druid_page_standard, $_logo_image) = @_ }
sub set_logo_background { my ($_druid_page_standard, $_color) = @_ }
sub set_title { my ($_druid_page_standard, $_title) = @_ }
sub set_title_foreground { my ($_druid_page_standard, $_color) = @_ }
sub set_top_watermark { my ($_druid_page_standard, $_top_watermark_image) = @_ }
sub vbox { my ($_druid_page_standard) = @_ }

package Gnome2::Entry;
our @ISA = qw();
sub append_history { my ($_gentry, $_save, $_text) = @_ }
sub clear_history { my ($_gentry) = @_ }
sub get_history_id { my ($_gentry) = @_ }
sub get_max_saved { my ($_gentry) = @_ }
sub gtk_entry { my ($_gentry) = @_ }
sub new { my ($_class, $_o_history_id) = @_ }
sub prepend_history { my ($_gentry, $_save, $_text) = @_ }
sub set_history_id { my ($_gentry, $_history_id) = @_ }
sub set_max_saved { my ($_gentry, $_max_saved) = @_ }

package Gnome2::FileEntry;
our @ISA = qw();
sub get_directory_entry { my ($_fentry) = @_ }
sub get_full_path { my ($_fentry, $_file_must_exist) = @_ }
sub get_modal { my ($_fentry) = @_ }
sub gnome_entry { my ($_fentry) = @_ }
sub gtk_entry { my ($_fentry) = @_ }
sub new { my ($_class, $_history_id, $_browse_dialog_title) = @_ }
sub set_default_path { my ($_fentry, $_path) = @_ }
sub set_directory_entry { my ($_fentry, $_directory_entry) = @_ }
sub set_filename { my ($_fentry, $_filename) = @_ }
sub set_modal { my ($_fentry, $_is_modal) = @_ }
sub set_title { my ($_fentry, $_browse_dialog_title) = @_ }

package Gnome2::FontPicker;
our @ISA = qw();
sub fi_set_show_size { my ($_gfp, $_show_size) = @_ }
sub fi_set_use_font_in_label { my ($_gfp, $_use_font_in_label, $_size) = @_ }
sub get_font_name { my ($_gfp) = @_ }
sub get_mode { my ($_gfp) = @_ }
sub get_preview_text { my ($_gfp) = @_ }
sub get_title { my ($_gfp) = @_ }
sub new { my ($_class) = @_ }
sub set_font_name { my ($_gfp, $_fontname) = @_ }
sub set_mode { my ($_gfp, $_mode) = @_ }
sub set_preview_text { my ($_gfp, $_text) = @_ }
sub set_title { my ($_gfp, $_title) = @_ }
sub uw_get_widget { my ($_gfp) = @_ }
sub uw_set_widget { my ($_gfp, $_widget) = @_ }

package Gnome2::GConf;
our @ISA = qw();
sub get_app_settings_relative { my ($_class, $_program, $_subkey) = @_ }
sub get_gnome_libs_settings_relative { my ($_class, $_subkey) = @_ }

package Gnome2::HRef;
our @ISA = qw();
sub get_label { my ($_href) = @_ }
sub get_text { my ($_href) = @_ }
sub get_url { my ($_href) = @_ }
sub new { my ($_class, $_url, $_text) = @_ }
sub set_label { my ($_href, $_label) = @_ }
sub set_text { my ($_href, $_text) = @_ }
sub set_url { my ($_href, $_url) = @_ }

package Gnome2::Help;
our @ISA = qw();
sub display { my ($_class, $_file_name, $_o_link_id) = @_ }
sub display_desktop { my ($_class, $_program, $_doc_id, $_file_name, $_o_link_id) = @_ }
sub display_desktop_with_env { my ($_class, $_program, $_doc_id, $_file_name, $_link_id, $_env_ref) = @_ }

package Gnome2::I18N;
our @ISA = qw();
sub get_language_list { my ($_class, $_o_category_name) = @_ }
sub pop_c_numeric_locale { my ($_class) = @_ }
sub push_c_numeric_locale { my ($_class) = @_ }

package Gnome2::IconEntry;
our @ISA = qw();
sub get_filename { my ($_ientry) = @_ }
sub new { my ($_class, $_history_id, $_browse_dialog_title) = @_ }
sub pick_dialog { my ($_ientry) = @_ }
sub set_browse_dialog_title { my ($_ientry, $_browse_dialog_title) = @_ }
sub set_filename { my ($_ientry, $_filename) = @_ }
sub set_history_id { my ($_ientry, $_history_id) = @_ }
sub set_max_saved { my ($_ientry, $_max_saved) = @_ }
sub set_pixmap_subdir { my ($_ientry, $_subdir) = @_ }

package Gnome2::IconList;
our @ISA = qw();
sub append { my ($_gil, $_icon_filename, $_text) = @_ }
sub append_pixbuf { my ($_gil, $_im, $_icon_filename, $_text) = @_ }
sub clear { my ($_gil) = @_ }
sub find_icon_from_filename { my ($_gil, $_filename) = @_ }
sub focus_icon { my ($_gil, $_idx) = @_ }
sub freeze { my ($_gil) = @_ }
sub get_icon_at { my ($_gil, $_x, $_y) = @_ }
sub get_icon_filename { my ($_gil, $_idx) = @_ }
sub get_icon_pixbuf_item { my ($_gil, $_idx) = @_ }
sub get_icon_text_item { my ($_gil, $_idx) = @_ }
sub get_items_per_line { my ($_gil) = @_ }
sub get_num_icons { my ($_gil) = @_ }
sub get_selection { my ($_gil) = @_ }
sub get_selection_mode { my ($_gil) = @_ }
sub icon_is_visible { my ($_gil, $_pos) = @_ }
sub insert { my ($_gil, $_pos, $_icon_filename, $_text) = @_ }
sub insert_pixbuf { my ($_gil, $_pos, $_im, $_icon_filename, $_text) = @_ }
sub moveto { my ($_gil, $_pos, $_yalign) = @_ }
sub new { my ($_class, $_icon_width, $_adj, $_flags) = @_ }
sub remove { my ($_gil, $_pos) = @_ }
sub select_icon { my ($_gil, $_pos) = @_ }
sub set_col_spacing { my ($_gil, $_pixels) = @_ }
sub set_hadjustment { my ($_gil, $_hadj) = @_ }
sub set_icon_border { my ($_gil, $_pixels) = @_ }
sub set_icon_width { my ($_gil, $_w) = @_ }
sub set_row_spacing { my ($_gil, $_pixels) = @_ }
sub set_selection_mode { my ($_gil, $_mode) = @_ }
sub set_separators { my ($_gil, $_sep) = @_ }
sub set_text_spacing { my ($_gil, $_pixels) = @_ }
sub set_vadjustment { my ($_gil, $_vadj) = @_ }
sub thaw { my ($_gil) = @_ }
sub unselect_all { my ($_gil) = @_ }
sub unselect_icon { my ($_gil, $_pos) = @_ }

package Gnome2::IconSelection;
our @ISA = qw();
sub add_defaults { my ($_gis) = @_ }
sub add_directory { my ($_gis, $_dir) = @_ }
sub clear { my ($_gis, $_not_shown) = @_ }
sub get_box { my ($_gis) = @_ }
sub get_gil { my ($_gis) = @_ }
sub get_icon { my ($_gis, $_full_path) = @_ }
sub new { my ($_class) = @_ }
sub select_icon { my ($_gis, $_filename) = @_ }
sub show_icons { my ($_gis) = @_ }
sub stop_loading { my ($_gis) = @_ }

package Gnome2::IconTextItem;
our @ISA = qw();
sub configure { my ($_iti, $_x, $_y, $_width, $_fontname, $_text, $_is_editable, $_is_static) = @_ }
sub focus { my ($_iti, $_focused) = @_ }
sub get_editable { my ($_iti) = @_ }
sub get_text { my ($_iti) = @_ }
sub select { my ($_iti, $_sel) = @_ }
sub setxy { my ($_iti, $_x, $_y) = @_ }
sub start_editing { my ($_iti) = @_ }
sub stop_editing { my ($_iti, $_accept) = @_ }

package Gnome2::IconTheme;
our @ISA = qw();
sub append_search_path { my ($_theme, $_path) = @_ }
sub get_allow_svg { my ($_theme) = @_ }
sub get_example_icon_name { my ($_theme) = @_ }
sub get_search_path { my ($_theme) = @_ }
sub has_icon { my ($_theme, $_icon_name) = @_ }
sub list_icons { my ($_theme, $_o_context) = @_ }
sub lookup { my ($_icon_theme, $_thumbnail_factory, $_file_uri, $_custom_icon, $_file_info, $_mime_type, $_flags) = @_ }
sub lookup_icon { my ($_theme, $_icon_name, $_size) = @_ }
sub lookup_sync { my ($_icon_theme, $_thumbnail_factory, $_file_uri, $_custom_icon, $_flags) = @_ }
sub new { my ($_class) = @_ }
sub prepend_search_path { my ($_theme, $_path) = @_ }
sub rescan_if_needed { my ($_theme) = @_ }
sub set_allow_svg { my ($_theme, $_allow_svg) = @_ }
sub set_custom_theme { my ($_theme, $_theme_name) = @_ }
sub set_search_path { my ($_theme, @_more_paras) = @_ }

package Gnome2::ModuleInfo;
our @ISA = qw();
sub bonobo { my ($_class) = @_ }
sub description { my ($_module_info) = @_ }
sub libgnome { my ($_class) = @_ }
sub libgnomeui { my ($_class) = @_ }
sub name { my ($_module_info) = @_ }
sub opt_prefix { my ($_module_info) = @_ }
sub version { my ($_module_info) = @_ }

package Gnome2::PasswordDialog;
our @ISA = qw();
sub get_domain { my ($_password_dialog) = @_ }
sub get_password { my ($_password_dialog) = @_ }
sub get_remember { my ($_password_dialog) = @_ }
sub get_username { my ($_password_dialog) = @_ }
sub new { my ($_class, $_dialog_title, $_message, $_username, $_password, $_readonly_username) = @_ }
sub run_and_block { my ($_password_dialog) = @_ }
sub set_domain { my ($_password_dialog, $_domain) = @_ }
sub set_password { my ($_password_dialog, $_password) = @_ }
sub set_readonly_domain { my ($_password_dialog, $_readonly) = @_ }
sub set_readonly_username { my ($_password_dialog, $_readonly) = @_ }
sub set_remember { my ($_password_dialog, $_remember) = @_ }
sub set_show_domain { my ($_password_dialog, $_show) = @_ }
sub set_show_password { my ($_password_dialog, $_show) = @_ }
sub set_show_remember { my ($_password_dialog, $_show_remember) = @_ }
sub set_show_username { my ($_password_dialog, $_show) = @_ }
sub set_username { my ($_password_dialog, $_username) = @_ }

package Gnome2::PixmapEntry;
our @ISA = qw();
sub get_filename { my ($_pentry) = @_ }
sub new { my ($_class, $_history_id, $_browse_dialog_title, $_do_preview) = @_ }
sub preview_widget { my ($_pentry) = @_ }
sub scrolled_window { my ($_pentry) = @_ }
sub set_pixmap_subdir { my ($_pentry, $_subdir) = @_ }
sub set_preview { my ($_pentry, $_do_preview) = @_ }
sub set_preview_size { my ($_pentry, $_preview_w, $_preview_h) = @_ }

package Gnome2::PopupMenu;
our @ISA = qw();
sub new { my ($_class, $_uiinfo, $_o_accelgroup) = @_ }
sub new_with_accelgroup { my ($_class, $_uiinfo, $_o_accelgroup) = @_ }

package Gnome2::Program;
our @ISA = qw();
sub get_app_id { my ($_program) = @_ }
sub get_app_version { my ($_program) = @_ }
sub get_human_readable_name { my ($_program) = @_ }
sub get_program { my ($_class) = @_ }
sub init { my ($_class, $_app_id, $_app_version, $_o_module_info, @_more_paras) = @_ }
sub locate_file { my ($_program, $_domain, $_file_name, $_only_if_exists) = @_ }
sub module_load { my ($_class, $_mod_name) = @_ }
sub module_register { my ($_class, $_module_info) = @_ }
sub module_registered { my ($_class, $_module_info) = @_ }

package Gnome2::Score;
our @ISA = qw();
sub get_notable { my ($_class, $_gamename, $_level) = @_ }
sub init { my ($_class, $_gamename) = @_ }
sub log { my ($_class, $_score, $_level, $_higher_to_lower_score_order) = @_ }

package Gnome2::Scores;
our @ISA = qw();
sub display { my ($_class, $_title, $_app_name, $_level, $_pos) = @_ }
sub display_with_pixmap { my ($_class, $_pixmap_logo, $_app_name, $_level, $_pos) = @_ }
sub new { my ($_class, $_names, $_scores, $_times, $_clear) = @_ }
sub set_color { my ($_gs, $_n, $_col) = @_ }
sub set_colors { my ($_gs, $_col) = @_ }
sub set_current_player { my ($_gs, $_i) = @_ }
sub set_def_color { my ($_gs, $_col) = @_ }
sub set_logo_label { my ($_gs, $_txt, $_font, $_col) = @_ }
sub set_logo_label_title { my ($_gs, $_txt) = @_ }
sub set_logo_pixmap { my ($_gs, $_pix_name) = @_ }
sub set_logo_widget { my ($_gs, $_w) = @_ }

package Gnome2::Sound;
our @ISA = qw();
sub connection_get { my ($_class) = @_ }
sub init { my ($_class, $_o_hostname) = @_ }
sub play { my ($_class, $_filename) = @_ }
sub sample_load { my ($_class, $_sample_name, $_filename) = @_ }
sub shutdown { my ($_class) = @_ }

package Gnome2::ThumbnailFactory;
our @ISA = qw();
sub can_thumbnail { my ($_factory, $_uri, $_mime_type, $_mtime) = @_ }
sub create_failed_thumbnail { my ($_factory, $_uri, $_mtime) = @_ }
sub generate_thumbnail { my ($_factory, $_uri, $_mime_type) = @_ }
sub has_valid_failed_thumbnail { my ($_factory, $_uri, $_mtime) = @_ }
sub lookup { my ($_factory, $_uri, $_mtime) = @_ }
sub new { my ($_class, $_size) = @_ }
sub save_thumbnail { my ($_factory, $_thumbnail, $_uri, $_original_mtime) = @_ }

package Gnome2::UIDefs;
our @ISA = qw();
sub key_mod_clear { my ($_class) = @_ }
sub key_mod_close { my ($_class) = @_ }
sub key_mod_close_window { my ($_class) = @_ }
sub key_mod_copy { my ($_class) = @_ }
sub key_mod_cut { my ($_class) = @_ }
sub key_mod_find { my ($_class) = @_ }
sub key_mod_find_again { my ($_class) = @_ }
sub key_mod_new { my ($_class) = @_ }
sub key_mod_new_game { my ($_class) = @_ }
sub key_mod_new_window { my ($_class) = @_ }
sub key_mod_open { my ($_class) = @_ }
sub key_mod_paste { my ($_class) = @_ }
sub key_mod_pause_game { my ($_class) = @_ }
sub key_mod_print { my ($_class) = @_ }
sub key_mod_print_setup { my ($_class) = @_ }
sub key_mod_quit { my ($_class) = @_ }
sub key_mod_redo { my ($_class) = @_ }
sub key_mod_redo_move { my ($_class) = @_ }
sub key_mod_replace { my ($_class) = @_ }
sub key_mod_save { my ($_class) = @_ }
sub key_mod_save_as { my ($_class) = @_ }
sub key_mod_select_all { my ($_class) = @_ }
sub key_mod_undo { my ($_class) = @_ }
sub key_mod_undo_move { my ($_class) = @_ }
sub key_name_clear { my ($_class) = @_ }
sub key_name_close { my ($_class) = @_ }
sub key_name_close_window { my ($_class) = @_ }
sub key_name_copy { my ($_class) = @_ }
sub key_name_cut { my ($_class) = @_ }
sub key_name_find { my ($_class) = @_ }
sub key_name_find_again { my ($_class) = @_ }
sub key_name_new { my ($_class) = @_ }
sub key_name_new_game { my ($_class) = @_ }
sub key_name_new_window { my ($_class) = @_ }
sub key_name_open { my ($_class) = @_ }
sub key_name_paste { my ($_class) = @_ }
sub key_name_pause_game { my ($_class) = @_ }
sub key_name_print { my ($_class) = @_ }
sub key_name_print_setup { my ($_class) = @_ }
sub key_name_quit { my ($_class) = @_ }
sub key_name_redo { my ($_class) = @_ }
sub key_name_redo_move { my ($_class) = @_ }
sub key_name_replace { my ($_class) = @_ }
sub key_name_save { my ($_class) = @_ }
sub key_name_save_as { my ($_class) = @_ }
sub key_name_select_all { my ($_class) = @_ }
sub key_name_undo { my ($_class) = @_ }
sub key_name_undo_move { my ($_class) = @_ }
sub pad { my ($_class) = @_ }
sub pad_big { my ($_class) = @_ }
sub pad_small { my ($_class) = @_ }

package Gnome2::URL;
our @ISA = qw();
sub show { my ($_class, $_url) = @_ }
sub show_with_env { my ($_class, $_url, $_env_ref) = @_ }

package Gnome2::Util;
our @ISA = qw();
sub extension { my ($_class, $_path) = @_ }
sub home_file { my ($_class, $_file) = @_ }
sub prepend_user_home { my ($_class, $_file) = @_ }
sub user_shell { my ($_class) = @_ }

package Gnome2::WindowIcon;
our @ISA = qw();
sub init { my ($_class) = @_ }
sub set_default_from_file { my ($_class, $_filename) = @_ }
sub set_default_from_file_list { my ($_class, $_filenames_ref) = @_ }
sub set_from_default { my ($_class, $_w) = @_ }
sub set_from_file { my ($_class, $_w, $_filename) = @_ }
sub set_from_file_list { my ($_class, $_w, $_filenames_ref) = @_ }

package Gtk2::Gdk::Pixbuf;
our @ISA = qw();
sub has_uri { my ($_pixbuf, $_uri) = @_ }
sub is_valid { my ($_pixbuf, $_uri, $_mtime) = @_ }
sub md5 { my ($_class, $_uri) = @_ }
sub path_for_uri { my ($_class, $_uri, $_size) = @_ }
sub scale_down_pixbuf { my ($_pixbuf, $_dest_width, $_dest_height) = @_ }

package Gtk2::Menu;
our @ISA = qw();
sub append_from { my ($_popup, $_uiinfo) = @_ }
sub attach_to { my ($_popup, $_widget, $_o_user_data) = @_ }
sub do_popup { my ($_popup, $_pos_func, $_pos_data, $_event, $_user_data, $_for_widget) = @_ }
sub do_popup_modal { my ($_popup, $_pos_func, $_pos_data, $_event, $_user_data, $_for_widget) = @_ }

package Gtk2::MenuShell;
our @ISA = qw();
sub fill_menu { my ($_menu_shell, $_uiinfo, $_accel_group, $_uline_accels, $_pos) = @_ }
sub find_menu_pos { my ($_parent, $_path) = @_ }

package Gtk2::Statusbar;
our @ISA = qw();
sub install_menu_hints { my ($_bar, $_uiinfo) = @_ }

package Gtk2::Toolbar;
our @ISA = qw();
sub fill_toolbar { my ($_toolbar, $_uiinfo, $_accel_group) = @_ }

package Gtk2::Widget;
our @ISA = qw();
sub add_popup_items { my ($_widget, $_uiinfo, $_o_user_data) = @_ }

package Gtk2::Window;
our @ISA = qw();
sub toplevel_set_title { my ($_window, $_doc_name, $_app_name, $_extension) = @_ }
