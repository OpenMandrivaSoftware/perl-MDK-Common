#!/usr/bin/perl

use MDK::Common;

sub gtk2 {
    my (@files) = @_;

    my @subroutines = (
      [ 'set_size_request',             ' { my ($_self, $_x, $_y) = @_ }' ],
      [ 'set_popdown_strings',          ' {}' ],
      [ 'signal_emit',                  ' {}' ],
      [ 'signal_emit_by_name',          ' {}' ],
      [ 'signal_connect',               ' { my ($_target, $_name, $_callback, $o_data) = @_ }' ],
      [ 'signal_connect_swapped',       ' { my ($_target, $_name, $_callback, $o_data) = @_ }' ],
      [ 'signal_connect_after',         ' { my ($_target, $_name, $_callback, $o_data) = @_ }' ],
      [ 'signal_handler_block',         ' { my ($_target, $_closure) = @_ }' ],
      [ 'signal_handler_unblock',       ' { my ($_target, $_closure) = @_ }' ],
      [ 'signal_disconnect',            ' { my ($_target, $_closure) = @_ }' ],
      [ 'signal_is_connected',          ' { my ($_target, $_closure) = @_ }' ],
      [ 'signal_stop_emission_by_name', ' { my ($_target, $_detailed_signal) = @_ }' ],
      [ 'timeout_add',                  ' { my ($_class, $_interval, $_func, $o_data) = @_ }' ],
      [ 'timeout_remove',               ' { my ($_class, $_id) = @_ }' ],
      [ 'idle_add',                     ' { my ($_class, $_func, $o_data) = @_ }' ],
      [ 'idle_remove',                  ' { my ($_class, $_id) = @_ }' ],
      [ 'create_items',                 ' { my ($_factory, $_entries, $o_callback_data) = @_ }' ],
      [ 'style',                        ' { my ($_widget, $o_style) = @_ }' ],
      [ 'visible',                      ' { my ($_widget, $o_bool) = @_ }' ],
      [ 'white_gc',                     ' { my ($_style, $o_gc) = @_ }' ],
      [ 'black_gc',                     ' { my ($_style, $o_gc) = @_ }' ],
      [ 'get',                          ' {}' ],
      [ 'append_item',                  ' { my ($_self, $_text, $_tooltip, $_private, $_icon, $_callback, $o_user_data) = @_ }' ],
      [ 'toggle_expansion',             ' { my ($_self, $_path, $o_open_all) = @_ }' ],
      [ 'get_path_at_pos',              ' { my ($_self, $_x, $_y) = @_ }' ],
      [ 'allow_grow',                   ' { my ($_window, $o_bool) = @_ }' ],
      [ 'allow_shrink',                 ' { my ($_window, $o_bool) = @_ }' ],
      [ 'default_height',               ' { my ($_window, $o_bool) = @_ }' ],
      [ 'default_width',                ' { my ($_window, $o_bool) = @_ }' ],
      [ 'destroy_with_parent',          ' { my ($_window, $o_bool) = @_ }' ],
      [ 'has_top_level_focus',          ' { my ($_window, $o_bool) = @_ }' ],
      [ 'is_active',                    ' { my ($_window, $o_bool) = @_ }' ],
      [ 'modal',                        ' { my ($_window, $o_bool) = @_ }' ],
      [ 'resizable',                    ' { my ($_window, $o_bool) = @_ }' ],
      [ 'window_position',              ' { my ($_window, $o_pos) = @_ }' ],
      [ 'expand_to_path',               ' { my ($_treeview, $_path) = @_ }' ],
      [ 'fraction',                     ' { my ($_progress_bar, $o_fraction) = @_ }' ],
      [ 'orientation',                  ' { my ($_progress_bar, $o_orientation) = @_ }' ],
      [ 'get_selected_rows',            ' { my ($_tree_selection) = @_ }' ],
                      );
    my @added_subroutines;
    my $add = sub {
        member($_[0], map { $_->[0] } @subroutines) and return;
        push @added_subroutines, [ $_[0], $_[1] ];
    };
    
    my $pm_file = sub {
        my ($file) = @_;
        my @contents = cat_($file);
        each_index {
            if (/^\s*sub\s+(\w+)/) {
                my $fun = $1;
                my $line = $::i;

                #- obtain first statement of function
                local $_ = $_;
                if (/^\s*sub\s+\w+\s*{?\s*$/) {
                    if ($contents[$::i+1] =~ /^\s*{\s*$/) {
                        $_ .= $contents[++$line] . $contents[++$line];
                    } else {
                        $_ .= $contents[++$line];
                    }
                }

                my $subroutine_decl = '^\s*sub\s+\w+\s*{\s*';

                #- one liner constants
                #-   sub EXPOSURE_MASK { 'exposure-mask' }
                /$subroutine_decl('[^']+')|("[^"]+")\s*}/ and $add->($fun, '() {}');
                #-   sub Sym_Hangul_J_Phieuf { 0xeed }
                /$subroutine_decl\d\S+\s*}/                and $add->($fun, '() {}');

                #- traditional form
                #-   my ($class, $interval, $func, $data) = @_;
                if (/$subroutine_decl\my\s*\(([^\)]+)\)\s*=\s*\@_\s*;\s*$/) {
                    my @args = map { /^\s*\$(.*)/ or goto skip_trad; '$_'.$1 } split /,/, $1;
                    $add->($fun, ' { my ('.join(', ', @args).') = @_ }');
                  skip_trad:
                }

                #- methods not naming arguments
                #-   sub set_name { $_[0]->set_property('name', $_[1]) }
                if (/$subroutine_decl([^}]+)\s*}\s*$/) {
                    my $statement = $1;
                    if ($statement !~ /\$[a-zA-Z]/ && $statement !~ /\@_/ && $statement =~ /.*\$_\[(\d+)\]/) {
                        $add->($fun, ' { my ('.join(', ', map { '$_DUMMY'.$_ } 0..$1).') = @_ }');
                    }
                }

                #- methods with no argument
                #-   my $values = shift->_get_size_request;
                if (/$subroutine_decl(my.*=)?\s*shift->\w+\s*((;)|(}))\s*$/) {
                    $add->($fun, ' { my ($_self) = @_ }');
                }

                #- methods with variable list of arguments (which branch to different XS functions)
                #-   Gtk2::_Helpers::check_usage(\@_, [ 'Gtk2::GSList group' ], [ 'Gtk2::GSList group', 'string label' ]);
                if (/Gtk2::_Helpers::check_usage\(\\\@_, (.*)/) {
                    my $various = $1;
                    while ($various !~ /\)\s*;\s*$/) {
                        $various .= $contents[++$line];
                    }
                    $various =~ s/\)\s*;\s*$//;

                    my $subroutine = ' { my (';
                    my @various = split /\]\s*,/, $various;
                    s/[\[\]]//g foreach @various;
                    my @mandatory = split /,/, $various[0];
                    my $proto2varname = sub { $_[0] =~ /\s*'\s*\S+\s+(.*)\s*'/; $1 };
                    $subroutine .= join(', ', map { '$_'.$proto2varname->($_) } @mandatory);
                    @mandatory and $subroutine .= ', ';
                    my @optional = split /,/, $various[-1];
                    @optional = splice @optional, @mandatory;
                    $subroutine .= join(', ', map { '$o_'.$proto2varname->($_) } @optional);
                    $add->($fun, "$subroutine) = \@_ }");
                }

            }

        } @contents;
    };

    my $c_file = sub {
        my ($file) = @_;
        my @contents = cat_($file);
        my $comment;
        each_index {
            m|/\*| and $comment = 1;
            m|\*/| and $comment = 0;
            s|/\*.*\*/||;
            s|//.*||;
            $comment and goto next_;
            /^#/ and goto next_;
            /^\s*static/ and goto next_;
            if (/^\S.*\s(\w+)\s*\((.*)/) {
                my $fun = $1;
                #- skip "internal" functions
                $fun =~ /__/ and goto next_;
                my $args = $2;
                
                #- guess function name
                $fun =~ s/^.*perl_//;
                
                my ($trimlast) = $file =~ /([A-Z]\w+)\.c$/;
                while ($trimlast =~ s/([a-z])([A-Z])/$1_$2/) {}
                $file =~ /\bGC\b/ or $trimlast =~ s/^G([A-Z])/$1/; #- glib case
                $file =~ m|Gdk/Event/src| and $trimlast = "event_$trimlast"; #- gdkevent case
                $trimlast = lc($trimlast);
                #- skip functions that will not be exported anyway because don't follow the naming scheme
                $fun =~ s/^\Q$trimlast\_// or goto next_;

                #- explore following lines if prototype was not complete
                my $line = $::i;
                while ($args !~ /\)/) {
                    $line++;
                    $args .= $contents[$line];
                }
                $args =~ s/\s+/ /g;
                $args =~ s/\).*//;

                my $proto2varname = sub { $_[0] =~ /(\w+)\s*$/; $1 };
                my @args = split /,/, $args;

                $add->($fun, ' { my (' . join(', ', map { '$_'.$proto2varname->($_) } @args) . ') = @_ }');
            }
          next_:
        } @contents;
    };

    foreach (@files) {
        /\.pm$/ and $pm_file->($_);
        /\.c$/ and $c_file->($_);
    }

    print
"package Gtk2;

our \@ISA = qw();

";
    @subroutines = sort { $a->[0] cmp $b->[0] } @subroutines, @added_subroutines;
    my @ok;
    foreach my $fun (uniq(map { $_->[0] } @subroutines)) {
        my @multiples = grep { $_->[0] eq $fun } @subroutines;
        if (@multiples != 1) {
            my $args = -1;
            foreach (@multiples) {
                my $a = split /,/, $_->[1];
                $args == -1 and $args = $a;
                #- ignore multiply defined functions that have different numbers of arguments
                $args != $a and $multiples[0][1] = ' {}';
            }
            my $i;
            $multiples[0][1] =~ s/\$_(\w+)/'$_DUMMY'.$i++/ge;
            push @ok, @multiples[0];
        } else {
            push @ok, @multiples;
        }
    }

    print "sub Gtk2::$_->[0]$_->[1]\n" foreach @ok;
}


if ($ARGV[0] =~ /gtk2/) {
    shift @ARGV;
    gtk2(@ARGV);
}
