#!/usr/bin/perl

use MDK::Common;

sub gtk2 {
    my (@files) = @_;

    my @subroutines;
    my $add = sub {
        push @subroutines, [ $_[0], $_[1] ];
    };
    
    my $pm_file = sub {
        my ($file) = @_;
        my @contents = cat_($file);
        each_index {
            if (/^\s*sub\s+(\w+)/) {
                my $fun = $1;
                #- one liner constants
                #-   sub EXPOSURE_MASK { 'exposure-mask' }
                /^\s*sub\s+(\w+)\s*{\s*('[^']+')|("[^"]+")\s*}/ and $add->($fun, '() { }');
                #-   sub Sym_Hangul_J_Phieuf { 0xeed }
                /^\s*sub\s+(\w+)\s*{\s*0\S+\s*}/                and $add->($fun, '() { }');

                #- explore first line of subroutine definition
                local $_ = $contents[$::i+1];
                #- methods with no argument
                #-   my $values = shift->_get_size_request;
                if (/shift->\w+\s*;/) {
                    $add->($fun, ' { my ($_self) = @_ }');
                }
                #- methods with variable list of arguments (which branch to different XS functions)
                #-   Gtk2::_Helpers::check_usage(\@_, [ 'Gtk2::GSList group' ], [ 'Gtk2::GSList group', 'string label' ]);
                if (/Gtk2::_Helpers::check_usage\(\\\@_, (.*)\);/) {
                    my $subroutine = ' { my (';
                    my @various = split /\]\s*,/, $1;
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
                $trimlast =~ s/^G([A-Z])/$1/; #- glib case
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
    @subroutines = sort { $a->[0] cmp $b->[0] } @subroutines;
    my @ok;
    foreach my $fun (uniq(map { $_->[0] } @subroutines)) {
        my @multiples = grep { $_->[0] eq $fun } @subroutines;
        if (@multiples != 1) {
            my $args = -1;
            foreach (@multiples) {
                my $a = split /,/, $_->[1];
                $args == -1 and $args = $a;
                #- skip multiply defined functions that have different numbers of arguments
                $args != $a and goto skip_it;
            }
            my $i;
            $multiples[0][1] =~ s/\$_(\w+)/'$_DUMMY'.$i++/ge;
            push @ok, @multiples[0];
        } else {
            push @ok, @multiples;
        }
      skip_it:
    }

    print "sub Gtk2::$_->[0]$_->[1]\n" foreach @ok;
}


if ($ARGV[0] =~ /gtk2/) {
    shift @ARGV;
    gtk2(@ARGV);
}
