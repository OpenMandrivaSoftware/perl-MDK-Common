=head1 NAME

MDK::Common::Various - miscellaneous functions

=head1 SYNOPSIS

    use MDK::Common::Various qw(:all);

=head1 EXPORTS

=over 

=item first(LIST)

returns the first value. C<first(XXX)> is an alternative for C<((XXX)[0])>

=item second(LIST)

returns the second value. C<second(XXX)> is an alternative for C<((XXX)[1])>

=item top(LIST)

returns the last value. C<top(@l)> is an alternative for C<$l[$#l]>

=item to_bool(SCALAR)

returns a value in { 0, 1 }

=item to_int(STRING)

extracts the number from the string. You could use directly C<int "11 foo">, but
you'll get I<Argument "11 foo" isn't numeric in int>. It also handles returns
11 for C<"foo 11 bar">

=item to_float(STRING)

extract a decimal number from the string

=item bool2text(SCALAR)

returns a value in { "true", "false" }

=item bool2yesno(SCALAR)

returns a value in { "yes", "no" }

=item text2bool(STRING)

inverse of C<bool2text> and C<bool2yesno>

=item chomp_(STRING)

non-mutable version of chomp: do not modify the argument, returns the chomp'ed
value. Also works on lists: C<chomp_($a, $b)> is equivalent to 
C<chomp($a) ; chomp($b) ; ($a,$b)>

=item backtrace()

returns a string describing the backtrace. eg: 

    sub g { print "oops\n", backtrace() }
    sub f { &g }
    f();

gives

    oops
    main::g() called from /tmp/t.pl:2
    main::f() called from /tmp/t.pl:4
    
=back

=head1 SEE ALSO

L<MDK::Common>

=cut
  

package MDK::Common::Various;

use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(first second top to_bool to_int to_float bool2text bool2yesno text2bool chomp_ backtrace);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


sub first { $_[0] }
sub second { $_[1] }
sub top { $_[-1] }

sub to_bool { $_[0] ? 1 : 0 }
sub to_int { $_[0] =~ /(\d*)/; $1 }
sub to_float { $_[0] =~ /(\d*(\.\d*)?)/; $1 }
sub bool2text { $_[0] ? "true" : "false" }
sub bool2yesno { $_[0] ? "yes" : "no" }
sub text2bool { my $t = lc($_[0]); $t eq "true" || $t eq "yes" ? 1 : 0 }

sub chomp_ { my @l = map { my $l = $_; chomp $l; $l } @_; wantarray ? @l : $l[0] }

sub backtrace {
    my $s;
    for (my $i = 1; caller($i); $i++) {
	my ($package, $file, $line, $func) = caller($i);
	$s .= "$func() called from $file:$line\n";
    }
    $s;
}

1;
