package MDK::Common::Various;

use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(first second top bool to_int to_float bool2text bool2yesno text2bool chomp_ backtrace);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


sub first { $_[0] }
sub second { $_[1] }
sub top { $_[-1] }

sub bool { $_[0] ? 1 : 0 }
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

