package MDK::Common::Math;

use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK $PRECISION $PI);
@ISA = qw(Exporter);
@EXPORT_OK = qw($PI even odd sqr sign round round_up round_down divide min max or_ and_ sum product factorize decimal2fraction poly2);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


$PRECISION = 10;
$PI = 3.1415926535897932384626433832795028841972;

sub even { $_[0] % 2 == 0 }
sub odd  { $_[0] % 2 == 1 }
sub sqr  { $_[0] * $_[0] }
sub sign { $_[0] <=> 0 }
sub round { int ($_[0] + 0.5) }
sub round_up { my ($i, $r) = @_; $i = int $i; $i += $r - ($i + $r - 1) % $r - 1; }
sub round_down { my ($i, $r) = @_; $i = int $i; $i -= $i % $r; }
sub divide { my $d = int $_[0] / $_[1]; wantarray ? ($d, $_[0] % $_[1]) : $d }
sub min { my $n = shift; $_ < $n and $n = $_ foreach @_; $n }
sub max { my $n = shift; $_ > $n and $n = $_ foreach @_; $n }
sub or_ { my $n = 0; $n ||= $_ foreach @_; $n }
sub and_{ my $n = 1; $n &&= $_ foreach @_; $n }
sub sum { my $n = 0; $n  += $_ foreach @_; $n }
sub product { my $n = 1; $n  *= $_ foreach @_; $n }


sub factorize {
    my ($n) = @_;
    my @r = ();

    $n == 1 and return [ 1, 1 ];
    for (my $k = 2; sqr($k) <= $n; $k++) {
	my $i = 0;
	for ($i = 0; $n % $k == 0; $i++) { $n /= $k; }
	$i and push @r, [ $k, $i ];
    }
    $n > 1 and push @r, [ $n, 1 ];
    @r;
}

sub decimal2fraction { # ex: 1.33333333 -> (4, 3)
    my $n0 = shift;
    my $precision = 10 ** -(shift || $PRECISION);
    my ($a, $b) = (int $n0, 1);
    my ($c, $d) = (1, 0);
    my $n = $n0 - int $n0;
    my $k;
    until (abs($n0 - $a / $c) < $precision) {
	$n = 1 / $n;
	$k = int $n;
	($a, $b) = ($a * $k + $b, $a);
	($c, $d) = ($c * $k + $d, $c);
	$n -= $k;
    }
    ($a, $c)
}

sub poly2 {
    my ($a, $b, $c) = @_;
    my $d = ($b**2 - 4 * $a * $c) ** 0.5;  
    (-$b + $d) / 2 / $a, (-$b - $d) / 2 / $a
}

1;
