package MDK::Common::Func;

use MDK::Common::Math;


use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(fold_left mapn mapn_ map_index grep_index find_index map_each grep_each before_leaving catch_cdie cdie);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


sub fold_left(&@) {
    my ($f, $initial, @l) = @_;
    local ($::a, $::b);
    $::a = $initial;
    foreach $::b (@_) { $::a = &$f() }
    $::a
}

sub smapn {
    my $f = shift;
    my $n = shift;
    my @r = ();
    for (my $i = 0; $i < $n; $i++) { push @r, &$f(map { $_->[$i] } @_); }
    @r
}
sub mapn(&@) {
    my $f = shift;
    smapn($f, MDK::Common::Math::min(map { scalar @$_ } @_), @_);
}
sub mapn_(&@) {
    my $f = shift;
    smapn($f, MDK::Common::Math::max(map { scalar @$_ } @_), @_);
}


sub map_index(&@) {
    my $f = shift;
    my @v; local $::i = 0;
    map { @v = &$f($::i); $::i++; @v } @_;
}
sub grep_index(&@) {
    my $f = shift;
    my $v; local $::i = 0;
    grep { $v = &$f($::i); $::i++; $v } @_;
}
sub find_index(&@) {
    my $f = shift;
    local $_;
    for (my $i = 0; $i < @_; $i++) {
	$_ = $_[$i];
	&$f and return $i;
    }
    die "find_index failed in @_";
}
sub map_each(&%) {
    my ($f, %h) = @_;
    my @l;
    local ($::a, $::b);
    while (($::a, $::b) = each %h) { push @l, &$f($::a, $::b) }
    @l;
}
sub grep_each(&%) {
    my ($f, %h) = @_;
    my %l;
    local ($::a, $::b);
    while (($::a, $::b) = each %h) { $l{$::a} = $::b if &$f($::a, $::b) }
    %l;
}


sub add_f4before_leaving {
    my ($f, $b, $name) = @_;

    unless ($MDK::Common::Func::before_leaving::{$name}) {
	no strict 'refs';
	${"MDK::Common::Func::before_leaving::$name"} = 1;
	${"MDK::Common::Func::before_leaving::list"} = 1;
    }
    local *N = *{$MDK::Common::Func::before_leaving::{$name}};
    my $list = *MDK::Common::Func::before_leaving::list;
    $list->{$b}{$name} = $f;
    *N = sub {
	my $f = $list->{$_[0]}{$name} or die '';
	$name eq 'DESTROY' and delete $list->{$_[0]};
	goto $f;
    } unless defined &{*N};

}

#- ! the functions are not called in the order wanted, in case of multiple before_leaving :(
sub before_leaving(&) {
    my ($f) = @_;
    my $b = bless {}, 'MDK::Common::Func::before_leaving';
    add_f4before_leaving($f, $b, 'DESTROY');
    $b;
}

sub catch_cdie(&&) {
    my ($f, $catch) = @_;

    local @MDK::Common::Func::cdie_catches;
    unshift @MDK::Common::Func::cdie_catches, $catch;
    &$f();
}

sub cdie {
    my ($err, $f) = @_;
    foreach (@MDK::Common::Func::cdie_catches) {
	$@ = $err;
	&{$_}(\$err) and return;
    }
    die $err;
}

1;

