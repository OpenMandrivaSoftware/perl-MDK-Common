package MDK::Common::DataStructure;

use MDK::Common::Math;
use MDK::Common::Func;


use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(ikeys add2hash add2hash_ put_in_hash member invbool listlength strcpy deref is_empty_array_ref is_empty_hash_ref uniq difference2 intersection next_val_in_array list2kv);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


sub ikeys { my %l = @_; sort { $a <=> $b } keys %l }
sub add2hash  { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { $a->{$k} ||= $v } $a }
sub add2hash_ { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { exists $a->{$k} or $a->{$k} = $v } $a }
sub put_in_hash { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { $a->{$k} = $v } $a }
sub member { my $e = shift; foreach (@_) { $e eq $_ and return 1 } 0 }
sub invbool { my $a = shift; $$a = !$$a; $$a }
sub listlength { scalar @_ }
sub strcpy { substr($_[0], $_[2] || 0, length $_[1]) = $_[1] }
sub deref { ref $_[0] eq "ARRAY" ? @{$_[0]} : ref $_[0] eq "HASH" ? %{$_[0]} : $_[0] }

sub is_empty_array_ref { my $a = shift; !defined $a || @$a == 0 }
sub is_empty_hash_ref { my $a = shift; !defined $a || keys(%$a) == 0 }

sub uniq { my %l; @l{@_} = (); keys %l }
sub difference2 { my %l; @l{@{$_[1]}} = (); grep { !exists $l{$_} } @{$_[0]} }
sub intersection { my (%l, @m); @l{@{shift @_}} = (); foreach (@_) { @m = grep { exists $l{$_} } @$_; %l = (); @l{@m} = (); } keys %l }


sub next_val_in_array {
    my ($v, $l) = @_;
    my %l = mapn { @_ } $l, [ @$l[1..$#$l], $l->[0] ];
    $l{$v};
}


sub list2kv { 
    my (@k, @v);
    for (my $i = 0; $i < @_; $i += 2) {	
	push @k, $_[$i + 0];
	push @v, $_[$i + 1];
    }
    \@k, \@v;
}

1;
