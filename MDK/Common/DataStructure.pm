=head1 NAME

MDK::Common::DataStructure - miscellaneous list/hash manipulation functions

=head1 SYNOPSIS

    use MDK::Common::DataStructure qw(:all);

=head1 EXPORTS

=over

=item ikeys(HASH)

aka I<sorted integer keys>, as simple as C<sort { $a E<lt>=E<gt> $b } keys>

=item add2hash(HASH REF, HASH REF)

adds to the first hash the second hash if the key/value is not already there

=item add2hash_

adds to the first hash the second hash if the key is not already there

=item put_in_hash

adds to the first hash the second hash, crushing existing key/values

=item member(SCALAR, LIST)

is the value in the list?

=item invbool(SCALAR REF)

toggles the boolean value

=item listlength(LIST)

returns the length of the list. Useful in list (opposed to array) context:

    sub f { "a", "b" } 
    my $l = listlength f();

whereas C<scalar f()> would return "b"

=item deref(REF)

de-reference

=item is_empty_array_ref(SCALAR)

is the scalar undefined or is the array empty

=item is_empty_hash_ref(SCALAR)

is the scalar undefined or is the hash empty

=item uniq(LIST)

returns the list with no duplicates

=item difference2(ARRAY REF, ARRAY REF)

returns the first list without the element of the second list

=item intersection(ARRAY REF, ARRAY REF, ...)

returns the elements which are in all lists

=item next_val_in_array(SCALAR, ARRAY REF)

finds the value that follow the scalar in the list (circular):
C<next_val_in_array(3, [1, 2, 3])> gives C<1>
(do not use a list with duplicates)

=item list2kv(LIST)

interprets the list as an hash, returns the keys and the values:
C<list2kv(1 => 2, 3 => 4)> gives C<[1,3], [2,4]>

=back

=head1 SEE ALSO

L<MDK::Common>

=cut

package MDK::Common::DataStructure;

use MDK::Common::Math;
use MDK::Common::Func;


use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(ikeys add2hash add2hash_ put_in_hash member invbool listlength deref is_empty_array_ref is_empty_hash_ref uniq difference2 intersection next_val_in_array list2kv);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


sub ikeys { my %l = @_; sort { $a <=> $b } keys %l }
sub put_in_hash { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { $a->{$k} = $v } $a }
sub add2hash    { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { $a->{$k} ||= $v } $a }
sub add2hash_   { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { exists $a->{$k} or $a->{$k} = $v } $a }
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
    my %l = MDK::Common::Func::mapn(sub { @_ }, $l, [ @$l[1..$#$l], $l->[0] ]);
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