=head1 NAME

MDK::Common::String - formatting functions

=head1 SYNOPSIS

    use MDK::Common::String qw(:all);

=head1 EXPORTS

=over

=item bestMatchSentence(STRING, LIST)

finds in the list the best corresponding string

=item formatList(INT, LIST)

if the list size is bigger than INT, replace the remaining elements with "...".

formatList(3, qw(a b c d e))  # => "a, b, c, ..."

=item formatError(STRING)

the string is something like "error at foo.pl line 2" that you get when
catching an exception. formatError will remove the "at ..." so that you can
nicely display the returned string to the user

=item formatTimeRaw(TIME)

the TIME is an epoch as returned by C<time>, the formatted time looks like "23:59:00"

=item formatLines(STRING)

remove "\n"s when the next line doesn't start with a space. Otherwise keep
"\n"s to keep the indentation.

=item formatAlaTeX(STRING)

handle carriage return just like LaTeX: merge lines that are not separated by
an empty line

=item warp_text(STRING, INT)

return a list of lines which do not exceed INT characters

=item warp_text(STRING)

warp_text at a default width (80)

=back

=head1 SEE ALSO

L<MDK::Common>

=cut

package MDK::Common::String;

use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(bestMatchSentence formatList formatError formatTimeRaw formatLines formatAlaTeX warp_text);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


# count the number of character that match
sub bestMatchSentence {

    my $best = -1;
    my $bestSentence;
    my @s = split /\W+/, shift;
    foreach (@_) {
	my $count = 0;
	foreach my $e (@s) {
	    $count+= length ($e) if /^$e$/;
	    $count+= length ($e) if /^$e$/i;
	    $count+= length ($e) if /$e/;
	    $count+= length ($e) if /$e/i;
	}
	$best = $count, $bestSentence = $_ if $count > $best;
    }
    wantarray ? ($bestSentence, $best) : $bestSentence;
}


sub formatList {
    my $nb = shift;
    join(", ", @_ <= $nb ? @_ : (@_[0..$nb-1], '...'));
}
sub formatError {
    my ($err) = @_;
    $err =~ s/ at .*?$/\./s if !$::testing;
    $err;
}
sub formatTimeRaw {
    my ($s, $m, $h) = gmtime($_[0]);
    sprintf "%d:%02d:%02d", $h, $m, $s;
}
sub formatLines {
    my ($t, $tmp);
    foreach (split "\n", $_[0]) {
	if (/^\s/) {
	    $t .= "$tmp\n";
	    $tmp = $_;
	} else {
	    $tmp = ($tmp ? "$tmp " : ($t && "\n") . $tmp) . $_;
	}
    }
    "$t$tmp\n";
}
sub formatAlaTeX {
    my ($t, $tmp);
    foreach (split "\n", $_[0]) {
	if (/^$/) {
	    $t .= ($t && "\n") . $tmp;
	    $tmp = '';
	} else {
	    $tmp = ($tmp && "$tmp ") . (/^\s*(.*?)\s*$/)[0];
	}
    }
    $t . ($t && $tmp && "\n") . $tmp;
}




sub warp_text {
    my ($text, $width) = @_;
    $width ||= 80;

    my @l;
    foreach (split "\n", $text) {
	my ($beg) = /^(\s*)/;
	my $t = '';
	foreach (split /\s+/, $_) {
	    if (length "$beg$t $_" > $width) {
		push @l, "$beg$t";
                $beg = '';
		$t = $_;
	    } else {
		$t = $t ? "$t $_" : $_;
	    }
	}
	push @l, "$beg$t";
    }
    @l;
}

1;
