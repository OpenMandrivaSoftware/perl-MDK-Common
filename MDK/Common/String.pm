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
    $err =~ s/ at .*?$/\./ if !$::testing;
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
	my $t = '';
	foreach (split /\s+/, $_) {
	    if (length "$t $_" > $width) {
		push @l, $t;
		$t = $_;
	    } else {
		$t = "$t $_";
	    }
	}
	push @l, $t;
    }
    @l;
}

1;
