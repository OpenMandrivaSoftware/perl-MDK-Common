package read_t;

use lib '../..';
use MDK::Common;

sub read_t {
    my ($file) = @_;

    my @tests;
    my ($column_width, $line_number, @lines, @logs);
    foreach (cat_($file), "\n") {
	if (/^$/) {
	    push @tests, { line_number => $line_number, lines => [ @lines ], logs => [ @logs ] } if @lines;
	    @lines = @logs = ();
	} else {
	    $column_width ||= length(first(/(.{20}\s+)/));
	    my ($line, $log) = $column_width > 25 && /(.{$column_width})(.*)/ ? (chomp_($1) . "\n", $2) : ($_, '');
	    push @lines, $line;
	    push @logs, $log;
	}
	$line_number++;
    }
    @tests;
}

1;

