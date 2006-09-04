

print <<'EOF';
package MDK::Common;

=head1 NAME

MDK::Common - miscellaneous functions

=head1 SYNOPSIS

    use MDK::Common;
    # exports all functions, equivalent to

    use MDK::Common::DataStructure qw(:all);
    use MDK::Common::File qw(:all);
    use MDK::Common::Func qw(:all);
    use MDK::Common::Math qw(:all);
    use MDK::Common::String qw(:all);
    use MDK::Common::System qw(:all);
    use MDK::Common::Various qw(:all);

=head1 DESCRIPTION

C<MDK::Common> is a collection of packages containing various simple functions:
L<MDK::Common::DataStructure>,
L<MDK::Common::File>,
L<MDK::Common::Func>,
L<MDK::Common::Math>,
L<MDK::Common::String>,
L<MDK::Common::System>,
L<MDK::Common::Various>.

EOF

foreach my $f (<MDK/Common/*.pm>) {
    (my $pkg = $f) =~ s|/|::|g;
    open F, $f or die "can't open file $f";
    my $line;
    while (<F>) {
	$line++;
	if (/^=head1 (EXPORTS|OTHER)/ .. /^=back/) {
	    s/^=head1 EXPORTS/=head1 EXPORTS from $pkg/;
	    s/^=head1 OTHER/=head1 OTHER in $pkg/;
	    s/^=back/=back\n/;
	    /^\s+\n/ and warn "$f:$line: spaces only line\n";
	    print;
	}
    }
}


print <<'EOF';
=head1 COPYRIGHT

Copyright (c) 2001-2005 Mandriva <pixel@mandriva.com>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut


use MDK::Common::DataStructure qw(:all);
use MDK::Common::File qw(:all);
use MDK::Common::Func qw(:all);
use MDK::Common::Math qw(:all);
use MDK::Common::String qw(:all);
use MDK::Common::System qw(:all);
use MDK::Common::Various qw(:all);

use Exporter;
our @ISA = qw(Exporter);
# perl_checker: RE-EXPORT-ALL
our @EXPORT = map { @$_ } map { values %{'MDK::Common::' . $_ . 'EXPORT_TAGS'} } grep { /::$/ } keys %MDK::Common::;

our $VERSION = "1.2.3";

1;
EOF
