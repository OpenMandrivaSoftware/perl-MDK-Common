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
L<MDK::Common::Globals>,
L<MDK::Common::Math>,
L<MDK::Common::String>,
L<MDK::Common::System>,
L<MDK::Common::Various>.

=head1 EXPORTS

C<MDK::Common> exports the functions in the package mentioned above. See their
manpage for more.

=head1 COPYRIGHT

Copyright (c) 2001 MandrakeSoft <pixel@mandrakesoft.com>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut

package MDK::Common;

use MDK::Common::DataStructure qw(:all);
use MDK::Common::File qw(:all);
use MDK::Common::Func qw(:all);
use MDK::Common::Math qw(:all);
use MDK::Common::String qw(:all);
use MDK::Common::System qw(:all);
use MDK::Common::Various qw(:all);

use vars qw(@ISA @EXPORT $VERSION); #);
@ISA = qw(Exporter);
# perl_checker: RE-EXPORT-ALL
@EXPORT = map { @$_ } map { values %{'MDK::Common::' . $_ . 'EXPORT_TAGS'} } grep { /::$/ } keys %MDK::Common::;

$VERSION = "1.0";

1;
