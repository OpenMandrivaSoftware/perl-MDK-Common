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
