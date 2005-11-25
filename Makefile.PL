use ExtUtils::MakeMaker;

if ($ENV{MAKEFILE_NAME}) {
    symlink '.', 'lib';
    system("perl lib/MDK/Common.pm.pl > lib/MDK/Common.pm");
}

WriteMakefile(
    NAME          => 'MDK::Common',
    VERSION_FROM  => 'lib/MDK/Common.pm',
    ABSTRACT_FROM => 'lib/MDK/Common.pm',
    AUTHOR        => 'Pixel <pixel@mandriva.com>',
    $ENV{MAKEFILE_NAME} ? (
	MAKEFILE => $ENV{MAKEFILE_NAME},
	dist	 => { COMPRESS => "bzip2 -f" },
    ) : (),
);
