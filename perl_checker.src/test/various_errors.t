local $xxx ||= $yyy                      applying ||= on a new initialized variable is wrong

$1 =~ s/xxx/yyy/                         do not modify the result of a match (eg: $1)

$xxx[1, 2]                               you must give only one argument

$xxx[]                                   you must give one argument

'' || 'xxx'                              <constant> || ... is the same as ...

if ($xxx = '') {}                        are you sure you did not mean "==" instead of "="?

N("xxx$yyy")                             don't use interpolated translated string, use %s or %d instead

1 + 2 >> 3                               missing parentheses (needed for clarity)

$xxx ? $yyy = 1 : $zzz = 2;              missing parentheses (needed for clarity)

N_("xxx") . 'yyy'                        N_("xxx") . "yyy" is dumb since the string "xxx" will never get translated

join(@l)                                 first argument of join() must be a scalar

join(',', 'foo')                         join('...', $foo) is the same as $foo

my (@l2, $xxx) = @l;                     @l2 takes all the arguments, $xxx is undef in any case

$bad                                     undeclared variable $bad

{ my $a }                                unused variable $a

my $xxx; yyy($xxx); my $xxx;             redeclared variable $xxx

{ my $xxx; $xxx = 1 }                    variable $xxx assigned, but not read

$a                                       undeclared variable $a

use bad;                                 can't find package bad

use pkg3 ':bad';                         package pkg3 doesn't export tag :bad
bad();                                   unknown function bad

use pkg3 ':missing_fs';                  name &f is not defined in package pkg3
f();                                     name &f0 is not defined in package pkg3

use pkg3 'f';                            name &f is not defined in package pkg3
f();                                     
