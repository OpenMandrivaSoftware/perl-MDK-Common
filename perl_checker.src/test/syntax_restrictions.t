$xxx <<= 2                               don't use "<<=", use the expanded version instead

m@xxx@                                   don't use m@...@, replace @ with / ! , or |

s:xxx:yyy:                               don't use s:...:, replace : with / ! , or |

qw/a b c/                                don't use qw/.../, use qw(...) instead

qw{a b c}                                don't use qw{...}, use qw(...) instead

q{xxx}                                   don't use q{...}, use q(...) instead

qq{xxx}                                  don't use qq{...}, use qq(...) instead

qx(xxx)                                  don't use qx(...), use `...` instead

-xxx                                     don't use -xxx, use '-xxx' instead

not $xxx                                 don't use "not", use "!" instead

$1 =~ /xxx/                              do not use the result of a match (eg: $1) to match another pattern

$xxx =~ "yyy"                            use a regexp, not a string

xxx() =~ s/xxx/yyy/                      you can only use s/// on a variable

$1 =~ /xxx/                              do not use the result of a match (eg: $1) to match another pattern

grep /xxx/, @l                           always use "grep" with a block (eg: grep { ... } @list)

for (@l) {}                              write "foreach" instead of "for"

foreach ($xxx = 0; $xxx < 9; $xxx++) {}  write "for" instead of "foreach"

foreach $xxx (@l) {}                     don't use for without "my"ing the iteration variable

foreach ($xxx) {}                        you are using the special trick to locally set $_ with a value, for this please use "for" instead of "foreach"

unless ($xxx) {} else {}                 don't use "else" with "unless" (replace "unless" with "if")

unless ($xxx) {} elsif ($yyy) {}         don't use "elsif" with "unless" (replace "unless" with "if")

zzz() unless $xxx || $yyy;               don't use "unless" when the condition is complex, use "if" instead

$$xxx{yyy}                               for complex dereferencing, use "->"

wantarray                                please use wantarray() instead of wantarray

eval                                     please use "eval $_" instead of "eval"

local *F; open F, "foo";                 use a scalar instead of a bareword (eg: occurrences of F with $F)

$xxx !~ s/xxx/yyy/                       use =~ instead of !~ and negate the return value

pkg::nop $xxx;                           use parentheses around argument (otherwise it might cause syntax errors if the package is "require"d and not "use"d

new foo $xxx                             you must parenthesize parameters: "new Class(...)" instead of "new Class ..."

*xxx = *yyy                              "*xxx = *yyy" is better written "*xxx = \&yyy"

$_xxx = 1                                variable $_xxx must not be used
                                           (variable with name _XXX are reserved for unused variables)

sub f2 { my ($x, $_y) = @_; $x }         not enough parameters
f2(@l);    # ok                          
f2(xxx()); # bad                         

$xxx = <<"EOF";                          Don't use <<"MARK", use <<MARK instead
foo                                      
EOF                                      
