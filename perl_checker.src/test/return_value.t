if ($xxx or $yyy) {}                     value should be dropped
                                         context () is not compatible with context bool

if ($xxx and $yyy) {}                    value should be dropped
                                         context () is not compatible with context bool

$xxx && yyy();                           value is dropped

`xxx`;                                   value is dropped

/(.*)/;                                  value is dropped

'xxx';

'xxx' if $xxx;

map { xxx($_) } @l;                      if you don't use the return value, use "foreach" instead of "map"

$xxx = chomp;                            () context not accepted here
                                         context () is not compatible with context scalar

$xxx = push @l, 1                        () context not accepted here
                                         context () is not compatible with context scalar
