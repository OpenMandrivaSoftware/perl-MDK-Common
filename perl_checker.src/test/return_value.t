if ($xxx or $yyy) {}                     value should be dropped
                                         context () is not compatible with context scalar

if ($xxx and $yyy) {}                    value should be dropped
                                         context () is not compatible with context scalar

$xxx && yyy();                           value is dropped

`xxx`;                                   value is dropped

/(.*)/;                                  value is dropped

map { xxx($_) } @l;                      if you don't use the return value, use "foreach" instead of "map"

$xxx = chomp;                            () context not accepted here
                                         context () is not compatible with context scalar
