foreach (%h) {}                          context hash is not compatible with context list
                                         foreach with a hash is usually an error

map { 'xxx' } %h                         a hash is not a valid parameter to function map

$xxx = ('yyy', 'zzz')                    context tuple(string, string) is not compatible with context scalar

@l ||= 'xxx'                             "||=" is only useful with a scalar

length @l                                never use "length @l", it returns the length of the string int(@l)

%h . 'yyy'                               context hash is not compatible with context string

'xxx' > 'yyy'                            context string is not compatible with context float
                                         context string is not compatible with context float
                                         

1 cmp 2                                  you should use a number operator, not the string operator "cmp" (or replace the number with a string)

$xxx == undef                            context undef is not compatible with context float

my ($xxx) = 1                            context int is not compatible with context tuple(scalar)

($xxx, $yyy) = 1                         context int is not compatible with context tuple(scalar, scalar)

($xxx, $yyy) = (1, 2, 3)                 context tuple(int, int, int) is not compatible with context tuple(scalar, scalar)

@l eq '3'                                context array is not compatible with context string

qw(a b) > 2                              context tuple(string, string) is not compatible with context float

%h > 0                                   context hash is not compatible with context float

%h eq 0                                  context hash is not compatible with context string
                                         you should use a number operator, not the string operator "eq" (or replace the number with a string)

@l == ()

$xxx = { xxx() }->{xxx};

$xxx = { xxx() }->{$xxx};
