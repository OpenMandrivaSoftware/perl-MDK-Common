foreach (%h) {}                          context hash is not compatible with context list
                                         foreach with a hash is usually an error

map { 'xxx' } %h                         a hash is not a valid parameter to function map

$xxx = ('yyy', 'zzz')                    context tuple(string, string) is not compatible with context scalar

@l ||= 'xxx'                             "||=" is only useful with a scalar

length @l                                never use "length @l", it returns the length of the string int(@l)

%h . 'yyy'                               context hash is not compatible with context string

'xxx' > 'yyy'                            context string is not compatible with context float
                                         context string is not compatible with context float
                                         you should use a string operator, not the number operator ">"

1 cmp 2                                  you should use a number operator, not the string operator "cmp" (or replace the number with a string)

$xxx == undef                            context undef is not compatible with context float

