@{$xxx}                                  @{$xxx} can be written @$xxx

$h{"yyy"}                                {"yyy"} can be written {yyy}

"$xxx"                                   $xxx is better written without the double quotes

$xxx->{yyy}->{zzz}                       the arrow "->" is unneeded

"xxx\$xxx"                               you can replace "xxx\$xxx" with 'xxx$xxx', that way you don't need to escape <$>

"xxx\$xxx"                               you can replace "xxx\$xxx" with 'xxx$xxx', that way you don't need to escape <$>

"xxx\"$xxx"                              you can replace "xxx\"xxx" with qq(xxx"xxx), that way you don't need to escape <">

/xxx\'xxx/                               you can replace \' with '

/xxx\;xxx/                               you can replace \; with ;

/\//                                     change the delimit character / to get rid of this escape

{ nop(); }                               spurious ";" before closing block

+1                                       don't use unary +

return ($xxx)                            unneeded parentheses

if (($xxx eq $yyy) || $zzz) {}           unneeded parentheses

if (($xxx =~ /yyy/) || $zzz) {}          unneeded parentheses

nop() foreach ($xxx, $yyy);              unneeded parentheses

($xxx) ||= 'xxx'                         remove the parentheses

$o->m0()                                 remove these unneeded parentheses

$o = xxx() if !$o;                       "$foo = ... if !$foo" can be written "$foo ||= ..."

$o = xxx() unless $o;                    "$foo = ... unless $foo" can be written "$foo ||= ..."

$o or $o = xxx();                        "$foo or $foo = ..." can be written "$foo ||= ..."

$_ =~ s/xxx/yyy/                         "$_ =~ s/regexp/.../" can be written "s/regexp/.../"

$xxx =~ /^yyy$/                          "... =~ /^yyy$/" is better written "... eq 'yyy'"

/xxx.*/                                  you can remove ".*" at the end of your regexp

/xxx.*$/                                 you can remove ".*$" at the end of your regexp

/[^\s]/                                  you can replace [^\s] with \S

/[^\w]/                                  you can replace [^\w] with \W

$xxx ? $xxx : $yyy                       you can replace "$foo ? $foo : $bar" with "$foo || $bar"

my @l = ();                              no need to initialize variables, it's done by default

$l[$#l]                                  you can replace $#l with -1

$#l == 0                                 $#x == 0 is better written @x == 1

$#l == -1                                $#x == -1 is better written @x == 0

$#l < 0                                  change your expression to use @xxx instead of $#xxx

$l[@l] = 1                               "$a[@a] = ..." is better written "push @a, ..."

xxx(@_)                                  replace xxx(@_) with &xxx

member($xxx, keys %h)                    you can replace "member($xxx, keys %yyy)" with "exists $yyy{$xxx}"

!($xxx =~ /.../)                         !($var =~ /.../) is better written $var !~ /.../

!($xxx == 1)                             !($foo == $bar) is better written $foo != $bar

!($xxx eq 'foo')                         !($foo eq $bar) is better written $foo ne $bar

grep { !member($_, qw(a b c)) } @l       you can replace "grep { !member($_, ...) } @l" with "difference2([ @l ], [ ... ])"

any { $_ eq 'foo' } @l                   you can replace "any { $_ eq ... } @l" with "member(..., @l)"

foreach (@l) {                           use "push @l2, grep { ... } ..." instead of "foreach (...) { push @l2, $_ if ... }"
  push @l2, $_ if yyy($_);                 or sometimes "@l2 = grep { ... } ..."
}                                        

foreach (@l) {                           use "push @l2, map { ... } ..." instead of "foreach (...) { push @l2, ... }"
    push @l2, yyy($_);                     or sometimes "@l2 = map { ... } ..."
}                                        

foreach (@l) {                           use "push @l2, map { ... ? ... : () } ..." instead of "foreach (...) { push @l2, ... if ... }"
  push @l2, yyy($_) if zzz($_);            or sometimes "@l2 = map { ... ? ... : () } ..."
}                                          or sometimes "@l2 = map { if_(..., ...) } ..."

foreach (@l) {                           use "$xxx = find { ... } ..."
  if (xxx($_)) {                         
    $xxx = $_;                           
    last;                                
  }                                      
}                                        

if (grep { xxx() } @l) {}                in boolean context, use "any" instead of "grep"

$xxx = grep { xxx() } @l;                you may use "find" instead of "grep"

$xxx ? $yyy : ()                         you may use if_() here
                                           beware that the short-circuit semantic of ?: is not kept
                                           if you want to keep the short-circuit behaviour, replace () with @{[]} and there will be no warning anymore

system(qq(foo "$xxx"))                   instead of quoting parameters you should give a list of arguments

system("mkdir", $xxx)                    you can replace system("mkdir ...") with mkdir(...)
