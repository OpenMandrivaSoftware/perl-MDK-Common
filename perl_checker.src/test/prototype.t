
sub xxx { 'yyy' }                        if the function doesn't take any parameters, please use the empty prototype.
                                         example "sub foo() { ... }"

sub xxx {                                an non-optional argument must not follow an optional argument
  my ($o_xxx, $yyy) = @_;                
  ($o_xxx, $yyy);                        
}                                        

sub xxx {                                an array must be the last variable in a prototype
  my (@xxx, $yyy) = @_;                  
  @xxx, $yyy;
}                                        

bad()                                    unknown function bad

sub f0() {}                              too many parameters
f0('yyy')

sub f2 { my ($x, $_y) = @_; $x }         not enough parameters
f2('yyy')

N("xxx %s yyy")                          not enough parameters
