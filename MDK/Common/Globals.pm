package MDK::Common::Globals;

=head1 NAME

Shares constant values between modules

=head1 SYNOPSIS

    use MDK::Common::Globals "foo", qw($a $b);
    
    MDK::Common::Globals::init(a => 2, b => 3);
    
    print $a;  # 2

=cut


sub import {
    my (undef, $name, @globals) = @_;
    foreach (@globals) {
	$name =~ /^\$/ and die qq(usage : use MDK::Common::Globals "group", qw(\$var1 \$var2 ...);\n);
	s/^\$// or die qq(bad parameter to "use MDK::Common::Globals": missing variable ``$_'' should be written ``\$$_''); #);

	no strict 'refs';
	my $v = caller() . '::' . $_;
	my $lv = "$foo __ $_";
	*$v = *$lv;
	eval { undef = $$lv; tie $$lv, 'MDK::Common::Globals', $_ };
    }
}

sub init {
    @_ % 2 == 0 or die "usage MDK::Common::Globals::init(key => val, key2 => val2, ...)\n";
    my %l = @_;
    foreach (keys %l) {
	my $v = caller() . '::' . $_;
	no strict 'refs';
	$$v = $l{$_};
    }
}

sub TIESCALAR {
    my ($class, $name) = @_;
    my $var;
    bless [$var, undef, $name], $class;
}

sub STORE {
    my ($o, $val) = @_;
    $o->[1] and die "MDK::Common::Globals::$o->[2] already set\n";
    $o->[1] = 1;
    $o->[0] = $val;
}

sub FETCH {
    my ($o) = @_;
    $o->[1] or die "MDK::Common::Globals::$o->[2] unset\n";
    $o->[0];
}

1;
