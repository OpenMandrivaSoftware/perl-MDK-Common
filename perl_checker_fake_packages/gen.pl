#!/usr/bin/perl -w

use strict;
use MDK::Common;

my ($current_package, $current_prefix, $current_name);

my %l;
sub get_paras {
    my ($name, $para) = @_;
    $name =~ s/\Q$current_prefix//;
    $current_name = $name;
    $l{$current_package}{$name} = [ map {
	if (/\Q.../) {
	    '@_more_paras';
	} else {
	    my ($optional) = s/=(.*)//;
	    my $s = /.*\W(\w+)/ ? $1 : $_;
	    '$_' . ($optional ? 'o_' : '') . $s;
	}
    } grep { !/OUTLIST/ } split(',', $para) ];
}

sub parse_xs {
    my ($file) = @_;
    warn "parse_xs $file\n";
    my $state = 'waiting_for_type';
    ($current_package, $current_prefix) = ('', '');
    my $multi_line;
    my $c;
    foreach (cat_($file)) {
	$c++;
	next if /^=/ ... /^=cut/;
	chomp;
	my $orig_line = $_;

	if (/^\s*#/ || (m!^\s*/\*! .. m!\*/!)) {
	    # forget it
	} elsif ($state eq 'multi_line') {
	    if (/(.*)\)/) {
		get_paras($current_name, $multi_line . $1);
		$state = 'waiting_for_end';
	    } else {
		$multi_line .= $_;
	    }
#	} elsif (/^\s*gperl_set_isa\s*\("(.*)", ".*"\)\s*;/) {
	} elsif (/MODULE\s*=\s*\S+\s+PACKAGE\s*=\s*(\S+)\s+PREFIX\s*=\s*(\S+)/) {
	    ($current_package, $current_prefix) = ($1, $2);
	} elsif (/MODULE\s*=\s*\S+\s+PACKAGE\s*=\s*(\S+)/) {
	    ($current_package, $current_prefix) = ($1, '');
	} elsif (!$current_package) {
	    # waiting for the MODULE line
	} elsif (/^\s*$/) {
	    $state = 'waiting_for_type';
	} elsif (/^\w[^\(]*$/ && $state eq 'waiting_for_type') {
	    $state = 'waiting_for_function' if !/^BOOT:/ && !/;/;
	} elsif (/^\s*ALIAS:\s*$/) {
	    $state = 'alias';
	} elsif ($state eq 'alias') {
	    if (my ($f) = /^\s*(\S+)\s*=\s*\d+\s*$/) {
		my $pkg = $f =~ s/(.*)::// ? $1 : $current_package;
		$l{$pkg}{$f} ||= $l{$current_package}{$current_name};
	    } else {
		warn "bad line #$c $orig_line (state: $state)\n" if !/^\s*\w+:\s*$/ && !/^\s*$/;
		$state = 'waiting_for_end';
	    }
	} elsif ($state eq 'waiting_for_type' && s/^(const\s*)?\w+\s*(\*\s*)?// || 
		 $state eq 'waiting_for_function' && /^\w+/) {
	    if (my ($name, $para) = /^(\S+)\s*\((.*)\)\s*;?\s*$/) {
		get_paras($name, $para);
		$state = 'waiting_for_end';
	    } elsif (($name, $para) = /^(\S+)\s*\((.*)$/) {
		$multi_line = $para;
		$current_name = $name;
		$state = 'multi_line';
	    } else {
		warn "bad line #$c $orig_line (state: $state)\n";
	    }
	} else {
	    warn "bad line #$c $orig_line (state: $state)\n" if 
	      !(($state eq 'waiting_for_end' || $state eq 'waiting_for_type') && 
		(/^\s/ || /^[{}]\s*$/ || /^(CODE|OUTPUT):\s*$/));
	}
    }
}


my ($pkg_name, $dir) = @ARGV;
my @xs_files = chomp_(`find $dir -name "*.xs"`);
@ARGV == 2 && @xs_files or die "usage: gen.pl <Gtk2 or Glib> <dir where Gtk2's or Glib's *.xs are>\n";

parse_xs($_) foreach @xs_files;

print "package $pkg_name;\nuse Glib;\n" if $pkg_name eq 'Gtk2';

foreach my $pkg (sort keys %l) {
    print "\npackage $pkg;\n";
    print "our \@ISA = qw();\n";
    foreach my $name (sort keys %{$l{$pkg}}) {
	my $para = $l{$pkg}{$name};
	$name = $pkg . '::' . $name if $name =~ /^(length|x|y|eq|foreach|format)$/;
	if (@$para) {
	    print "sub $name { my (", join(", ", @$para), ") = \@_ }\n";
	} else {
	    print "sub $name() {}\n";
	}
    }
}
