=head1 NAME

MDK::Common::System - formatting functions

=head1 SYNOPSIS

    use MDK::Common::System qw(:all);

=head1 EXPORTS

=over

=item %compat_arch

architecture compatibility mapping (eg: k6 => i586, k7 => k6 ...)

=item %printable_chars

7 bit ascii characters

=item $sizeof_int

sizeof(int)

=item $bitof_int

$sizeof_int * 8

=item arch()

return the architecture (eg: i686, ppc, ia64, k7...)

=item typeFromMagic(FILENAME, LIST)

find the first corresponding magic in FILENAME. eg of LIST:

    [ 'empty', 0, "\0\0\0\0" ],
    [ 'grub', 0, "\xEBG", 0x17d, "stage1 \0" ],
    [ 'lilo', 0x2,  "LILO" ],

where each entry is [ magic_name, offset, string, offset, string, ... ].

=item list_passwd()

return the list of users as given by C<getpwent> (see perlfunc)

=item list_home()

return the list of home (eg: /home/foo, /home/pixel, ...)

=item list_skels()

return the directories where we can find dot files: homes, /root and /etc/skel

=item syscall_(NAME, PARA)

calls the syscall NAME

=item psizeof(STRING)

useful to know the length of a C<pack> format string. 

    psizeof("I I I C C S") = 4 + 4 + 4 + 1 + 1 + 2 = 16

=item availableMemory()

size of swap + memory

=item availableRamMB()

size of RAM as reported by the BIOS (it is a round number that can be
displayed or given as "mem=128M" to the kernel)

!! "mem=..." is dangerous in 2.4 kernels

=item gettimeofday()

returns the epoch in microseconds

=item unix2dos(STRING)

takes care of CR/LF translation

=item getVarsFromSh(FILENAME)

returns a hash associating shell variables to their value. useful for config
files such as /etc/sysconfig files

=item setVarsInSh(FILENAME, HASH REF)

write file in shell format association a shell variable + value for each
key/value

=item setVarsInSh(FILENAME, HASH REF, LIST)

restrict the fields that will be printed to LIST

=item setVarsInShMode(FILENAME, INT, HASH REF, LIST)

like setVarsInSh with INT being the chmod value for the config file

=item setVarsInCsh(FILENAME, HASH REF, LIST)

same as C<setVarsInSh> for csh format

=item template2file(FILENAME_IN, FILENAME_OUT, HASH)

read in a template file, replace keys @@@key@@@ with value, save it in out
file

=item template2userfile(PREFIX, FILENAME_IN, FILENAME_OUT, BOOL, HASH)

read in a template file, replace keys @@@key@@@ with value, save it in every homes.
If BOOL is true, overwrite existing files. FILENAME_OUT must be a relative filename

=item update_gnomekderc(FILENAME, STRING, HASH)

modifies GNOME-like and KDE-like config files (aka windows-like).
If the category doesn't exist, it creates it. eg:

    update_gnomekderc("/etc/skels/.kderc", 'KDE', 
		      kfmIconStyle => "Large")

=back

=head1 OTHER

=over

=item better_arch(ARCH1, ARCH2)

is ARCH1 compatible with ARCH2?

better_arch('i386', 'ia64') and better_arch('ia64', 'i386') are false

better_arch('k7', 'k6') is true and better_arch('k6', 'k7') is false

=item compat_arch(STRING)

test the architecture compatibility. eg: 

compat_arch('i386') is false on a ia64

compat_arch('k6') is true on a k6 and k7 but false on a i386 and i686

=back

=head1 SEE ALSO

L<MDK::Common>

=cut

package MDK::Common::System;

use MDK::Common::Math;
use MDK::Common::File;


use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK %compat_arch $printable_chars $sizeof_int $bitof_int); #);
@ISA = qw(Exporter);
@EXPORT_OK = qw(%compat_arch $printable_chars $sizeof_int $bitof_int arch typeFromMagic list_passwd list_home list_skels syscall_ psizeof availableMemory availableRamMB gettimeofday unix2dos getVarsFromSh setVarsInSh setVarsInShMode setVarsInCsh template2file template2userfile update_gnomekderc); #);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);


%compat_arch = ( #- compatibilty arch mapping.
		     'noarch'  => undef,
		     'ia32'    => 'noarch',
		     'i386'    => 'ia32',
		     'i486'    => 'i386',
		     'i586'    => 'i486',
		     'i686'    => 'i586',
		     'i786'    => 'i686',
		     'k6'      => 'i586',
		     'k7'      => 'k6',
		     'k8'      => 'k7',
		     'ia64'    => 'noarch',
		     'ppc'     => 'noarch',
		     'alpha'   => 'noarch',
		     'sparc'   => 'noarch',
		     'sparc32' => 'sparc',
		     'sparc64' => 'sparc32',
		     'ia64'    => 'noarch',
		   );

$printable_chars = "\x20-\x7E";
$sizeof_int      = psizeof("i");
$bitof_int       = $sizeof_int * 8;


sub arch() {
    my $SYS_NMLN = 65;
    my $format = "Z$SYS_NMLN" x 6;
    my $t = pack $format;
    syscall_('uname', $t);
    (unpack($format, $t))[4];
}
sub better_arch {
    my ($new, $old) = @_;
    while ($new && $new ne $old) { $new = $compat_arch{$new} }
    $new;
}
sub compat_arch { better_arch(arch(), $_[0]) }

sub typeFromMagic {
    my $f = shift;
    local *F; sysopen F, $f, 0 or return;

    my $tmp;
  M: foreach (@_) {
	my ($name, @l) = @$_;
	while (@l) {
	    my ($offset, $signature) = splice(@l, 0, 2);
	    sysseek(F, $offset, 0) or next M;
	    sysread(F, $tmp, length $signature);
	    $tmp eq $signature or next M;
	}
	return $name;
    }
    undef;
}


sub list_passwd() {
    my (@l, @e);
    setpwent();
    while (@e = getpwent()) { push @l, [ @e ] }
    endpwent();
    @l;
}
sub list_home() {
    map { $_->[7] } grep { $_->[2] >= 500 } list_passwd();
}
sub list_skels { 
    my ($prefix, $suffix) = @_;
    grep { -d $_ && -w $_ } map { "$prefix$_/$suffix" } '/etc/skel', '/root', list_home();
}



sub syscall_ {
    my $f = shift;

    require 'syscall.ph';
    syscall(&{"SYS_$f"}, @_) == 0;
}


#- return the size of the partition and its free space in KiB
sub df {
    my ($mntpoint) = @_;
    my ($blocksize, $size, $free);
    my $buf = ' ' x 20000;
    syscall_('statfs', $mntpoint, $buf) or return;
    (undef, $blocksize, $size, $free, undef, undef) = unpack "L!6", $buf;
    map { $_ * ($blocksize / 1024) } $size, $free;
}

sub sync { syscall_('sync') }
sub psizeof { length pack $_[0] }
sub availableMemory() { MDK::Common::Math::sum(map { /(\d+)/ } grep { /^(MemTotal|SwapTotal):/ } MDK::Common::File::cat_("/proc/meminfo")); }
sub availableRamMB() { 4 * MDK::Common::Math::round((-s '/proc/kcore') / 1024 / 1024 / 4); }
sub gettimeofday { my $t = pack "LL"; syscall_('gettimeofday', $t, 0) or die "gettimeofday failed: $!\n"; unpack("LL", $t) }
sub unix2dos { local $_ = $_[0]; s/\015$//mg; s/$/\015/mg; $_ }



sub getVarsFromSh {
    my %l;
    local *F; open F, $_[0] or return;
    local $_;
    while (<F>) {
	s/#.*//; # remove comments
	my ($v, $val, $val2) =
	  /^\s*			# leading space
	   (\w+) =		# variable
	   (
   	       "([^"]*)"	# double-quoted text
   	     | '([^']*)'	# single-quoted text
   	     | [^'"\s]+		# normal text
           )
           \s*$			# end of line
          /x or next;
	$l{$v} = defined $val2 ? $val2 : $val;
    }
    %l;
}

sub setVarsInSh {
    my ($file, $l, @fields) = @_;
    setVarsInShMode($file, 0777 ^ umask(), $l, @fields);
}

sub setVarsInShMode {
    my ($file, $mod, $l, @fields) = @_;
    @fields = keys %$l unless @fields;

    local *F;
    open F, "> $file" or die "cannot create config file $file";
    chmod $mod, $file;
    $l->{$_} and print F "$_=$l->{$_}\n" foreach @fields;
}

sub setVarsInCsh {
    my ($file, $l, @fields) = @_;
    @fields = keys %$l unless @fields;

    local *F;
    open F, "> $_[0]" or die "cannot create config file $file";
    $l->{$_} and print F "setenv $_ $l->{$_}\n" foreach @fields;
}

sub template2file {
    my ($in, $out, %toreplace) = @_;
    output $out, map { s/@@@(.*?)@@@/$toreplace{$1}/g; $_ } MDK::Common::File::cat_($in);
}
sub template2userfile {
    my ($prefix, $in, $out_rel, $force, %toreplace) = @_;

    foreach (list_skels($prefix, $out_rel)) {
	-d MDK::Common::File::dirname($_) or !-e $_ or $force or next;

	template2file($in, $_, %toreplace);
	m|/home/(.+?)/| and chown(getpwnam($1), getgrnam($1), $_);
    }
}
sub update_gnomekderc {
    my ($file, $category, %subst) = @_;

    output $file,
      (map {
	  my $l = $_;
	  s/^\s*//;
	  if (my $i = /^\[$category\]/i ... /^\[/) {
	      if ($i =~ /E/) { #- for last line of category
		  $l = join('', map_each { "$::a=$::b\n" } %subst) . $l;
		  %subst = ();
	      } elsif (/^(\w*?)=/) {
		  if (my $e = delete $subst{lc($1)}) {
		      $l = "$1=$e\n";
		  }
	      }
	  }
	  $l;
      } MDK::Common::File::cat_($file)),
	(%subst && "[$category]\n", map_each { "$::a=$::b\n" } %subst); #- if category has not been found above.
}

1;