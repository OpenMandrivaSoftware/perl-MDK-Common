package MDK::Common::File;

use vars qw(@ISA %EXPORT_TAGS @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(dirname basename cat_ cat__ output linkf symlinkf renamef touch all glob_ substInFile concat_symlink expand_symlinks openFileMaybeCompressed catMaybeCompressed);
%EXPORT_TAGS = (all => [ @EXPORT_OK ]);

sub dirname { local $_ = shift; s|[^/]*/*\s*$||; s|(.)/*$|$1|; $_ || '.' }
sub basename { local $_ = shift; s|/*\s*$||; s|.*/||; $_ }
sub cat_ { local *F; open F, $_[0] or $_[1] ? die "cat of file $_[0] failed: $!\n" : return; my @l = <F>; wantarray ? @l : join '', @l }
sub cat__ { my ($f) = @_; my @l = <$f>; wantarray ? @l : join '', @l }
sub output { my $f = shift; local *F; open F, ">$f" or die "output in file $f failed: $!\n"; print F foreach @_; }
sub linkf    { unlink $_[1]; link    $_[0], $_[1] }
sub symlinkf { unlink $_[1]; symlink $_[0], $_[1] }
sub renamef  { unlink $_[1]; rename  $_[0], $_[1] }


sub touch {
    my ($f) = @_;
    unless (-e $f) {
	local *F;
	open F, ">$f";
    }
    my $now = time;
    utime $now, $now, $f;
}


sub all {
    my $d = shift;

    local *F;
    opendir F, $d or return;
    my @l = grep { $_ ne '.' && $_ ne '..' } readdir F;
    closedir F;

    @l;
}

sub glob_ {
    my ($d, $f) = ($_[0] =~ /\*/) ? (dirname($_[0]), basename($_[0])) : ($_[0], '*');

    $d =~ /\*/ and die "glob_: wildcard in directory not handled ($_[0])\n";
    ($f = quotemeta $f) =~ s/\\\*/.*/g;

    $d =~ m|/$| or $d .= '/';
    map { $d eq './' ? $_ : "$d$_" } grep { /^$f$/ } all($d);
}


sub substInFile(&@) {
    my $f = shift;
    foreach my $file (@_) {
	if (-e $file) {
	    local @ARGV = $file;
	    local ($^I, $_) = '';
	    while (<>) { &$f($_); print }
	} else {
	    local *F; my $old = select F; # that way eof return true
	    local $_ = '';
	    &$f($_);
	    select $old;
	    eval { output($file, $_) };
	}
    }
}


sub concat_symlink {
    my ($f, $l) = @_;
    $l =~ m|^\.\./(/.*)| and return $1;

    $f =~ s|/$||;
    while ($l =~ s|^\.\./||) { 
	$f =~ s|/[^/]+$|| or die "concat_symlink: $f $l\n";
    }
    "$f/$l";
}
sub expand_symlinks {
    my ($first, @l) = split '/', $_[0];
    $first eq '' or die "expand_symlinks: $_[0] is relative\n";
    my ($f, $l);
    foreach (@l) {
	$f .= "/$_";
	$f = concat_symlink($f, "../$l") while $l = readlink $f;
    }
    $f;
}


sub openFileMaybeCompressed { 
    my ($f) = @_;
    -e $f || -e "$f.gz" or die "file $f not found";
    local *F;
    open F, -e $f ? $f : "gzip -dc $f.gz|" or die "file $f is not readable";
    *F;
}
sub catMaybeCompressed { cat__(openFileMaybeCompressed($_[0])) }

1;
