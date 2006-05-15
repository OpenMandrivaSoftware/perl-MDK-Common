# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common/

%define version 1.1.23
%define release %mkrel 1

%ifarch x86_64
%define build_option PERL_CHECKER_TARGET='debug-code BCSUFFIX=""'
%define require_ocaml /usr/bin/ocamlrun
%else
%define build_option %nil
%define require_ocaml %nil
%endif

Summary: Verify perl code
Name: perl_checker
Version: %{version}
Release: %{release}
License: GPL
Group: Development/Perl
Requires: perl-base >= 2:5.8.0 %{require_ocaml}
URL: http://cvs.mandriva.com/cgi-bin/cvsweb.cgi/soft/perl-MDK-Common/perl_checker.src
Source0: perl_checker-%version.tar.bz2
BuildRoot: %{_tmppath}/%{name}-buildroot
BuildRequires: ocaml >= 3.06
# for the faked packages:
AutoReqProv: 0

Obsoletes: perl-MDK-Common-devel <= 1.1.24
Provides: perl-MDK-Common-devel <= 1.1.24

%description
Various verifying scripts created for DrakX

%prep
%setup -q

%build
make %build_option

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall_std %build_option

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc perl_checker.src/perl_checker.html
%{_bindir}/*
%{perl_vendorlib}/perl_checker_fake_packages
%{_datadir}/vim/ftplugin/*
%config(noreplace) %{_sysconfdir}/emacs/site-start.d/*


# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common
%changelog
* Mon May 15 2006 Pixel <pixel@mandriva.com> 1.1.23-1mdk
- it seems stack is smaller on amd64, function concat_spaces need to be tail-recursive

* Tue Apr 11 2006 Thierry Vignaud <tvignaud@mandriva.com> 1.1.22-1mdk
- sync with Glib/Gtk2-1.120

* Fri Nov 25 2005 Pixel <pixel@mandriva.com> 1.1.21-1mdk
- renamed package from perl-MDK-Common-devel to perl_checker
- new option --generate-package-dependencies-graph 

* Fri Nov 26 2004 Pixel <pixel@mandrakesoft.com> 1.1.20-2mdk
- new checks

* Wed Nov 10 2004 Pixel <pixel@mandrakesoft.com> 1.1.19-1mdk
- various enhancements/fixes


* Wed Aug 18 2004 Pixel <pixel@mandrakesoft.com> 1.1.17-3mdk
- use DESTDIR
- add perl_checker-vim
- add Ctrl-return in perl and cperl emacs mode
- fake Getopt::Long

* Wed Aug 11 2004 Pixel <pixel@mandrakesoft.com> 1.1.17-2mdk
- various enhancements/fixes

* Fri Jul 23 2004 Pixel <pixel@mandrakesoft.com> 1.1.15-2mdk
- workaround bug in ocaml on ultrasparc 
  (can't catch exception "Fatal error: out-of-bound access in array or string" in native code)

* Mon May 10 2004 Pixel <pixel@mandrakesoft.com> 1.1.12-1mdk
- many enhancements and cleanup

* Wed Apr  7 2004 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.11-4mdk
- perl_checker:
  o add perl_checker.html
  o add testsuite
  o fix detecting of boolean context vs scalar context
  o fix some warning
  o in "$a ? $a : xxx", "xxx" can need short circuit
  o recognize "-c" function
  o turn some errors to warnings
- perl_checker's faked packages:
  o sync with glib/gtk+ 2.4.0
  o support Gnome2 and Gnome2::Vte too

* Fri Feb 13 2004 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.10-2mdk
- update gtk2-perl binding fake package

* Tue Jan 13 2004 Pixel <pixel@mandrakesoft.com> 1.1.11-1mdk
- sync perl_checker_fake_packages/{Glib,Gtk2}.pm
- fix build time overflow in cache

* Fri Jan  9 2004 Pixel <pixel@mandrakesoft.com> 1.1.10-2mdk
- entries in generated pot file are sorted by files

* Mon Jan  5 2004 Pixel <pixel@mandrakesoft.com> 1.1.9-1mdk
- many enhancements

* Tue Nov 18 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.8-3mdk
- perl_checker --generate-pot: unescape "$" & "@" caracters

* Mon Nov 10 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.8-2mdk
- fix path in po generated from sources

* Thu Aug 28 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.6-2mdk
- resync perl_checker fake packages with to perl-Gtk2-0.95-5mdk

* Mon Aug 11 2003 Pixel <pixel@mandrakesoft.com> 1.1.6-1mdk
- allow $_o_XXX parameter name which is both unused and optional (same for $_b_XXX)
- shift is a ONE_SCALAR_PARA so that $box->pack_start(shift @l, 0, 0, 4) is parsed correctly
- in arrange_global_vars_declared(), don't keep anything in global_vars_declared, better
  create shadow packages to contain them
- much better merging of multiple files defining functions in the same package.
  This fixes the bad behaviour when using the cache (esp. do_pkgs, but it was even worse
  with things in ugtk2.pm)
- adapt to perl-Gtk2 xs (which replace the perl-GTK2 inline version)

* Mon Jun 16 2003 Pixel <pixel@mandrakesoft.com> 1.1.4-2mdk
- no native perl_checker for x86_64, only bytecode
- build require ocaml >= 3.06 (thanks to Per Øyvind Karlsen)

* Tue May 27 2003 Pixel <pixel@mandrakesoft.com> 1.1.4-1mdk
- many enhancements:
  - disallow return(...), prefering return ...
  - enhance restricted_subscripted to correctly handle -e foo::bar()->{boo}
  - handle  use foo()  and  use foo ("x", "y")
  - better warning for:  print $a . 'foo'
  - add a special case to handle "arch => 1" without going through word_alone()
  - warn things like:  if ($a = 1) { ... }  or  0 or ...
  - explicitly disallow <<=, >>= and **= (instead of having a syntax error)
  - check prototype coherence: disallow  ($a, @b, $c)  or  ($a, $o_b, $c)
  - warn spurious space in ( 1, 2) which should be (1, 2)
  - warn $o->method() which should be $o->method
  - suggest using the functional map instead of the imperative foreach when possible
  - add warning: you can replace "map { if_(..., $_) }" with "grep { ... }"
  - suggest any instead of grep in scalar context
  - suggest foreach instead of map in empty context
  - fix "/^\d+\.\*$/" giving warning "you can remove \".*$\" at the end of your regexp"

* Fri May 16 2003 Pixel <pixel@mandrakesoft.com> 1.1.3-1mdk
- fix pot generation (have \" instead of \\\")

* Tue Apr 29 2003 Pixel <pixel@mandrakesoft.com> 1.1.2-1mdk
- more context checks
  - ensure the values are used (eg: "map { ... } ...", "/xxx/")
  - ensure the values "... or ...", "... and ..." are not used

* Fri Apr 25 2003 Pixel <pixel@mandrakesoft.com> 1.1.1-1mdk
- enhanced "number of arguments" checking, including method calls

* Thu Apr 17 2003 Pixel <pixel@mandrakesoft.com> 1.1.0-1mdk
- basic "number of arguments" checking

* Fri Apr 11 2003 Pixel <pixel@mandrakesoft.com> 1.0.5-1mdk
- many enhancements:
  - allow 333 * `xxx` with no warning
  - warn non-useful or non-readable escaped sequences in strings and regexps
    (eg: /^\// should be m|^/|, /xxx\=xxx/ should be /xxx=xxx/ ...)
  - warn things like: ($foo) ||= ...
  - enhance non_scalar case for some operators using is_not_a_scalar
  - handle "keys %pkg::" (twas broken because keys() is now a ONE_SCALAR_PARA)
  - keys() is a ONE_SCALAR_PARA
  - correctly (in Perl way) handle priority for some special unary functions (length, exists, ref)
  - warn xxx == "ia64", xxx eq 2
  - 0.2 is a NUM, not a REVISION (otherwise it gets into a Raw_string)
  - better error message ("please remove the space before the function call"
    instead of "can't handle this nicely")
  - warn when using a regexp terminated with .* or .*$ (which is useless) 
  - allow to selectively import from @EXPORT instead of only accepting @EXPORT_OK

* Mon Feb 24 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-23mdk
- have the POT-Creation-Date set to the current date (when --generate-pot)
- various fixes

* Fri Feb 14 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-21mdk
- don't suggest to replace "@foo ? @foo : @bar" with "@foo || @bar", this is wrong!

* Thu Feb 13 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-20mdk
- add some more Gtk2 methods
- check use of variables with name _XXX (reserved for unused variables)

* Wed Feb 12 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-19mdk
- handle ${foo} (including "${foo}bar")
- warn when "ref" priority is badly handled by perl_checker

* Thu Feb  6 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-18mdk
- add various Gtk2 methods
- handle "...\x{hex}..."
- suggest replacing $l[$#l] with $l[-1]

* Tue Jan 21 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-16mdk
- add some Gtk2 methods

* Thu Jan 16 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-15mdk
- 
  - check occurences of "$foo ? $foo : $bar"
  - disallow "fq::f args" when args is not parenthesized

* Wed Jan 15 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-14mdk
- when generating pot, add an header and fake line numbers to
  please msgmerge

* Sat Dec 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-12mdk
- add some more Gtk2 functions

* Wed Dec 18 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-11mdk
- many new features including 
  - checking methods being available 
  - checking unused functions
  - saving parsed file in .perl_checker.cache
  - new instruction "Basedir .." in .perl_checker (useful for gi/perl-install/standalone/.perl_checker)

* Wed Dec 11 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-10mdk
- add option "-t" enabling titi to precise tab-width=4
- fix a bug in getting exported functions (fixes "unknown function gtkshow")

* Tue Dec 10 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-9mdk
- check the c-format conformity of translated strings

* Tue Dec 10 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-8mdk
- new --generate-pot feature

* Fri Dec  6 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-7mdk
- print on stdout, not stderr
- add option --restrict-to-files (mainly for perl_checko the Clean Keeper)

* Fri Dec  6 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-6mdk
- perl_checker now checks usage of $_
- ignore unknown functions coming from XS bootstrap when we can't use the .c
  to know the list of functions provided by the XS extension

* Wed Dec  4 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-5mdk
- add unused variable detection
- allow $AUTOLOAD usage in AUTOLOAD()
- handle "use lib qw(...)"

* Wed Dec  4 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-4mdk
- warn use of "cond ? list : ()" (use if_(cond, list) instead)

* Mon Dec  2 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-3mdk
- some more checks ($1 =~ /re/ is a warning)

* Thu Nov 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-2mdk
- new perl_checker now has every feature of the old version 
  (except checking $_ in small subs, a more global solution should come)

* Wed Nov 13 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-1mdk
- new perl_checker written in OCaml (not as featured as previous perl_checker yet)

* Thu Nov  7 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-18mdk
- many more warnings
  - warn unneeded parentheses after an infix foreach/if/unless
  - error when "unless" is used with complex expressions
  - force $_ to be localised when "while (<FILEHANDLE>)" is used
  - force FILEHANDLE to be localised when "open FILEHANDLE, ..." is used
  - warn about one-character long functions (esp. for &N and &_)
  - warn when N("...") is misused

* Thu Oct 17 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-17mdk
- add a check for function call PKG::f instead of PKG::f()
- ensure a missing "=cut" doesn't make perl_checker go crazy (eg: when titi adds some doc)

* Wed Aug 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-14mdk
- no function "xxx undefined" when using "#-#"

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-11mdk
- a few more stricter rules

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-10mdk
- cleaner, more usable (via .perl_checker for -exclude's)
- more stricter syntax rules

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-9mdk
- add *much* stricter syntax rules

* Tue Jul 23 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-5mdk
- catch misuse of =~ when = was meant

* Wed Jul 17 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-4mdk
- add new checks
- exclude Date::Manip

* Tue Feb 19 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-13mdk
- skip s///

* Sat Feb 16 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-11mdk
- don't fail on non-tagged import
