# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common

# do not change the version here, change in MDK/Common.pm.pl
%define version THEVERSION
%define release 2mdk

%ifarch x86_64
%define build_option PERL_CHECKER_TARGET='debug-code BCSUFFIX=""'
%define require_ocaml /usr/bin/ocamlrun
%else
%define build_option %nil
%define require_ocaml %nil
%endif

Summary: Various simple functions
Name: perl-MDK-Common
Version: %{version}
Release: %{release}
URL: http://cvs.mandrakesoft.com/cgi-bin/cvsweb.cgi/soft/perl-MDK-Common/
Source0: %{name}.tar.bz2
License: GPL
Group: Development/Perl
Conflicts: drakxtools-newt < 9.1-30mdk, drakconf < 9.1-14mdk
BuildRoot: %{_tmppath}/%{name}-buildroot
BuildRequires: ocaml >= 3.06
Provides: perl(MDK::Common) = %{version}
Provides: perl(MDK::Common::DataStructure)  
Provides: perl(MDK::Common::File)  
Provides: perl(MDK::Common::Func)  
Provides: perl(MDK::Common::Globals)  
Provides: perl(MDK::Common::Math)  
Provides: perl(MDK::Common::String)  
Provides: perl(MDK::Common::System)  
Provides: perl(MDK::Common::Various)  


%package devel
Summary: Various verifying scripts
Group: Development/Perl
AutoReqProv: 0
Requires: perl-base >= 2:5.8.0 %{require_ocaml}

%description
Various simple functions created for DrakX

%description devel
Various verifying scripts created for DrakX

%prep
%setup -n %{name}

%build
make test %build_option

%install
rm -rf $RPM_BUILD_ROOT
make install PREFIX="$RPM_BUILD_ROOT/usr" %build_option

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYING
%{perl_vendorlib}/MDK

%files devel
%defattr(-,root,root)
%doc index.html tutorial.html
%{_bindir}/*
%{perl_vendorlib}/perl_checker_fake_packages

# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common
%changelog
* Fri Jan  9 2004 Pixel <pixel@mandrakesoft.com> 1.1.10-2mdk
- perl_checker: entries in generated pot file are sorted by files

* Wed Jan  7 2004 Pixel <pixel@mandrakesoft.com> 1.1.10-1mdk
- add whereis_binary()

* Mon Jan  5 2004 Pixel <pixel@mandrakesoft.com> 1.1.9-1mdk
- many perl_checker enhancements

* Tue Dec 16 2003 Pixel <pixel@mandrakesoft.com> 1.1.8-4mdk
- MDK::Common::File::cp_f() added

* Tue Nov 18 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.8-3mdk
- perl_checker --generate-pot: unescape "$" & "@" caracters
- substInFile: if file is a symlink, make sure it stays a symlink

* Mon Nov 10 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.8-2mdk
- fix path in po generated from sources

* Wed Oct 15 2003 Guillaume Cottenceau <gc@mandrakesoft.com> 1.1.8-1mdk
- add uniq_ (uniq but according to some code results on each value)

* Fri Sep 19 2003 Pixel <pixel@mandrakesoft.com> 1.1.7-1mdk
- read_gnomekderc() & update_gnomekderc() will now handle key=value where key
can contain spaces
- export cat_or_die()

* Mon Sep  1 2003 Pixel <pixel@mandrakesoft.com> 1.1.6-3mdk
- MDK::Common::System::list_users() should list user 500 if it exists

* Thu Aug 28 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.1.6-2mdk
- resync perl_checker with to perl-Gtk2-0.95-5mdk

* Mon Aug 11 2003 Pixel <pixel@mandrakesoft.com> 1.1.6-1mdk
- perl_checker:
  - allow $_o_XXX parameter name which is both unused and optional (same for $_b_XXX)
  - shift is a ONE_SCALAR_PARA so that $box->pack_start(shift @l, 0, 0, 4) is parsed correctly
  - in arrange_global_vars_declared(), don't keep anything in global_vars_declared, better
    create shadow packages to contain them
  - much better merging of multiple files defining functions in the same package.
    This fixes the bad behaviour when using the cache (esp. do_pkgs, but it was even worse
    with things in ugtk2.pm)
  - adapt to perl-Gtk2 xs (which replace the perl-GTK2 inline version)

* Fri Aug  1 2003 Pixel <pixel@mandrakesoft.com> 1.1.5-2mdk
- rebuild for new perl (it helps DrakX build script)

* Wed Jul 30 2003 Pixel <pixel@mandrakesoft.com> 1.1.5-1mdk
- add read_gnomekderc() (and make update_gnomekderc() a little more robust when the category is plain weird)

* Mon Jun 16 2003 Pixel <pixel@mandrakesoft.com> 1.1.4-2mdk
- no native perl_checker for x86_64, only bytecode
- build require ocaml >= 3.06 (thanks to Per Øyvind Karlsen)

* Tue May 27 2003 Pixel <pixel@mandrakesoft.com> 1.1.4-1mdk
- many perl_checker enhancements:
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

* Mon May 12 2003 Pixel <pixel@mandrakesoft.com> 1.1.2-2mdk
- rebuild for perl auto-provides
  (except for perl-MDK-Common-devel which need special handling for the faked packages)

* Tue Apr 29 2003 Pixel <pixel@mandrakesoft.com> 1.1.2-1mdk
- perl_checker: more context checks
  - ensure the values are used (eg: "map { ... } ...", "/xxx/")
  - ensure the values "... or ...", "... and ..." are not used

* Fri Apr 25 2003 Pixel <pixel@mandrakesoft.com> 1.1.1-1mdk
- perl_checker: enhanced "number of arguments" checking, including method calls

* Fri Apr 18 2003 Guillaume Cottenceau <gc@mandrakesoft.com> 1.1.0-2mdk
- add the tutorial to the -devel package

* Thu Apr 17 2003 Pixel <pixel@mandrakesoft.com> 1.1.0-1mdk
- MDK::Common::Func: map_index, each_index and grep_index do not pass $::i as
a parameter anymore (this breaks backward compatibility, but it is cleaner and
otherwise perl_checker doesn't handle it correctly)
- basic "number of arguments" checking

* Fri Apr 11 2003 Pixel <pixel@mandrakesoft.com> 1.0.5-1mdk
- many perl_checker enhancements:
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

* Thu Feb 20 2003 Guillaume Cottenceau <gc@mandrakesoft.com> 1.0.4-22mdk
- snapshot (including formatError suitable for die \n() in DrakX)

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

* Wed Jan 29 2003 Thierry Vignaud <tvignaud@mandrakesoft.com> 1.0.4-17mdk
- add list_users()

* Tue Jan 21 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-16mdk
- perl_checker: add some Gtk2 methods

* Thu Jan 16 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-15mdk
- perl_checker: 
  - check occurences of "$foo ? $foo : $bar"
  - disallow "fq::f args" when args is not parenthesized

* Wed Jan 15 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-14mdk
- perl_checker: when generating pot, add an header and fake line numbers to
  please msgmerge

* Mon Jan  6 2003 Pixel <pixel@mandrakesoft.com> 1.0.4-13mdk
- MDK::Common::Func: add "find", "any" and "every"

* Sat Dec 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-12mdk
- perl_checker: add some more Gtk2 functions
- MDK::Common::File: mkdir_p, rm_rf and cp_af returns 1 on success 
  (allowing "eval { mkdir_p() } or ...")

* Wed Dec 18 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-11mdk
- perl_checker: many new features including 
  - checking methods being available 
  - checking unused functions
  - saving parsed file in .perl_checker.cache
  - new instruction "Basedir .." in .perl_checker (useful for gi/perl-install/standalone/.perl_checker)

* Wed Dec 11 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-10mdk
- perl_checker: add option "-t" enabling titi to precise tab-width=4
- perl_checker: fix a bug in getting exported functions (fixes "unknown function gtkshow")

* Tue Dec 10 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-9mdk
- perl_checker: check the c-format conformity of translated strings

* Tue Dec 10 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-8mdk
- perl_checker: new --generate-pot feature

* Fri Dec  6 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-7mdk
- perl_checker: print on stdout, not stderr
- perl_checker: add option --restrict-to-files (mainly for perl_checko the Clean Keeper)

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
- add output_with_perm(), cat_or_die()
- some more checks in perl_checker ($1 =~ /re/ is a warning)

* Thu Nov 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-2mdk
- new perl_checker now has every feature of the old version 
  (except checking $_ in small subs, a more global solution should come)

* Wed Nov 13 2002 Pixel <pixel@mandrakesoft.com> 1.0.4-1mdk
- new perl_checker written in OCaml (not as featured as previous perl_checker yet)
- MDK::* made perl_checker compliant

* Thu Nov  7 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-18mdk
- perl_checker: many more warnings
  - warn unneeded parentheses after an infix foreach/if/unless
  - error when "unless" is used with complex expressions
  - force $_ to be localised when "while (<FILEHANDLE>)" is used
  - force FILEHANDLE to be localised when "open FILEHANDLE, ..." is used
  - warn about one-character long functions (esp. for &N and &_)
  - warn when N("...") is misused

* Thu Oct 17 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-17mdk
- add a check for function call PKG::f instead of PKG::f()
- ensure a missing "=cut" doesn't make perl_checker go crazy (eg: when titi adds some doc)

* Fri Sep  6 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-16mdk
- MDK::Common::System::update_gnomekderc: fix adding lines to the last section when it doesn't end with a cr

* Fri Sep  6 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-15mdk
- MDK::Common::System::update_gnomekderc: fix adding section when the file doesn't end with a cr

* Wed Aug 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-14mdk
- no function "xxx undefined" when using "#-#"

* Tue Aug 27 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-13mdk
- give a meaning to the return value of cdie
- fix typo in mkdir_p error message

* Mon Aug 12 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-12mdk
- add setExportedVarsInSh and setExportedVarsInCsh
- remove setVarsInCsh (obsoleted by setExportedVarsInCsh)

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-11mdk
- File.pm: add "append_to_file"
- perl_checker: a few more stricter rules

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-10mdk
- perl_checker: cleaner, more usable (via .perl_checker for -exclude's)
- perl_checker: more stricter syntax rules
- adapt *.pm's to those rules

* Wed Jul 31 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-9mdk
- perl_checker: add *much* stricter syntax rules
- adapt *.pm's to those rules

* Sun Jul 28 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-8mdk
- MDK::Common::DataStructure: add sort_numbers

* Thu Jul 25 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-7mdk
- add Various::internal_error
- export Various::noreturn

* Tue Jul 23 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-6mdk
- MDK::Common::System: add fuzzy_pidofs

* Tue Jul 23 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-5mdk
- perl_checker: catch misuse of =~ when = was meant
- MDK/Common/DataStructure.pm: add deref_array

* Wed Jul 17 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-4mdk
- perl_checker: add new checks
- perl_checker: exclude Date::Manip

* Tue Jul  9 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-3mdk
- workaround perl 5.8.0-RC2 bug

* Tue Jul  9 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-2mdk
- rebuild for perl 5.8.0

* Wed Jul  3 2002 Pixel <pixel@mandrakesoft.com> 1.0.3-1mdk
- MDK/Common/Func.pm: add "partition"

* Tue Feb 19 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-13mdk
- perl_checker: skip s///

* Sat Feb 16 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-12mdk
- MDK/Common/System.pm (update_gnomekderc): rework it, make it work in all possible case

* Sat Feb 16 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-11mdk
- MDK/Common/System.pm: fix call to "output" in "template2file" and "update_gnomekderc"
- perl-checker: don't fail on non-tagged import

* Thu Feb 14 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-10mdk
- warp_text returns a join'ed string in scalar context

* Sun Jan 27 2002 Pixel <pixel@mandrakesoft.com> 1.0.2-9mdk
- add MDK::Common::DataStructure::group_by2

* Thu Dec 20 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-8mdk
- add Various::noreturn()

* Mon Sep 17 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-7mdk
- (cp_af): fix typo

* Sun Sep 16 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-6mdk
- add output_p, cp_af, rm_rf

* Sun Sep 16 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-5mdk
- add mkdir_p

* Mon Sep 10 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-4mdk
- DataStructure::uniq : keep the order
- String::warp_text : fixed

* Thu Sep  6 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-3mdk
- substInFile works on empty files

* Mon Aug 27 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-2mdk
- create perl-MDK-Common-devel
- fix warp_text

* Thu Aug  9 2001 Pixel <pixel@mandrakesoft.com> 1.0.2-1mdk
- each_index added
- a few more checks in perl_checker

* Sat Aug  4 2001 Pixel <pixel@mandrakesoft.com> 1.0.1-1mdk
- add some arch() stuff

* Fri Aug  3 2001 Pixel <pixel@mandrakesoft.com> 1.0-1mdk
- doc finished
- index.html added (nicer than perldoc)

* Fri Aug  3 2001 Pixel <pixel@mandrakesoft.com> 1.0-0.3mdk
- much doc added

* Wed Jul 25 2001 Pixel <pixel@mandrakesoft.com> 1.0-0.2mdk
- another pre-release: some doc added, some fixes

* Tue Jul 24 2001 Pixel <pixel@mandrakesoft.com> 1.0-0.1mdk
- first version

