# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common

# do not change the version here, change in MDK/Common.pm.pl
%define version THEVERSION
%define release 15mdk

Summary: Various simple functions
Name: perl-MDK-Common
Version: %{version}
Release: %{release}
URL: http://cvs.mandrakesoft.com/cgi-bin/cvsweb.cgi/soft/perl-MDK-Common/
Source0: %{name}.tar.bz2
License: GPL
Group: Development/Perl
BuildRoot: %{_tmppath}/%{name}-buildroot
BuildRequires: ocaml

%package devel
Summary: Various verifying scripts
Group: Development/Perl

%description
Various simple functions created for DrakX

%description devel
Various verifying scripts created for DrakX

%prep
%setup -n %{name}

%build
make test

%install
rm -rf $RPM_BUILD_ROOT
make install PREFIX="$RPM_BUILD_ROOT/usr"

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYING
%{perl_vendorlib}/MDK

%files devel
%defattr(-,root,root)
%doc index.html
%{_bindir}/*
%{perl_vendorlib}/perl_checker_fake_packages

# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common
%changelog
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

