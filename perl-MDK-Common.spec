# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common

# do not change the version here, change in MDK/Common.pm.pl
%define version THEVERSION
%define release 7mdk

Summary: Various simple functions
Name: perl-MDK-Common
Version: %{version}
Release: %{release}
Source0: %{name}.tar.bz2
License: GPL
Group: Development/Perl
BuildRoot: %{_tmppath}/%{name}-buildroot
BuildArch: noarch

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

# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common
%changelog
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

