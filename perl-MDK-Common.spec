# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common

# do not change the version here, change in MDK/Common.pm.pl
%define version THEVERSION
%define release 2mdk
%define perl_sitelib %(eval "`perl -V:installsitelib`"; echo $installsitelib)

Summary: Various simple functions
Name: perl-MDK-Common
Version: %{version}
Release: %{release}
Source0: %{name}.tar.bz2
License: GPL
Group: Development/Perl
BuildRoot: %{_tmppath}/%{name}-buildroot
Requires: /usr/bin/perl

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
%{perl_sitelib}/MDK

%files devel
%defattr(-,root,root)
%doc index.html
%{_bindir}/*

# MODIFY IN THE CVS: cvs.mandrakesoft.com:/cooker soft/perl-MDK-Common
%changelog
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

