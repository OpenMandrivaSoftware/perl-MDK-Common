RPM ?= $(HOME)/rpm

PREFIX = /usr
BINDIR = $(PREFIX)/bin
VENDORLIB = $(shell eval "`perl -V:installvendorlib`"; echo $$installvendorlib)
INSTALLVENDORLIB = $(DESTDIR)$(VENDORLIB)
PERL_CHECKER_TARGET = native-code
PERL_CHECKER_VERSION = 1.1.22

FILES-perl_checker = AUTHORS COPYING README.emacs Makefile misc perl_checker.spec perl_checker.src perl_checker_fake_packages

.PHONY: perl_checker.src

all: perl_checker.src/perl_checker test

MDK/Common.pm: %: %.pl
	perl $< > $@

perl_checker.src/perl_checker:
	$(MAKE) -C perl_checker.src build_ml perl_checker.html $(PERL_CHECKER_TARGET) VENDORLIB=$(VENDORLIB) DEBUG=0

test: perl_checker.src/perl_checker
	$(MAKE) -C perl_checker.src/test

clean:
	rm -f Makefile-MDK-Common MDK/Common.pm perl_checker.src/perl_checker *.tar.* META.yml .perl_checker.cache lib
	$(MAKE) -C perl_checker.src clean
	find -name "*~" | xargs rm -rf

install: clean all
	$(MAKE) -C misc install
	install perl_checker.src/perl_checker $(DESTDIR)$(BINDIR)
	install -d $(INSTALLVENDORLIB)
	tar c `find perl_checker_fake_packages -name "*.pm"` | tar xC $(INSTALLVENDORLIB)

update:
	cvs update

commit:
	cvs commit

tar: clean
	mkdir -p perl_checker-$(PERL_CHECKER_VERSION)
	tar c --exclude CVS $(FILES-perl_checker) | tar xC perl_checker-$(PERL_CHECKER_VERSION)
	tar cfj perl_checker-$(PERL_CHECKER_VERSION).tar.bz2 perl_checker-$(PERL_CHECKER_VERSION)
	rm -rf perl_checker-$(PERL_CHECKER_VERSION)

srpm: tar
	cp -f perl_checker*.tar.* $(RPM)/SOURCES
	cat perl_checker.spec > $(RPM)/SPECS/perl_checker.spec
	-rpmbuild -bs $(RPM)/SPECS/perl_checker.spec

rpm: update srpm
	-rpmbuild -bb $(RPM)/SPECS/perl_checker.spec


Makefile-MDK-Common:
	MAKEFILE_NAME=Makefile-MDK-Common perl Makefile.PL

tar-MDK-Common: clean Makefile-MDK-Common
	$(MAKE) -f Makefile-MDK-Common dist

srpm-MDK-Common: update tar-MDK-Common
	cp -f MDK-Common*.tar.* $(RPM)/SOURCES
	perl -I. -MMDK::Common -pe 's/THEVERSION/$$MDK::Common::VERSION/' perl-MDK-Common.spec > $(RPM)/SPECS/perl-MDK-Common.spec
	-rpmbuild -bs $(RPM)/SPECS/perl-MDK-Common.spec

rpm-MDK-Common: srpm-MDK-Common
	-rpmbuild -bb $(RPM)/SPECS/perl-MDK-Common.spec
