NAME = perl-MDK-Common
TAR = $(NAME).tar.bz2
RPM ?= $(HOME)/rpm

PREFIX = /usr
BINDIR = $(PREFIX)/bin
VENDORLIB = $(shell eval "`perl -V:installvendorlib`"; echo $$installvendorlib)
INSTALLVENDORLIB = $(shell echo $(VENDORLIB) | sed 's,/usr,$(PREFIX),')
PERL_CHECKER_TARGET = native-code

GENERATED = MDK/Common.pm index.html perl_checker.src/perl_checker

.PHONY: perl_checker.src

all: $(GENERATED) test

index.html: MDK/Common.pm
	pod2html $< > $@
	rm -f pod2htm*.tmp

MDK/Common.pm: %: %.pl
	perl $< > $@

perl_checker.src/perl_checker:
	$(MAKE) -C perl_checker.src build_ml perl_checker.html $(PERL_CHECKER_TARGET) VENDORLIB=$(VENDORLIB) DEBUG=0

test: perl_checker.src/perl_checker
	perl_checker.src/perl_checker MDK/Common/*.pm
	$(MAKE) -C perl_checker.src/test

clean:
	rm -f $(GENERATED)
	$(MAKE) -C perl_checker.src clean
	find -name "*~" | xargs rm -rf

install: clean all
	install -d $(BINDIR) $(INSTALLVENDORLIB)/MDK/Common
	install perl_checker.src/perl_checker $(BINDIR)
	install -m 644 MDK/Common.pm $(INSTALLVENDORLIB)/MDK
	install -m 644 MDK/Common/*.pm $(INSTALLVENDORLIB)/MDK/Common
	tar c `find perl_checker_fake_packages -name "*.pm"` | tar xC $(INSTALLVENDORLIB)

rpm: srpm
	-rpmbuild -bb $(RPM)/SPECS/$(NAME).spec
	rm -f ../$(TAR)


update:
	cvs update

commit:
	cvs commit

tar: clean
	cd .. ; tar cf - $(NAME) | bzip2 -9 >$(TAR)

srpm: update tar MDK/Common.pm
	cp -f ../$(TAR) $(RPM)/SOURCES
	perl -I. -MMDK::Common -pe 's/THEVERSION/$$MDK::Common::VERSION/' $(NAME).spec > $(RPM)/SPECS/$(NAME).spec
	-rpmbuild -bs $(RPM)/SPECS/$(NAME).spec
