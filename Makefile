NAME = perl-MDK-Common
TAR = $(NAME).tar.bz2

PREFIX = /usr
BINDIR = $(PREFIX)/bin
INSTALLVENDORLIB = $(shell eval "`perl -V:installvendorlib`"; echo $$installvendorlib | sed 's,/usr,$(PREFIX),')

GENERATED = MDK/Common.pm index.html

.PHONY: perl_checker.src

all: $(GENERATED) test

index.html: MDK/Common.pm
	pod2html $< > $@

MDK/Common.pm: %: %.pl
	perl $< > $@

perl_checker.src:
	$(MAKE) -C $@

test: perl_checker.src
	perl_checker.src/perl_checker MDK/Common/*.pm

clean:
	rm -f $(GENERATED)
	$(MAKE) -C perl_checker.src clean
	find -name "*~" | xargs rm -rf

install: clean all
	install -d $(BINDIR) $(INSTALLVENDORLIB)/MDK/Common
	install perl_checker.src/perl_checker $(BINDIR)
	install -m 644 MDK/Common.pm $(INSTALLVENDORLIB)/MDK
	install -m 644 MDK/Common/*.pm $(INSTALLVENDORLIB)/MDK/Common

rpm: update tar build commit


update:
	cvs update

commit:
	cvs commit

tar: clean
	cd .. ; tar cf - $(NAME) | bzip2 -9 >$(TAR)

build: MDK/Common.pm
	cp -f ../$(TAR) $(RPM)/SOURCES
	perl -I. -MMDK::Common -pe 's/THEVERSION/$$MDK::Common::VERSION/' $(NAME).spec > $(RPM)/SPECS/$(NAME).spec
	-rpm -ba $(RPM)/SPECS/$(NAME).spec
	rm -f ../$(TAR)
