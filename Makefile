NAME = perl-MDK-Common
TAR = $(NAME).tar.bz2

PREFIX = /usr
BINDIR = $(PREFIX)/bin
INSTALLSITEARCH = $(shell eval "`perl -V:installsitelib`"; echo $$installsitelib | sed 's,/usr,$(PREFIX),')

test:
	./perl_checker MDK/Common/*.pm

clean:
	find -name "*~" | xargs rm -rf

install: clean test
	install -d $(BINDIR) $(INSTALLSITEARCH)
	install perl_checker $(BINDIR)
	cp -r MDK $(INSTALLSITEARCH)

rpm: update tar build commit


update:
#	cvs update

commit:
	cvs commit

tar: clean
	cd .. ; tar cf - $(NAME) | bzip2 -9 >$(TAR)

build:
	cp -f ../$(TAR) $(RPM)/SOURCES
	perl -MMDK::Common -pe 's/THEVERSION/$$MDK::Common::VERSION/' $(NAME).spec > $(RPM)/SPECS/$(NAME).spec
	-rpm -ba $(NAME).spec
	rm -f ../$(TAR)
