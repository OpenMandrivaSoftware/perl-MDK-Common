PREFIX = /usr
DATADIR = $(PREFIX)/share

install:
	install -D -m 644 perl_checker.vim $(DESTDIR)$(DATADIR)/vim/ftplugin/perl_checker.vim
	install -D -m 644 perl_checker.el $(DESTDIR)/etc/emacs/site-start.d/perl_checker.el
	install -D perl_checker-vim $(DESTDIR)$(PREFIX)/bin/perl_checker-vim
