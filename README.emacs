To use under emacs, simply add the following line to your .emacs, 
then when you visit a perl file, you can use Ctrl-Return to run perl_checker
on this file

  (global-set-key [(control return)] (lambda () (interactive) (save-some-buffers 1) (compile (concat "perl_checker --restrict-to-files " (buffer-file-name (current-buffer))))))

perl_checker --restrict-to-files scanner.pm > errors.err ; vim -c ':copen 4' -c ':so /usr/share/vim/ftplugin/perl_checker.vim' -q



/usr/share/vim/ftplugin/perl_checker.vim

" Error formats
setlocal efm=
  \%EFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,
  \%EFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,
  \%+EReference\ to\ unbound\ regexp\ name\ %m,
  \%Eocamlyacc:\ e\ -\ line\ %l\ of\ \"%f\"\\,\ %m,
  \%Wocamlyacc:\ w\ -\ %m,
  \%-Zmake%.%#,
  \%C%m
