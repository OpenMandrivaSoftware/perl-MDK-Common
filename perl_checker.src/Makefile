# OCAMLC = ocamlcp -p a
OCAMLBCFLAGS = -w A -w e
YFLAGS = -v
TRASH = parser.output TAGS
RESULT  = perl_checker
BCSUFFIX = _debug
SOURCES = build.ml common.ml flags.ml config_file.ml types.mli info.ml parser_helper.ml parser.mly lexer.mll tree.ml global_checks.ml perl_checker.ml
LIBS = unix
VENDORLIB = $(shell dirname `pwd`)
DEBUG = 1

default: TAGS build_ml build.ml debug-code native-code perl_checker.html

build_ml:
	rm -f build.ml
	$(MAKE) build.ml

build.ml:
	date '+let date = %s' > $@
	echo 'let fake_packages_dir = "'$(VENDORLIB)'/perl_checker_fake_packages"' >> $@
	echo 'let debugging = $(DEBUG) > 0' >> $@

%.html: %.html.pl
	rm -f $@
	perl $< > $@
	chmod a-w $@

tags: 
	ocamltags *.ml

TAGS: 
	ocamltags *.ml

-include OCamlMakefile
