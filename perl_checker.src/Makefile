# OCAMLC = ocamlcp -p a
OCAMLBCFLAGS = -w A
YFLAGS = -v
TRASH = parser.output TAGS
RESULT  = perl_checker
BCSUFFIX = _debug
SOURCES = common.ml flags.ml types.mli info.ml parser_helper.ml parser.mly lexer.mll tree.ml perl_checker.ml
LIBS = unix

NAME = shyant

default: TAGS debug-code native-code

tags: 
	ocamltags *.ml

TAGS: 
	ocamltags *.ml

-include OCamlMakefile
