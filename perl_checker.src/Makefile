# OCAMLC = ocamlcp -p a
OCAMLBCFLAGS = -w A
YFLAGS = -v
TRASH = parser.output
RESULT  = perl_checker
BCSUFFIX = _debug
SOURCES = common.ml types.mli info.ml parser_helper.ml parser.mly lexer.mll perl_checker.ml
LIBS = unix

NAME = shyant

default: debug-code native-code

-include OCamlMakefile