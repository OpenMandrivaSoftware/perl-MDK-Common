CSLC = ocamlcp -p a
CSLC = ocamlc
CSLOPT = ocamlopt
CSLDEP = ocamldep
CSLLEX = ocamllex
CSLYACC = ocamlyacc -v
CSLFLAGS = -w A -g
CSLOPTFLAGS =

LEX_FILES = $(wildcard *.mll)
YACC_FILES = $(wildcard *.mly)
TMP_FILES =  $(YACC_FILES:%.mly=%.mli) $(YACC_FILES:%.mly=%.output) $(YACC_FILES:%.mly=%.ml) $(LEX_FILES:%.mll=%.ml)

ALL_PROGS = perl_checker_debug perl_checker

PROG_OBJS_WITH_CMI = parser_helper.cmo parser.cmo print.cmo perl_checker.cmo
PROG_OBJS = common.cmo flags.cmo info.cmo $(LEX_FILES:%.mll=%.cmo) $(PROG_OBJS_WITH_CMI)
CMA_FILES = unix.cma

PROG_OBJX_WITH_CMI = $(PROG_OBJS_WITH_CMI:%.cmo=%.cmx)
PROG_OBJX = $(PROG_OBJS:%.cmo=%.cmx)
CMXA_FILES = $(CMA_FILES:%.cma=%.cmxa)

.PHONY: depend tags clean 

default: .compiling TAGS $(ALL_PROGS)
	rm -f .compiling

all: perl_checker

perl_checker_debug: .depend $(PROG_OBJS)
	$(CSLC) -custom $(CSLFLAGS) $(LIBDIRS) -o $@ $(CMA_FILES) $(PROG_OBJS)

perl_checker: .depend $(PROG_OBJX)
	$(CSLOPT) $(CSLOPTFLAGS) $(LIBDIRS) -o $@ $(CMXA_FILES) $(PROG_OBJX)

.compiling:
	touch $@

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

%.ml: %.mll types.cmi parser.cmi
	$(CSLLEX) $<

%.mli %.ml: %.mly
	$(CSLYACC) $<

$(PROG_OBJS_WITH_CMI): %.cmo: %.cmi

$(PROG_OBJX_WITH_CMI): %.cmx: %.cmi 

%.cmo: %.ml
	$(CSLC) $(CSLFLAGS) -c $<

%.cmx: %.ml
	$(CSLOPT) $(CSLOPTFLAGS) -c $<

.mli.cmi:
	$(CSLC) $(CSLFLAGS) -c $<

clean:
	rm -f $(ALL_PROGS) *~ *.o *.cm[iox] $(TMP_FILES) .depend .compiling TAGS gmon.out ocamlprof.dump

tags: 
	ocamltags *.ml

TAGS:
	ocamltags *.ml

# Dependencies
depend: .depend
.depend:
	$(CSLDEP) $(INCLUDES) *.mli *.mll *.ml > .depend

# missing dependencies:
perl_checker.cmo: lexer.cmi parser.cmi
perl_checker.cmx: lexer.cmi parser.cmi
lexer.cmx: common.cmi parser.cmi
lexer.cmo: common.cmi parser.cmi
parser.cmo: parser_helper.cmi
parser.cmx: parser_helper.cmi

-include .depend
