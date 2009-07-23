ARCHIVE := $(shell grep "archive(byte)" ../META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")
XARCHIVE := $(shell grep "archive(native)" ../META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")

include ../Makefile.conf

PP	   = #-pp "camlp4o pa_macro.cmo"

INCLUDE		= -I ..
OCAMLC_FLAGS	= -g -dtypes $(INCLUDE) $(ARCHIVE)
OCAMLOPT_FLAGS := -inline 3 $(INCLUDE) $(XARCHIVE)

OCAMLDOC_GEN = -html
OCAMLDOC_FLAGS := $(OCAMLDOC_GEN)

TESTS=test.ml

default: all

######################################################################

ML_FILES  := $(wildcard *.ml)
MLI_FILES := $(wildcard *.mli)
CMI_FILES := $(addsuffix .cmi,$(basename $(MLI_FILES)))

BYTE_OBJS := $(if $(ML_FILES),$(PACKAGE).cmo $(BYTE_OBJS),)
OPT_OBJS  := $(if $(ML_FILES),$(PACKAGE).cmx $(OPT_OBJS),)

#DOCFILES  += $(ML_FILES) $(MLI_FILES)
PUBFILES  += $(DOCFILES) README

PKGS = $(shell grep "requires" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGS_CMA  += $(addsuffix .cma, $(PKGS))
PKGS_CMXA += $(addsuffix .cmxa, $(PKGS))

export OCAMLPATH = ..

.PHONY: all opt byte ex
all: $(CMI_FILES) byte opt
byte: $(TESTS:.ml=.exe)
opt: $(TESTS:.ml=.com)


TAGS: $(ML_FILES)
	$(OCAMLTAGS) $^

# Make.bat -- easy compilation on win32
Make.bat:
	$(MAKE) clean
#	Filter out all "make" messages
	$(MAKE) all | grep --invert-match "make" > $@

# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $<

%.exe: %.cmo ../csv.cma
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $<

%.com: %.cmx ../csv.cmxa
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) $<


.PHONY: depend
depend: .depend
.depend.ocaml: $(wildcard *.ml) $(wildcard *.mli) $(wildcard test/*.ml)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend.ocaml

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp gmon.out
	rm -f Make.bat
	find . -type f -perm -u=x -exec rm -f {} \;


distclean: clean
	rm -f TAGS .depend.ocaml
