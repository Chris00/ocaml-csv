PACKAGE	:= $(shell grep "name" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
VERSION := $(shell grep "version" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
ARCHIVE := $(shell grep "archive(byte)" META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")
XARCHIVE := $(shell grep "archive(native)" META | sed -e \
		"s/.*\"\([^\"]*\)\".*/\1/")

include Makefile.conf

PP	   = #-pp "camlp4o pa_macro.cmo"

INCLUDE		=
OCAMLC_FLAGS	= -g -dtypes $(INCLUDE)
OCAMLOPT_FLAGS := -inline 3 $(INCLUDE)

OCAMLDOC_GEN = -html
OCAMLDOC_FLAGS := $(OCAMLDOC_GEN)

DOCFILES = csv.mli

DISTFILES = META Makefile $(wildcard *.ml) $(wildcard *.mli)

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
byte: $(ARCHIVE) $(EXTRA_LIBS:.ml=.cma)
opt: $(XARCHIVE) $(EXTRA_LIBS:.ml=.cmxa)


TAGS: $(ML_FILES)
	$(OCAMLTAGS) $^

# Compile HTML documentation
DOC_DIR=doc
DOC_CSS=ocamldoc.css
doc: $(DOCFILES) $(CMI_FILES)
	@if [ -n "$(DOCFILES)" ] ; then \
	    if [ ! -x $(DOC_DIR) ] ; then mkdir $(DOC_DIR) ; fi ; \
	    if [ -r "$(DOC_CSS)" ] ; then \
	        CSS_STYLE="-css-style $(DOC_CSS)" ; \
	        $(LN) ../$(DOC_CSS) $(DOC_DIR)/$(DOC_CSS) ; \
	    fi; \
	    $(OCAMLDOC) -v -d $(DOC_DIR) -colorize-code -stars \
		 $(OCAMLDOC_FLAGS) $$CSS_STYLE $(DOCFILES) ; \
	fi

# (Un)installation
.PHONY: install uninstall
install: all
	ocamlfind remove $(PACKAGE); \
	[ -f "$(XARCHIVE)" ] && \
	extra="$(XARCHIVE) $(basename $(XARCHIVE)).a"; \
	ocamlfind install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PACKAGE) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META $$extra

installbyte:
	ocamlfind remove $(PACKAGE); \
	ocamlfind install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PACKAGE) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META


uninstall:
	ocamlfind remove $(PACKAGE)

# Make the tests
.PHONY: tests
tests: all
	cd tests; $(MAKE) all

# Make.bat -- easy compilation on win32
Make.bat:
	$(MAKE) clean
#	Filter out all "make" messages
	$(MAKE) all | grep --invert-match "make" > $@

# Make a tarball
.PHONY: dist
dist: $(DISTFILES) Make.bat
	@ if [ -z "$(PACKAGE)" ]; then echo "PACKAGE not defined"; exit 1; fi
	@ if [ -z "$(VERSION)" ]; then \
		echo "VERSION not defined"; exit 1; fi
	mkdir $(PACKAGE)-$(VERSION) ; \
	mv Make.bat $(PACKAGE)-$(VERSION); \
	cp -r $(DISTFILES) $(PACKAGE)-$(VERSION)/; \
	tar --exclude "CVS" --exclude ".cvsignore" --exclude "*~" \
	   --exclude "*.cm{i,x,o,xa}" --exclude "*.o" \
	  -jcvf $(PKG_TARBALL) $(PACKAGE)-$(VERSION); \
	rm -rf $(PACKAGE)-$(VERSION)

# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(PP) $(OCAMLC_FLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(PP) $(OCAMLOPT_FLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPT_FLAGS) $<


.PHONY: depend
depend: .depend
.depend.ocaml: $(wildcard *.ml) $(wildcard *.mli) $(wildcard test/*.ml)
	$(OCAMLDEP) $(PP) $(SYNTAX_OPTS) $^ > $@
include .depend.ocaml

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cm{i,o,x,a,xa} *.annot *.{a,o} *.tmp gmon.out
	rm -f *.html *.ps
	rm -f Make.bat $(PACKAGE)-$(VERSION).tar.bz2
	rm -rf $(DOC_DIR)
	if [ -d tests ]; then cd tests/; $(MAKE) clean; fi

distclean: clean
	rm -f TAGS .depend.ocaml
