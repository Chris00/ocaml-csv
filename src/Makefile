ROOT=..
include $(ROOT)/Makefile.conf

# Native code versions must be installed if and only if make opt was
# called previously (i.e. the files exist):
INSTALL_FILES = csv.mli $(wildcard *.cmi *.cma *.cmx *.a *.cmxa)

.PHONY: default all opt byte native
default: opt
all: byte
opt: native
byte: csv.cma
native: csv.cmxa

csv.cma: csv.cmo
csv.cmxa: csv.cmx

.PHONY: doc
doc:
	test -f $(DOC_DIR) || mkdir -p $(DOC_DIR)
	$(OCAMLDOC) -html -d $(DOC_DIR) $(OCAMLDOC_FLAGS) $(wildcard *.mli)

.PHONY: install uninstall
# TODO: copy HTML doc
install: META $(INSTALL_FILES)
	ocamlfind install $(PACKAGE) $^

uninstall:
	ocamlfind remove $(PACKAGE)

META: META.in
	@ sed -e "s/@PACKAGE@/$(PACKAGE)/" $< \
	| sed -e "s/@VERSION@/$(VERSION)/" > $@
	@ echo "Updated \"$@\"."


# Make.bat -- easy compilation on win32
Make.bat:
	$(MAKE) clean
#	Filter out all "make" messages
	$(MAKE) all | grep --invert-match "make" > $@

include $(ROOT)/Makefile.ocaml

