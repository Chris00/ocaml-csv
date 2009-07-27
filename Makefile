include Makefile.conf

.PHONY: default all opt byte native install uninstall htdoc doc examples
default: byte opt
all: byte
opt: native
htdoc: doc
byte native install uninstall doc:
	$(MAKE) -C src $@

csv.godiva: csv.godiva.in
	@ sed -e "s/@PACKAGE@/$(PACKAGE)/" $< \
	| sed -e "s/@VERSION@/$(VERSION)/" \
	| sed -e "s/@TARBALL@/$(TARBALL)/" \
	| sed -e "s/@DOWNLOAD@/$(OCAMLFORGE_FILE_NO)/" > $@
	@ echo "Updated \"$@\"."

# Assume the environment variable $GODI_LOCALBASE is set
.PHONY: godi
godi: csv.godiva
	godiva $<

# "Force" a tag to be defined for each released tarball
tar:
	bzr export /tmp/$(TARBALL) -r "tag:$(VERSION)"
	@echo "Created tarball '/tmp/$(TARBALL)'."

.PHONY: tests
tests: byte
	$(MAKE) -C tests

.PHONY: web
web: doc
	$(MAKE) -C doc $@

.PHONY: clean
clean:
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg) csv.godiva
	$(MAKE) -C src $@
	$(MAKE) -C tests $@
