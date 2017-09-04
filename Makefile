PKGVERSION = $(shell git describe --always --dirty)

WEB = san@math.umons.ac.be:~/public_html/software/

build:
	jbuilder build @install examples/example.exe -j 4 --dev

tests: build
	jbuilder runtest

install uninstall clean:
	jbuilder $@

doc: build
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/csv.mli \
	  > _build/default/src/csv.mli
	jbuilder build @doc
	echo '.def { background: #f0f0f0; }' >> _build/default/_doc/odoc.css

upload-doc: doc
	scp -C -p -r _build/default/_doc/csv/ $(WEB)
	scp -C -p -r _build/default/_doc/csv-lwt/ $(WEB)
	scp -C -p _build/default/_doc/odoc.css $(WEB)

csvtool: build
	jbuilder exec csvtool pastecol 1-3 2,1,2 \
	  tests/testcsv9.csv tests/testcsv9.csv

submit:
	topkg distrib
	topkg publish distrib
	topkg opam pkg -n csv
	topkg opam pkg -n csv-lwt
# 	Perform the subtitution that topkkg does not:
	sed -e 's/\(^ *"csv"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/csv-lwt.$(PKGVERSION)/opam
	CONDUIT_TLS=native topkg opam submit -n csv
	CONDUIT_TLS=native topkg opam submit -n csv-lwt

.PHONY: build tests install uninstall doc upload-doc clean csvtool submit
