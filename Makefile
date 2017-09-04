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


.PHONY: build tests install uninstall doc upload-doc clean csvtool
