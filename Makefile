PKGVERSION = $(shell git describe --always --dirty)

WEB = san@math.umons.ac.be:~/public_html/software/

build:
	dune build @install examples/example.exe -j 4

tests: build
	dune runtest --force

install uninstall clean:
	dune $@

doc: build
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/csv.mli \
	  > _build/default/src/csv.mli
	dune build @doc
	echo '.def { background: #f0f0f0; }' >> _build/default/_doc/odoc.css

upload-doc: doc
	scp -C -p -r _build/default/_doc/csv/ $(WEB)
	scp -C -p -r _build/default/_doc/csv-lwt/ $(WEB)
	scp -C -p _build/default/_doc/odoc.css $(WEB)

csvtool: build
	dune exec csvtool pastecol 1-3 2,1,2 \
	  tests/testcsv9.csv tests/testcsv9.csv


submit:
	topkg distrib
	topkg publish distrib
	topkg opam pkg -n csv
	topkg opam pkg -n csv-lwt
# 	Perform the subtitution that topkkg does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	sed -e 's/\(^ *"csv"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/csv-lwt.$(PKGVERSION)/opam
# until we have https://github.com/ocaml/opam-publish/issues/38
	[ -d packages ] ||(echo "ERROR: Make a symbolic link packages → \
		opam-repo/packages"; exit 1)
	cp -r _build/csv.* packages/csv/
	cp -r _build/csv-lwt.* packages/csv-lwt/
	cd packages && git add csv csv-lwt
#	CONDUIT_TLS=native topkg opam submit -n csv -n csv-lwt

.PHONY: build tests install uninstall doc upload-doc clean csvtool submit
