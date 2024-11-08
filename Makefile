PKGVERSION = $(shell git describe --always --dirty)

build:
	dune build @install examples/example.exe -j 4

tests: build
	dune runtest --force

install uninstall clean:
	dune $@

doc: build
	dune build @doc
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' --in-place \
	  _build/default/_doc/_html/csv/Csv/index.html \
	  _build/default/_doc/_html/csv-lwt/Csv_lwt/index.html


csvtool: build
	dune exec csvtool pastecol 1-3 2,1,2 \
	  tests/testcsv9.csv tests/testcsv9.csv

example:
	dune exec examples/example.exe

submit:
	dune-release distrib
	dune-release publish distrib
	dune-release opam pkg -p csv
	dune-release opam pkg -p csv-lwt
# 	Perform the subtitution that dune-release does not
#	(until opam2, https://discuss.ocaml.org/t/sync-versions-of-several-packages-coming-from-a-single-repo/808/5)
	sed -e 's/\(^ *"csv"\) */\1 {= "$(PKGVERSION)"}/' --in-place \
	  _build/csv-lwt.$(PKGVERSION)/opam
# until we have https://github.com/ocaml/opam-publish/issues/38
	[ -d packages ] ||(echo "ERROR: Make a symbolic link packages → \
		opam-repo/packages"; exit 1)
	cp -r _build/csv.* packages/csv/
	cp -r _build/csv-lwt.* packages/csv-lwt/
	cd packages && git add csv csv-lwt
#	CONDUIT_TLS=native dune-release opam submit -n csv -n csv-lwt

.PHONY: build tests install uninstall doc clean csvtool example submit
