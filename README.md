[![Build Status](https://travis-ci.org/Chris00/ocaml-csv.svg?branch=master)](https://travis-ci.org/Chris00/ocaml-csv)

OCaml CSV
=========

The [comma-separated values](http://en.wikipedia.org/wiki/Comma-separated_values)
format — or CSV for short — is a simple tabular format supported by
all major spreadsheets.  This library implements pure OCaml functions
to read and write files in this format (including Excel extensions) as
well as some convenience functions to manipulate such data.

Compile & install
-----------------

The easiest way to install this library is to use [OPAM][]:

    opam install csv

for the standard version and

    opam install csv-lwt

for the LWT one.  If you prefer to compile and install by hand,
make sure you have [dune][] and run

    dune build @install
    dune install csv
    dune install csv-lwt

For the command line manipulation utility, do

    opam install csvtool

or

    dune install csvtool

[OPAM]: https://opam.ocaml.org/
[dune]: https://github.com/ocaml/dune


Uninstall
---------

With OPAM:

    opam remove csv
    opam remove csv-lwt
	opam remove csvtool

Manually (from the source directory):

    dune uninstall csv
    dune uninstall csv-lwt
	dune uninstall csvtool


Documentation
-------------

The documentation for the `Csv` (resp. `Csv_lwt`) module can be
found [online](https://ocaml.org/p/csv/latest/doc/Csv/index.html)
(resp. [here](https://ocaml.org/p/csv-lwt/latest/doc/Csv_lwt/index.html))
or in
[csv.mli](src/csv.mli) (resp. [csv_lwt.mli](lwt/csv_lwt.mli)).

Also see the [examples](examples/).
