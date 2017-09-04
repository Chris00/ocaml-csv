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
make sure you have [jbuilder][] and run

    jbuilder build @install
    jbuilder install csv
    jbuilder install csv-lwt

[OPAM]: https://opam.ocaml.org/
[jbuilder]: https://github.com/janestreet/jbuilder


Uninstall
---------

With OPAM:

    opam remove csv
    opam remove csv-lwt

Manually (from the source directory):

    jbuilder uninstall csv
    jbuilder uninstall csv-lwt

Documentation
-------------

The documentation for the `Csv` (resp. `Csv_lwt`) module can be
found [online](https://math.umons.ac.be/anum/software/csv/) 
(resp. [here](https://math.umons.ac.be/anum/software/csv-lwt/)) or in
[csv.mli](src/csv.mli) (resp. [csv_lwt.mli](lwt/csv_lwt.mli)).

Also see the [examples](examples/).
