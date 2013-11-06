OCaml CSV
=========

The [comma-separated values](http://en.wikipedia.org/wiki/Comma-separated_values)
format — or CSV for short — is a simple tabular format supported by
all major spreadsheets.  This library implements functions to read and
write files in this format (including Excel extensions) as well as
some convenience functions to manipulate such data.

Compile
-------

This package uses [oasis](https://github.com/ocaml/oasis) to generate
its configure, build and install scripts.  You do not need oasis to
install and use the library.  Simply run

    ocaml setup.ml -configure
    ocaml setup.ml -build

Install
-------

    ocaml setup.ml -install

Uninstall
---------

    ocaml setup.ml -uninstall
