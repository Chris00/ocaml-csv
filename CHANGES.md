2.3 2019-09-09
--------------

- Be compatible with OCaml ≥ 4.08 (fixes #28).
- Put `csvtool` in its own package.
- Use UTF-8 to determine column widths for the "readable" format
  (fixes #21).
- Add deprecation attributes.
- Fix alignment in `csvtool` usage message.
- Small improvements to the documentation.

2.2 2018-11-04
--------------

- Compile the pre-processing script.  This makes possible to install
  the package using a compiler with the `-static` flag. (#25)

2.1 2018-07-11
--------------

- Improve the documentation of `Csv.Rows.load` and `of_in_obj`.
- Slightly improve tests.
- Use Dune (the new name for Jbuilder).

2.0 2017-09-02
--------------

- Split the package into `csv` and `csv-lwt`.
- Port to `jbuilder` and `topkg`.
