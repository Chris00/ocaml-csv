(* See also 'test.ml' for examples, and 'csv.mli' for documentation. *)

open Printf

let csvs =
  List.map (fun name -> name, Csv.load name)
    [ "example1.csv"; "example2.csv" ]

let () =
  List.iter (
    fun (name, csv) ->
      printf "---%s----------------------------------------\n" name;
      Csv.print_readable csv
  ) csvs
