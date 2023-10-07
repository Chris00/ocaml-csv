open Printf

let print =
  let print_assoc row =
    List.iter (fun (k, v) -> printf " (%s, %s)" k v) row;
    printf "\n"
  in
  fun csv -> List.iter print_assoc csv

let testcsv ?skip_bom filename expected =
  try
    let csv = Csv.Rows.load ?skip_bom ~separator:';' filename in
    let csv = List.map Csv.Row.to_assoc csv in
    if csv <> expected then (
      printf "input file: %s\n" filename;
      printf "Csv library produced:\n";
      print csv;
      printf "Expected:\n";
      print expected;
      failwith "failed")
  with Csv.Failure (nrow, nfield, err) ->
    printf
      "The file %S line %i, field %i, does not conform to the CSV \
       specifications: %s\n"
      filename nrow nfield err;
    failwith "failed"

let () =
  testcsv "testcsv13.csv" ~skip_bom:true
    [ [ ("", "A\n"); ("", "B\n") ]; [ ("", "1"); ("", "2") ] ]
