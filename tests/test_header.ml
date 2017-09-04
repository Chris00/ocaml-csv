open Printf

let print =
  let print_assoc row =
    List.iter (fun (k,v) -> printf " (%s, %s)" k v) row;
    printf "\n" in
  fun csv -> List.iter print_assoc csv

let testcsv ?has_header ?header filename expected =
  try
    let csv = Csv.Rows.load ?has_header ?header filename in
    let csv = List.map Csv.Row.to_assoc csv in
    if csv <> expected then (
      printf "input file: %s\n" filename;
      printf "Csv library produced:\n";
      print csv;
      printf "Expected:\n";
      print expected;
      failwith "failed"
    )
  with Csv.Failure(nrow, nfield, err) ->
    printf "The file %S line %i, field %i, does not conform to the CSV \
            specifications: %s\n" filename nrow nfield err;
    failwith "failed"

let () =
  printf "TEST %s\n%!" (Filename.basename Sys.argv.(0));
  testcsv
    "testcsv11.csv" ~has_header:true
    [["h1", "a";  "h2", "b";  "", "c";  "h4", "d"]];

  testcsv
    "testcsv11.csv" ~has_header:true ~header:["q1"; ""; "q3"; "q4"]
    [["q1", "a";  "h2", "b";  "q3", "c";  "q4", "d"]];

  testcsv
    "testcsv2.csv" ~header:["h1"; ""; "h1"] (* duplicate header *)
    [ [ "h1", "Normal field";  "", "Quoted field";
        "", "Quoted field with \"\" quotes" ] ]
