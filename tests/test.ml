open Printf

let do_testcsv ?separator filename expected =
  try
    let csv = Csv.load ?separator filename in
    if csv <> expected then (
      printf "input file: %s\n" filename;
      printf "Csv library produced:\n";
      Csv.print csv;
      printf "Expected:\n";
      Csv.print expected;
      failwith "failed"
    )
  with Csv.Failure(nrow, nfield, err) ->
    printf "The file %S line %i, field %i, does not conform to the CSV \
      specifications: %s\n" filename nrow nfield err;
    failwith "failed"



let () =
  do_testcsv
    "testcsv1.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns." ] ]
let () =
  do_testcsv
    "testcsv2.csv"
    [ [ "Normal field"; "Quoted field"; "Quoted field with \"\" quotes" ] ]
let () =
  do_testcsv
    "testcsv3.csv"
    [ [ "" ];
      [ ""; "" ];
      [ ""; ""; "" ];
      [ ""; ""; ""; "" ];
      [ ""; ""; ""; ""; "" ] ]
let () =
  do_testcsv
    "testcsv4.csv"
    []
let () =
  do_testcsv
    "testcsv5.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns.";
        "a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]
let () =
  do_testcsv
    "testcsv6.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns\nand \000";
        "a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ]

let () =
  do_testcsv
    "testcsv7.csv"
    [ [ "Initial"; "and"; "final"; ""; "spaces"; "do not matter" ];
      [ " Quoted spaces "; "are"; " important " ] ]


let () =
  do_testcsv ~separator:'\t'
    "testcsv8.csv"
    [["Foo"; "Bar"]; ["Baz"; "Boof"]; ["a"; ""; "c"]]

let () =
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
               [ "f"; "g"; "h"; "i"; "" ];
               [ "" ];
               [ ] ] in
  let csv2 = Csv.trim ~top:false ~left:false ~right:true ~bottom:true csv1 in
  assert(compare csv1 csv2 <> 0);
  assert(Csv.compare csv1 csv2 = 0)

let () =
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
               [ "f"; "g"; "h"; "i"; "" ];
               [ "" ];
               [ ] ] in
  let csv2 = [ [ "a"; "b"; "c"; "d"; "" ];
               [ "f"; "g"; "h"; "i"; "" ];
               [ "" ];
               [ ] ] in
  assert (Csv.compare csv1 csv2 < 0)

let () =
  print_endline "All tests succeeded."
