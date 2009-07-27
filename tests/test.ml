open Printf

let do_testcsv filename expected =
  try
    let csv = Csv.load filename ~excel_tricks:true in
    if csv <> expected then (
      printf "input file: %s\n" filename;
      printf "Csv library produced:\n";
      Csv.print csv;
      printf "Expected:\n";
      Csv.print expected;
      failwith "failed"
    )
  with e ->
    printf "Loading the file %S raised %s" filename (Printexc.to_string e);
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
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
	       [ "f"; "g"; "h"; "i"; "" ];
	       [ "" ];
	       [ ] ] in
  let csv2 = Csv.trim ~top:false ~left:false ~right:true ~bottom:true csv1 in
  assert (Csv.compare csv1 csv2 = 0)
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
