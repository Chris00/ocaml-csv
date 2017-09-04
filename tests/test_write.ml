open Printf

let roundtrip ?excel_tricks csv =
  let buf = Buffer.create 128 in
  Csv.output_all (Csv.to_buffer buf ?excel_tricks) csv;
  let csv' = Csv.input_all (Csv.of_string (Buffer.contents buf)) in
  if Csv.compare csv csv' <> 0 then (
    printf "Csv roundtrip:\n";
    Csv.print csv';
    printf "Expected:\n";
    Csv.print csv;
    failwith "failed!"
  )

let () =
  printf "TEST %s\n%!" (Filename.basename Sys.argv.(0));
  roundtrip [ [ "01234567" ] ] ~excel_tricks:true
