(* Test the [fix] flag. *)

open Printf

let test lineno csv_string expected =
  (* First, parsing the CSV must raise an exception without the flag. *)
  (try ignore(Csv.input_all(Csv.of_string csv_string));
       printf "File %S, line %d: The CSV must NOT parse without ~fix.\n"
         __FILE__ lineno;
       exit 1
   with Csv.Failure _ -> ());
  (* Now check that it works with ~fix *)
  let csv = Csv.input_all (Csv.of_string csv_string ~fix:true) in
  if csv <> expected then (
    printf "File %S, line %d: Csv library produced:\n" __FILE__ lineno;
    Csv.print csv;
    printf "Expected:\n";
    Csv.print expected;
    failwith "Test.fix: failed"
  )

let () =
  printf "TEST %s\n%!" (Filename.basename Sys.argv.(0));
  test __LINE__ {|""a""|} [["\"a\""]];
  test __LINE__ {|""a""b|} [["\"a\"b"]];
  test __LINE__ {|""a"""b|} [["\"a\"\"b"]];
  test __LINE__ {|""a"",b|} [["\"a\""; "b"]];
  (* ↳ ≠ libreoffice but consistent with the idea of printing fields
     forgetting to escape them. *)
  test __LINE__ {|""a"b",c|} [["\"a\"b"; "c"]];
  test __LINE__ {|"a"b"|} [["a\"b"]];
  test __LINE__ {|"a"b""|} [["a\"b\""]];
  test __LINE__ {|"a"b"" c"|} [["a\"b\" c"]];
  test __LINE__ {|"a"b",c|} [["a\"b"; "c"]];
  test __LINE__ {|"a"b"",c|} [["a\"b\""; "c"]];
  test __LINE__ {|"a"b"" c ",d|} [["a\"b\" c "; "d"]];
  test __LINE__ {|"a"b"  ,c|} [["a\"b"; "c"]];
  test __LINE__ {|"a"b""  ,c|} [["a\"b\""; "c"]];
  test __LINE__ {|"a"b""c "  ,d|} [["a\"b\"c "; "d"]];
  test __LINE__ {|"a"bc" ,d"|} [["a\"bc"; "d\""]];
  test __LINE__ {|""a"b"
                 c|} [["\"a\"b"]; ["c"]];
  test __LINE__ {|""ab""
                 c|} [["\"ab\""]; ["c"]];
  test __LINE__ "\"\"a\"b\"\"\n c" [["\"a\"b\"\n c"]];
  (* ↳ This is a difficult case: it is considered a single field
     because the 3rd quote is a badly escaped so one expected a 4th
     (bad) one to close it.  Libreoffice parses it the same way. *)
  test __LINE__ {|""a"b"""
                 c|} [["\"a\"b\""]; ["c"]];
