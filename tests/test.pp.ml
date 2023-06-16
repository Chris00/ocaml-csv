IF_LWT(open Lwt  open Lwt_io, open Printf)

module C = IF_LWT(Csv_lwt, IF_EIO(Csv_eio, Csv))

let do_testcsv IF_EIO(env, ()) ?separator ?strip ?backslash_escape filename expected =
  TRY_WITH(
    let%lwt csv = (C.load ?separator ?strip ?backslash_escape IF_EIO(Eio.(Path.(/) (Stdenv.fs env) filename), filename)) in
    if csv <> expected then (
      printf "input file: %s\n" filename;%lwt
      printf "Csv library produced:\n";%lwt
      C.print IF_EIO(~stdout:(Eio.Stdenv.stdout env),) csv;%lwt
      printf "Expected:\n";%lwt
      C.print IF_EIO(~stdout:(Eio.Stdenv.stdout env),) expected;%lwt
      raise(Failure "failed")
    )
    else return()
  , Csv.Failure(nrow, nfield, err) ->
    (printf "The file %S line %i, field %i, does not conform to the CSV \
      specifications: %s\n" filename nrow nfield err;%lwt
    raise(Failure "failed")))


let main env =
  do_testcsv env
    "testcsv1.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns." ] ];%lwt
  do_testcsv env
    "testcsv2.csv"
    [ [ "Normal field"; "Quoted field"; "Quoted field with \"\" quotes" ] ];%lwt
  do_testcsv env
    "testcsv3.csv"
    [ [ "" ];
      [ ""; "" ];
      [ ""; ""; "" ];
      [ ""; ""; ""; "" ];
      [ ""; ""; ""; ""; "" ] ];%lwt
  do_testcsv env
    "testcsv4.csv"
    [];%lwt
  do_testcsv env
    "testcsv5.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns.";
        "a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ];%lwt
  do_testcsv env
    "testcsv6.csv"
    [ [ "This is a test\nwith commas,,,,,\n\nand carriage returns\nand \000";
        "a second field"; "a third field" ];
      [ "a fourth field on a new line" ] ];%lwt

  do_testcsv env
    "testcsv7.csv"
    [ [ "Initial"; "and"; "final"; ""; "spaces"; "do not matter" ];
      [ " Quoted spaces "; "are"; " important " ] ];%lwt

  do_testcsv env
    "testcsv7.csv" ~strip:false
    [ [ " Initial "; " and "; " final"; " "; "\tspaces   "; " do not matter " ];
      [ " Quoted spaces "; " are"; " important " ] ];%lwt

  do_testcsv env ~separator:'\t'
    "testcsv8.csv"
    [["Foo"; "Bar"]; ["Baz"; "Boof"]; ["a"; ""; "c"]];%lwt

  do_testcsv env "testcsv9.csv"
    [["A1"; "A2"; "A3"]; ["B1"; "B2"]; ["C1"]];%lwt

  do_testcsv env "testcsv10.csv" ~backslash_escape:true
    [["a"; "b\"c"; "d\\d\000"]];%lwt

  do_testcsv env "testcsv12.csv" ~separator:';'
    [["Foo"; "Bar"]; ["Baz"; "Boof"]; ["a"; ""; "c"]]

let () =
  IF_LWT(
    Lwt_main.run (main ())
  , IF_EIO(
    Eio_main.run main
  ,
    main ()
  ))

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
  let csv1 = [ [ "a"; "b"; "c"; ""; "" ];
               [ "f"; "g"; "h"; "i"; "" ];
               [ "" ];
               [ ] ] in
  let csv2 = [ [ "A"; "B"; "C"; ""; "" ];
               [ "F"; "G"; "H"; "I"; "" ];
               [ "" ];
               [ ] ] in
  assert (Csv.map ~f:String.capitalize_ascii csv1 = csv2)


let () =
  print_endline IF_LWT("All conformity tests succeeded (Csv_lwt).",
                IF_EIO("All conformity tests succeeded (Csv_eio).",
                       "All conformity tests succeeded (Csv)."))
