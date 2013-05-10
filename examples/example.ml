(* See also 'test.ml' for examples, and 'csv.mli' for documentation. *)

open Printf

let embedded_csv = "\
\"Banner clickins\"
\"Clickin\",\"Number\",\"Percentage\",
\"brand.adwords\",\"4,878\",\"14.4\"
\"vacation.advert2.adwords\",\"4,454\",\"13.1\"
\"affiliates.generic.tc1\",\"1,608\",\"4.7\"
\"brand.overture\",\"1,576\",\"4.6\"
\"vacation.cheap.adwords\",\"1,515\",\"4.5\"
\"affiliates.generic.vacation.biggestchoice\",\"1,072\",\"3.2\"
\"breaks.no-destination.adwords\",\"1,015\",\"3.0\"
\"fly.no-destination.flightshome.adwords\",\"833\",\"2.5\"
\"exchange.adwords\",\"728\",\"2.1\"
\"holidays.cyprus.cheap\",\"574\",\"1.7\"
\"travel.adwords\",\"416\",\"1.2\"
\"affiliates.vacation.generic.onlinediscount.200\",\"406\",\"1.2\"
\"promo.home.topX.ACE.189\",\"373\",\"1.1\"
\"homepage.hp_tx1b_20050126\",\"369\",\"1.1\"
\"travel.agents.adwords\",\"358\",\"1.1\"
\"promo.home.topX.SSH.366\",\"310\",\"0.9\""


let csvs =
  List.map (fun name -> name, Csv.load name)
           [ "examples/example1.csv"; "examples/example2.csv" ]


let () =
  let ecsv = Csv.input_all(Csv.of_string embedded_csv) in
    printf "---Embedded CSV---------------------------------\n" ;
    Csv.print_readable ecsv;

  List.iter (
    fun (name, csv) ->
      printf "---%s----------------------------------------\n" name;
      Csv.print_readable csv
  ) csvs;
  printf "Compare (Embedded CSV) example1.csv = %i\n"
         (Csv.compare ecsv (snd(List.hd csvs)))
