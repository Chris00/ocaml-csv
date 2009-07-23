open Printf


let in_chan =
  new Csv.of_channel ~excel_tricks:true (open_in Sys.argv.(1));;

let print r =
(*   printf "%s\n%!" (String.concat ", " *)
(*                      (List.map (fun s -> "\""^String.escaped s^"\"") r)) *)
()

let () =
  in_chan#fold_left (fun _ r -> print r) ();;
