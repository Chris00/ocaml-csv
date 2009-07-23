open Printf

let cat file =
  let ic = Csv.of_channel ~excel_tricks:true (open_in file) in
  let oc = Csv.to_channel stdout in
  Csv.iter (fun r -> Csv.output_record oc r) ic

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Filename.basename Sys.argv.(i) in
    printf "---%s%s\n" file (String.make (47 - String.length file) '-');
    cat Sys.argv.(i);
  done;
  print_endline "--------------------------------------------------"

