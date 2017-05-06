(* Generate interfaces for standard IO and Lwt. *)

#load "str.cma";;
#load "unix.cma";;

let read_all fname =
  (* Avoid Bytes for backward compatibility. *)
  let fh = open_in fname in
  let len = in_channel_length fh in
  let b = Buffer.create len in
  Buffer.add_channel b fh len;
  Buffer.contents b

let write fname txts =
  (try Unix.chmod fname 0o666; Unix.unlink fname with _ -> ());
  let fh = open_out fname in
  List.iter (fun txt -> output_string fh txt) txts;
  close_out fh;
  (try Unix.chmod fname 0o466
   with Unix.Unix_error(e, _, _) ->
     prerr_endline("Warning: chmod " ^ fname ^ ": " ^ Unix.error_message e))

let substitute fname_in tr =
  let txt = read_all fname_in in
  let txt = List.fold_left (fun t (re, s) ->
                let repl = Str.global_replace (Str.regexp re) s in
                (* Replace twice to perform the substitution in macro args *)
                repl(repl t)) txt tr in
  [Printf.sprintf "#1 %S\n" fname_in;  txt]

let balanced_braces4 =
  let b = "\\([^()]\\|(" in
  let e = ")\\)*" in
  String.concat "" ["\\([^(),]\\|("; b; b; b; "[^()]*"; e; e; e; e]

let () =
  let pp = Filename.concat "src" "csv.pp.ml" in
  let csv_std =
    substitute pp
      [" +LWT_t", "";
       ("IF_LWT(\\(" ^ balanced_braces4 ^"\\),\\(" ^ balanced_braces4 ^ "\\))",
        "(* \\1 *)\\6");
       ("TRY_WITH(\\(" ^ balanced_braces4 ^"\\),\\(" ^ balanced_braces4 ^ "\\))",
        "try \\1 with \\6");
       (* Payload surrounded by braces to avoid absorbing "in" in \2 *)
       ("let%lwt \\([a-z][a-zA-Z_]*\\) = (\\(" ^ balanced_braces4 ^ "\\)) in",
        "let \\1 = \\2 in");
       ";%lwt", ";";
       "return", "";
      ] in
  write (Filename.concat "src" "csv_std.ml") csv_std;
  let csv_lwt =
    substitute pp
      [" +LWT_t", " Lwt.t";
       ("IF_LWT(\\(" ^ balanced_braces4 ^"\\),\\(" ^ balanced_braces4 ^ "\\))",
        " \\1(* \\6 *)");
       ("TRY_WITH(\\(" ^ balanced_braces4 ^"\\),\\(" ^ balanced_braces4 ^ "\\))",
        "Lwt.catch (fun () -> \\1) (function \\6 | exn -> Lwt.fail exn)");
       ("let%lwt \\([a-z][a-zA-Z_]*\\) = (\\(" ^ balanced_braces4 ^ "\\)) in",
        "(\\2) >>= fun \\1 -> ");
       ";%lwt", " >>= fun () -> ";
       "raise", "Lwt.fail";
      ] in
  write (Filename.concat "src" "csv_lwt.ml") csv_lwt
