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

let substitute fname_in fname_out tr =
  let txt = read_all fname_in in
  let txt = List.fold_left (fun t (re, s) ->
                Str.global_replace (Str.regexp re) s t) txt tr in
  write fname_out [Printf.sprintf "#1 %S\n" fname_in;
                   txt]

let () =
  let pp = Filename.concat "src" "csv.pp.ml" in
  substitute pp (Filename.concat "src" "csv_std.ml")
    [" +LWT_t", "";
     "IF_LWT(\\([^,]*\\), *\\([^(),]*\\))", "(* \\1 *)\\2";
    ];
  substitute pp (Filename.concat "src" "csv_lwt.ml")
    [" +LWT_t", " Lwt.t";
     "IF_LWT( *\\([^,]*\\), *\\([^(),]*\\))", " \\1(* \\2 *)";
    ]
