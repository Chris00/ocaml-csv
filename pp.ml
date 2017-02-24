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

let write fname txt =
  (try Unix.chmod fname 0o666; Unix.unlink fname with _ -> ());
  let fh = open_out fname in
  output_string fh txt;
  close_out fh;
  (try Unix.chmod fname 0o466
   with Unix.Unix_error(e, _, _) ->
     prerr_endline("Warning: chmod " ^ fname ^ ": " ^ Unix.error_message e))

let substitute fname_in fname_out tr =
  let txt = read_all fname_in in
  let txt = List.fold_left (fun t (re, s) ->
                Str.global_replace (Str.regexp re) s t) txt tr in
  write fname_out txt

let () =
  let pp = Filename.concat "src" "csv.pp.ml" in
  substitute pp (Filename.concat "src" "csv_internal.ml")
    [" +LWT_t", "";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", "\\2";
    ];
  substitute pp (Filename.concat "src" "csv_lwt.ml")
    [" +LWT_t", " Lwt.t";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", " \\1";
    ]
