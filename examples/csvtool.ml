(* Handy tool for managing CSV files.
   @author Richard Jones <rjones@redhat.com>
*)

open Printf

(*------------------------------ start of code from extlib *)
exception Invalid_string

let find str sub =
  let sublen = String.length sub in
  if sublen = 0 then
    0
  else
    let found = ref 0 in
    let len = String.length str in
    try
      for i = 0 to len - sublen do
        let j = ref 0 in
        while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      raise Invalid_string
    with
      Exit -> !found

let split str sep =
  let p = find str sep in
  let len = String.length sep in
  let slen = String.length str in
  String.sub str 0 p, String.sub str (p + len) (slen - p - len)

let nsplit str sep =
  if str = "" then []
  else (
    let rec nsplit str sep =
      try
        let s1 , s2 = split str sep in
        s1 :: nsplit s2 sep
      with
        Invalid_string -> [str]
    in
    nsplit str sep
  )

type 'a mut_list =  {
        hd: 'a;
        mutable tl: 'a list
}
external inj : 'a mut_list -> 'a list = "%identity"

let dummy_node () = { hd = Obj.magic (); tl = [] }

let rec drop n = function
  | _ :: l when n > 0 -> drop (n-1) l
  | l -> l

let take n l =
  let rec loop n dst = function
    | h :: t when n > 0 ->
        let r = { hd = h; tl = [] } in
        dst.tl <- inj r;
        loop (n-1) r t
    | _ ->
        ()
  in
  let dummy = dummy_node() in
  loop n dummy l;
  dummy.tl
(*------------------------------ end of extlib code *)

(** Generic [iter] function reading a list of CSV files.  The function
    [f] can raise [Exit] to mean that no further processing should be done. *)
let csv_iter ~input_sep ~f files =
  try
    List.iter (fun filename ->
        let fh = if filename = "-" then stdin else open_in filename in
        let csv_in = Csv.of_channel ~separator:input_sep fh in
        try (f filename csv_in : unit);
            Csv.close_in csv_in;
        with e -> Csv.close_in csv_in; raise e
      ) files
  with Exit -> ()

let iter_csv_rows ~input_sep ~f files =
  csv_iter ~input_sep files ~f:(fun _ csv_in -> Csv.iter ~f csv_in)

(** Generic [fold] function on a list of CSV files. *)
let csv_fold ~input_sep ~f ~init files =
  List.fold_left (fun a filename ->
      let fh = if filename = "-" then stdin else open_in filename in
      let csv_in = Csv.of_channel ~separator:input_sep fh in
      let a = f a csv_in in
      Csv.close_in csv_in;
      a
    ) init files



(* Parse column specs. *)
type colspec = range list
and range =
  | Col of int (* 0 *)
  | Range of int * int (* 2-5 *)
  | ToEnd of int (* 7- *)

let parse_colspec ~count_zero colspec =
  let cols = nsplit colspec "," in
  let cols = List.map (
    fun col ->
      try
        (try
           let first, second = split col "-" in
           if second <> "" then
             Range (int_of_string first, int_of_string second)
           else
             ToEnd (int_of_string first)
         with
           Invalid_string ->
             Col (int_of_string col)
        )
      with
        Failure _ ->
          failwith (colspec ^ ":" ^ col ^ ": invalid column-spec")
  ) cols in

  (* Adjust so columns always count from zero. *)
  if not count_zero then
    List.map (
      function
      | Col c -> Col (c-1)
      | Range (s, e) -> Range (s-1, e-1)
      | ToEnd e -> ToEnd (e-1)
    ) cols
  else
    cols

let rec width_of_colspec = function
  | [] -> 0
  | Col _ :: rest -> 1 + width_of_colspec rest
  | Range (s, e) :: rest -> (e-s+1) + width_of_colspec rest
  | ToEnd _ :: _ ->
      failwith "width_of_colspec: cannot calculate width of an open column spec (one which contains 'N-')"

(* For closed column specs, this preserves the correct width in the
 * result.
 *)
let cols_of_colspec colspec row =
  let rec loop = function
    | [] -> []
    | Col c :: rest ->
        (try List.nth row c
         with Failure _ -> "") :: loop rest
    | Range (s, e) :: rest ->
        let width = e-s+1 in
        let range = take width (drop s row) in
        let range = List.hd (Csv.set_columns ~cols:width [range]) in
        List.append range (loop rest)
    | ToEnd e :: rest ->
        List.append (drop e row) (loop rest)
  in
  loop colspec

(* The actual commands. *)
let cmd_cols ~input_sep ~output_sep ~chan colspec files =
  let csv_out = Csv.to_channel ~separator:output_sep chan in
  iter_csv_rows ~input_sep files ~f:(fun row ->
      Csv.output_record csv_out (cols_of_colspec colspec row));
  Csv.close_out csv_out

let cmd_namedcols ~input_sep ~output_sep ~chan names files =
  let csv_out = Csv.to_channel ~separator:output_sep chan in
  (* Output the header of the final file. *)
  Csv.output_record csv_out names;
  csv_iter ~input_sep files ~f:(fun fname csv_in ->
      match (try Some(Csv.next csv_in) with End_of_file -> None) with
      | None -> ()
      | Some header ->
         (* Do the headers requested exist in the CSV file?  If not,
            throw an error.  *)
         List.iter (fun name ->
             if not (List.mem name header) then
               failwith (sprintf "namedcol: requested header %S not in CSV \
                                  file %S" name fname)
        ) names;
      Csv.iter csv_in ~f:(fun row ->
          let row = Csv.combine ~header row in
          let named = List.map (fun name -> List.assoc name row) names in
          Csv.output_record csv_out named;
        )
    );
  Csv.close_out csv_out

let cmd_width ~input_sep ~chan files =
  let width = csv_fold ~input_sep files ~init:0 ~f:(fun w csv_in ->
                  Csv.fold_left csv_in ~init:w ~f:(fun w row ->
                      max w (List.length row))) in
  fprintf chan "%d\n" width

let cmd_height ~input_sep ~chan files =
  let height = csv_fold ~input_sep files ~init:0 ~f:(fun h csv_in ->
                   Csv.fold_left ~f:(fun h _ -> h + 1) ~init:h csv_in) in
  fprintf chan "%d\n" height

let cmd_readable ~input_sep ~chan files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  Csv.save_out_readable chan csv

let cmd_cat ~input_sep ~output_sep ~chan files =
  (* Avoid loading the whole file into memory. *)
  let chan = Csv.to_channel ~separator:output_sep chan in
  let f row =
    Csv.output_record chan row
  in
  iter_csv_rows ~input_sep ~f files

let cmd_paste ~input_sep ~output_sep ~chan files =
  (* Return the 1st row, concatenation of all 1st rows; whether all
     CSV files are empty; and the CSV files without their 1st row. *)
  let rec add_columns = function
    | [] -> ([], true, []) (* empty CSV file list *)
    | [] :: csvs -> (* exhausted the first CSV file *)
       let row, empty, csvs = add_columns csvs in
       (row, empty, [] :: csvs)
    | (r :: csv0) :: csvs ->
       let row, _, csvs = add_columns csvs in
       (r @ row, false, csv0 :: csvs) in
  let rec paste_rows csvs final_csv =
    let row, empty, csvs = add_columns csvs in
    if empty then List.rev final_csv
    else paste_rows csvs (row :: final_csv)
  in
  let csvs = List.map (Csv.load ~separator:input_sep) files in
  let csv = paste_rows csvs [] in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv


(* Given [colspec1] and [colspec2], return an associative list that
   indicates the correspondence between the i th column specified by
   [colspec1] and the corresponding one in [colspec2]. *)
let rec colspec_map colspec1 colspec2 =
  match colspec1 with
  | [] -> []
  | Col i :: tl1 ->
     (match colspec2 with
      | Col k :: tl2 -> (i,k) :: colspec_map tl1 tl2
      | Range(k,l) :: tl2 ->
         let colspec2 = if k < l then Range(k+1, l) :: tl2
                        else if k = l then tl2
                        else (* k > l *) Range(k-1, l) :: tl2 in
         (i,k) :: colspec_map tl1 colspec2
      | ToEnd k :: _ ->
         (i, k) :: colspec_map tl1 [ToEnd(k+1)]
      | [] -> failwith "pastecol: the second range does not contain \
                       enough columns")
  | Range(i,j) :: tl1 ->
     let colspec1 = if i < j then Range(i+1, j) :: tl1
                    else if i = j then tl1
                    else (* i > j *) Range(i-1, j) :: tl1 in
     (match colspec2 with
      | Col k :: tl2 ->  (i,k) :: colspec_map colspec1 tl2
      | Range(k,l) :: tl2 ->
         let colspec2 = if k < l then Range(k+1, l) :: tl2
                        else if k = l then tl2
                        else (* k > l *) Range(k-1, l) :: tl2 in
         (i,k) :: colspec_map colspec1 colspec2
      | ToEnd k :: _ ->
         (i,k) :: colspec_map colspec1 [ToEnd(k+1)]
      | [] -> failwith "pastecol: the second range does not contain \
                       enough columns")
  | ToEnd i :: _ ->
     let m = sprintf "pastecol: the first range cannot contain an open \
                      range like %i-" i in
     failwith m

(* When several bindings are defined for an initial column, use the
   last one.  ASSUME that the associative map is sorted w.r.t. the
   first data. *)
let rec reduce_colspec_map = function
  | (i,_) :: (((j,_) :: _) as tl) when (i: int) = j ->
     reduce_colspec_map tl (* maybe (j,_) is also supplanted *)
  | m :: tl -> m :: reduce_colspec_map tl
  | [] -> []

let cmd_pastecol ~input_sep ~output_sep ~chan colspec1 colspec2 file1 file2 =
  let csv1 = Csv.load ~separator:input_sep file1 in
  let csv2 = Csv.load ~separator:input_sep file2 in
  let m = colspec_map colspec1 colspec2 in
  let m = List.stable_sort (fun (i,_) (j,_) -> compare (i:int) j) m in
  let m = reduce_colspec_map m in
  let rec update m curr_col row1 row2 =
    match m with
    | [] -> row1 (* substitutions exhausted *)
    | (i, j) :: m_tl ->
       let c, row1 = match row1 with
         | [] -> "", [] (* row exhausted but some remaining substitutions must
                          be performed.  Create new columns. *)
         | c :: row1_tl -> c, row1_tl in
       if curr_col = i then
         let c' = try List.nth row2 j with _ -> "" in
         c' :: update m_tl (curr_col + 1) row1 row2
       else (* curr_col < i *)
         c :: update m (curr_col + 1) row1 row2
  in
  let csv = List.map2 (update m 0) csv1 csv2 in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv


let cmd_set_columns ~input_sep ~output_sep ~chan cols files =
  (* Avoid loading the whole file into memory. *)
  let csv_out = Csv.to_channel ~separator:output_sep chan in
  let f row =
    match Csv.set_columns ~cols [row] with
    | [row] -> Csv.output_record csv_out row
    | _ -> assert false in
  iter_csv_rows ~input_sep ~f files;
  Csv.close_out csv_out

let cmd_set_rows ~input_sep ~output_sep ~chan rows files =
  let rows = ref rows in
  let csv_out = Csv.to_channel ~separator:output_sep chan in
  iter_csv_rows ~input_sep files ~f:(fun row ->
      if !rows <= 0 then raise Exit;
      Csv.output_record csv_out row;
      decr rows;
    );
  for _i = 1 to !rows do Csv.output_record csv_out [] done;
  Csv.close_out csv_out

let cmd_head ~input_sep ~output_sep ~chan rows files =
  (* Avoid loading the whole file into memory, or even loading
   * later files.
   *)
  let nr_rows = ref rows in
  let chan = Csv.to_channel ~separator:output_sep chan in
  let f row =
    if !nr_rows > 0 then (
      decr nr_rows;
      Csv.output_record chan row
    )
    else raise Exit
  in
  iter_csv_rows ~input_sep ~f files

let cmd_drop ~input_sep ~output_sep ~chan rows files =
  (* Avoid loading the whole file into memory. *)
  let nr_rows = ref rows in
  let chan = Csv.to_channel ~separator:output_sep chan in
  let f row =
    if !nr_rows = 0 then
      Csv.output_record chan row
    else
      decr nr_rows
  in
  iter_csv_rows ~input_sep ~f files

let cmd_square ~input_sep ~output_sep ~chan files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.square csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

let cmd_sub ~input_sep ~output_sep ~chan r c rows cols files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.sub ~r ~c ~rows ~cols csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

let cmd_replace ~input_sep ~output_sep ~chan colspec update files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in

  (* Load the update CSV file in. *)
  let update = Csv.load ~separator:input_sep update in

  (* Compare two rows for equality by considering only the columns
   * in colspec.
   *)
  let equal row1 row2 =
    let row1 = cols_of_colspec colspec row1 in
    let row2 = cols_of_colspec colspec row2 in
    0 = Csv.compare [row1] [row2]
  in

  (* Look for rows in the original to be replaced by rows from the
   * update file.  This is an ugly O(n^2) hack (XXX).
   *)
  let csv = List.filter (
    fun row -> not (List.exists (equal row) update)
  ) csv in
  let csv = csv @ update in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

let cmd_transpose ~input_sep ~output_sep ~chan files =
  List.iter (fun file ->
             let tr = Csv.transpose (Csv.load ~separator:input_sep file) in
             Csv.output_all (Csv.to_channel ~separator:output_sep chan) tr
            ) files


type format_el = String of string | Col of int

let is_digit c = '0' <= c && c <= '9'

(* Return the non-negative number starting at [i0 < i1 < len_s =
   String.length s] and the index of the first character after that
   number.  It is expected that [s.[i0]] be a digit, otherwise
   [Failure] is be raised. *)
let rec get_digit s len_s i0 i1 =
  if i1 < len_s then
    if is_digit s.[i1] then get_digit s len_s i0 (i1 + 1)
    else (int_of_string(String.sub s i0 (i1 - i0)), i1)
  else (* i0 < i1 (>)= len_s *)
    (int_of_string(String.sub s i0 (len_s - i0)), len_s)

(* Prepend to the format [fmt] the substring s.[i0 .. i1 - 1] unless
   it is empty. *)
let prepend_substring s i0 i1 fmt =
  if i0 < i1 then String(String.sub s i0 (i1 - i0)) :: fmt
  else (* i0 ≥ i1, empty substring *) fmt

(* [i0 ≤ i1 ≤ len_s] *)
let rec split_format s len_s i0 i1 =
  if i1 >= len_s then
    if i0 < len_s then [String(String.sub s i0 (len_s - i0))]
    else []
  else if s.[i1] = '%' then
    let i2 = i1 + 1 in
    if i2 >= len_s then
      split_format s len_s i0 i2 (* consider a final '%' as a normal char *)
    else if is_digit s.[i2] then
      let col, i3 = get_digit s len_s i2 (i2 + 1) in
      prepend_substring s i0 i1 (Col col :: split_format s len_s i3 i3)
    else if s.[i2] = '(' then
      if i2 + 1 < len_s && is_digit s.[i2 + 1] then (
        let col, i3 = get_digit s len_s (i2 + 1) (i2 + 2) in
        if i3 >= len_s || s.[i3] <> ')' then (
          let r = String.sub s i1 (i3 - i1) in
          failwith(sprintf "Column format %S not terminated by ')'" r)
        );
        prepend_substring
          s i0 i1 (Col col :: split_format s len_s (i3 + 1) (i3 + 1))
      )
      else failwith "Column format %( not followed by a number"
    else if s.[i2] = '%' then
      let i3 = i2 + 1 in
      String(String.sub s i0 (i1 - i0 + 1)) :: split_format s len_s i3 i3
    else (* % + non-digit, consider it a literal '%' *)
      split_format s len_s i0 i2
  else if s.[i1] = '\\' then
    (* Handle usual escapes. *)
    let i2 = i1 + 1 in
    if i2 >= len_s then split_format s len_s i0 i2
    else if s.[i2] = 'n' then
      let i3 = i2 + 1 in
      prepend_substring s i0 i1 (String "\n" :: split_format s len_s i3 i3)
    else if s.[i2] = 'r' then
      let i3 = i2 + 1 in
      prepend_substring s i0 i1 (String "\r" :: split_format s len_s i3 i3)
    else if s.[i2] = 't' then
      let i3 = i2 + 1 in
      prepend_substring s i0 i1 (String "\t" :: split_format s len_s i3 i3)
    else split_format s len_s i0 i2
  else
    split_format s len_s i0 (i1 + 1)

let print_format row = function
  | String s -> print_string s
  | Col c -> try print_string (List.nth row (c - 1)) with _ -> ()

let cmd_format ~input_sep fmt files =
  let fmt = split_format fmt (String.length fmt) 0 0 in
  iter_csv_rows ~input_sep files
                ~f:(fun row -> List.iter (print_format row) fmt)

let cmd_call ~input_sep command files =
  (* Avoid loading the whole file into memory. *)
  (* Use bash if it exists to enable the [command] to be an exported
     bash function. *)
  let want_bash = Sys.os_type = "Unix" && Sys.file_exists "/bin/bash" in
  let f =
    if want_bash then (
      fun row ->
      let cmd = String.concat " " (command :: List.map Filename.quote row) in
      let pid = Unix.create_process "/bin/bash" [| "bash"; "-c"; cmd |]
                  Unix.stdin Unix.stdout Unix.stderr in
      match snd(Unix.waitpid [] pid) with
      | Unix.WEXITED code ->
         if code <> 0 then (
           eprintf "%s: terminated with exit code %d\n" command code;
           exit code
         )
      | Unix.WSIGNALED sg ->
         eprintf "%s: killed by signal %d\n" command sg;
         exit 1
      | Unix.WSTOPPED sg ->
         eprintf "%s: stopped by signal %d\n" command sg;
         exit 1
    )
    else (
      fun row ->
      let cmd = String.concat " " (command :: List.map Filename.quote row) in
      let code = Sys.command cmd in
      if code <> 0 then (
        eprintf "%s: terminated with exit code %d\n" command code;
        exit code
    ))
  in
  iter_csv_rows ~input_sep ~f files

let cmd_join ~input_sep ~output_sep ~chan colspec1 colspec2 files =
  (* Load in the files separately. *)
  let csvs = List.map (Csv.load ~separator:input_sep) files in

  (* For each CSV file, construct a hash table from row class (key) to
   * the (possibly empty) output columns (values).
   * Also construct a hash which has the unique list of row classes.
   *)
  let keys = Hashtbl.create 1023 in
  let hashes = List.map (
    fun csv ->
      let hash = Hashtbl.create 1023 in
      List.iter (
        fun row ->
          let key = cols_of_colspec colspec1 row in
          let value = cols_of_colspec colspec2 row in
          if not (Hashtbl.mem keys key) then Hashtbl.add keys key true;
          Hashtbl.add hash key value
      ) csv;
      hash
  ) csvs in

  (* Get the keys. *)
  let keys = Hashtbl.fold (fun key _ xs -> key :: xs) keys [] in

  let value_width = width_of_colspec colspec2 in
  let empty_value =
    List.hd (Csv.set_columns ~cols:value_width [[""]]) in
  let multiple_values =
    List.hd (Csv.set_columns ~cols:value_width [["!MULTIPLE VALUES"]]) in

  (* Generate output CSV. *)
  let keys = List.sort Pervasives.compare keys in
  let keys = List.map (fun key -> key, []) keys in
  let csv = List.fold_left (
    fun keys hash ->
      List.map (
        fun (key, values) ->
          let value = try Hashtbl.find_all hash key with Not_found -> [] in
          let value =
            match value with
            | [] -> empty_value
            | [value] -> value
            | _::_ -> multiple_values in
          key, (value :: values)
      ) keys
  ) keys hashes in
  let csv = List.map (
    fun (key, values) ->
      key @ List.flatten (List.rev values)
  ) csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

let cmd_trim ~input_sep ~output_sep ~chan (top, left, right, bottom) files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.trim ~top ~left ~right ~bottom csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

and trim_flags flags =
  let set c =
    try ignore (String.index flags c); true with Not_found -> false
  in
  let top = set 't' in
  let left = set 'l' in
  let right = set 'r' in
  let bottom = set 'b' in
  (top, left, right, bottom)

(* Process the arguments. *)
let usage =
  "csvtool - Copyright (C) 2005-2006 Richard W.M. Jones, Merjis Ltd.
           - Copyright (C) 2007- Richard W.M. Jones & Christophe Troestler

csvtool is a tool for performing manipulations on CSV files from shell scripts.

Summary:
  csvtool [-options] command [command-args] input.csv [input2.csv [...]]

Commands:
  col <column-spec>
    Return one or more columns from the CSV file.

    For <column-spec>, see below.

      Example: csvtool col 1-3,6 input.csv > output.csv

  namedcol <names>
    Assuming the first row of the CSV file is a list of column headings,
    this returned the column(s) with the named headings.

    <names> is a comma-separated list of names.

      Example: csvtool namedcol Account,Cost input.csv > output.csv

  width
    Print the maximum width of the CSV file (number of columns in the
    widest row).

  height
    Print the number of rows in the CSV file.

    For most CSV files this is equivalent to 'wc -l', but note that
    some CSV files can contain a row which breaks over two (or more)
    lines.

  setcolumns cols
    Set the number of columns to cols (this also makes the CSV file
    square).  Any short rows are padding with blank cells.  Any
    long rows are truncated.

  setrows rows
    'setrows n' sets the number of rows to 'n'.  If there are fewer
    than 'n' rows in the CSV files, then empty blank lines are added.

  head rows
  take rows
    'head n' and 'take n' (which are synonyms) take the first 'n'
    rows.  If there are fewer than 'n' rows, padding is not added.

  drop rows
    Drop the first 'rows' rows and return the rest (if any).

      Example:
        To remove the headings from a CSV file with headings:
          csvtool drop 1 input.csv > output.csv

        To extract rows 11 through 20 from a file:
          csvtool drop 10 input.csv | csvtool take 10 - > output.csv

  cat
    This concatenates the input files together and writes them to
    the output.  You can use this to change the separator character.

      Example: csvtool -t TAB -u COMMA cat input.tsv > output.csv

  paste
    Concatenate the columns of the files together and write them to the
    output.

      Example: csvtool paste input1.csv input2.csv > output.csv

  pastecol <column-spec1> <column-spec2> input.csv update.csv
    Replace the content of the columns referenced by <column-spec1> in the
    file input.csv with the one of the corresponding column specified by
    <column-spec2> in update.csv.

      Example: csvtool pastecol 2-3 1- input.csv update.csv.csv > output.csv

  join <column-spec1> <column-spec2>
    Join (collate) multiple CSV files together.

    <column-spec1> controls which columns are compared.

    <column-spec2> controls which columns are copied into the new file.

      Example:
        csvtool join 1 2 coll1.csv coll2.csv > output.csv

        In the above example, if coll1.csv contains:
          Computers,$40
          Software,$100
        and coll2.csv contains:
          Computers,$50
        then the output will be:
          Computers,$40,$50
          Software,$100,

  square
    Make the CSV square, so all rows have the same length.

      Example: csvtool square input.csv > input-square.csv

  trim [tlrb]+
    Trim empty cells at the top/left/right/bottom of the CSV file.

      Example:
        csvtool trim t input.csv    # trims empty rows at the top only
        csvtool trim tb input.csv   # trims empty rows at the top & bottom
        csvtool trim lr input.csv   # trims empty columns at left & right
        csvtool trim tlrb input.csv # trims empty rows/columns all around

  sub r c rows cols
    Take a square subset of the CSV, top left at row r, column c, which
    is rows deep and cols wide.  'r' and 'c' count from 1, or
    from 0 if -z option is given.

  replace <column-spec> update.csv original.csv
    Replace rows in original.csv with rows from update.csv.  The columns
    in <column-spec> only are used to compare rows in input.csv and
    update.csv to see if they are candidates for replacement.

      Example:
        csvtool replace 3 updates.csv original.csv > new.csv
        mv new.csv original.csv

  transpose input.csv
    Transpose the lines and columns of the CSV file.

  format fmt
    Print each row of the files according to the format 'fmt'.
    Each occurrence of \"%i\" or \"%(i)\" (where 'i' is a number) in
    'fmt' is replaced by the content of column number 'i' (remember
    that the leftmost column is numbered 1 in the traditional
    spreadsheet fashion).  A literal percent is obtained by doubling it.
    The usual escape sequences \\n, \\r, and \\t are recognized.

      Example:
        csvtool format '%(1) -> %8%%\\n' input.csv

  call command
    This calls the external command (or shell function) 'command'
    followed by a parameter for each column in the CSV file.  The
    external command is called once for each row in the CSV file.
    If any command returns a non-zero exit code then the whole
    program terminates.

      Tip:
        Use the shell command 'export -f funcname' to export
        a shell function for use as a command.  Within the
        function, use the positional parameters $1, $2, ...
        to refer to the columns.

      Example (with a shell function):
        function test {
          echo Column 1: $1
          echo Column 2: $2
        }
        export -f test
        csvtool call test my.csv

        In the above example, if my.csv contains:
          how,now
          brown,cow
        then the output is:
          Column 1: how
          Column 2: now
          Column 1: brown
          Column 2: cow

  readable
    Print the input CSV in a readable format.

Column specs:
  A <column-spec> is a comma-separated list of column numbers
  or column ranges.

    Examples:
      1                       Column 1 (the first, leftmost column)
      2,5,7                   Columns 2, 5 and 7
      1-3,5                   Columns 1, 2, 3 and 5
      1,5-                    Columns 1, 5 and up.

  Columns are numbered starting from 1 unless the -z option is given.

Input files:
  csvtool takes a list of input file(s) from the command line.

  If an input filename is '-' then take input from stdin.

Output file:
  Normally the output is written to stdout.  Use the -o option
  to override this.

Separators:
  The default separator character is , (comma).  To change this
  on input or output see the -t and -u options respectively.

  Use -t TAB or -u TAB (literally T-A-B!) to specify tab-separated
  files.

Options:"

let () =
  let input_sep = ref ',' in
  let set_input_sep = function
    | "TAB" -> input_sep := '\t'
    | "COMMA" -> input_sep := ','
    | "SPACE" -> input_sep := ' '
    | s -> input_sep := s.[0]
  in

  let output_sep = ref ',' in
  let set_output_sep = function
    | "TAB" -> output_sep := '\t'
    | "COMMA" -> output_sep := ','
    | "SPACE" -> output_sep := ' '
    | s -> output_sep := s.[0]
  in

  let count_zero = ref false in

  let output_file = ref "" in

  let rest = ref [] in
  let set_rest str =
    rest := str :: !rest
  in

  let argspec = [
    "-t", Arg.String set_input_sep,
    "Input separator char.  Use -t TAB for tab separated input.";
    "-u", Arg.String set_output_sep,
    "Output separator char.  Use -u TAB for tab separated output.";
    "-o", Arg.Set_string output_file,
    "Write output to file (instead of stdout)";
    "-z", Arg.Set count_zero,
    "Number columns from 0 instead of 1";
    "-", Arg.Unit (fun () -> set_rest "-"),
    "" (* Hack to allow '-' for input from stdin. *)
  ] in

  Arg.parse argspec set_rest usage;

  let input_sep = !input_sep in
  let output_sep = !output_sep in
  let count_zero = !count_zero in
  let output_file = !output_file in
  let rest = List.rev !rest in

  (* Set up the output file. *)
  let chan =
    if output_file <> "" then open_out output_file
    else stdout in

  (match rest with
     | ("col"|"cols") :: colspec :: files ->
         let colspec = parse_colspec ~count_zero colspec in
         cmd_cols ~input_sep ~output_sep ~chan colspec files
     | ("namedcol"|"namedcols") :: names :: files ->
         let names = nsplit names "," in
         cmd_namedcols ~input_sep ~output_sep ~chan names files
     | ("width"|"columns") :: files ->
         cmd_width ~input_sep ~chan files
     | ("height"|"rows") :: files ->
         cmd_height ~input_sep ~chan files
     | "readable" :: files ->
         cmd_readable ~input_sep ~chan files
     | ("cat"|"concat") :: files ->
         cmd_cat ~input_sep ~output_sep ~chan files
     | "paste" :: files ->
         cmd_paste ~input_sep ~output_sep ~chan files
     | "pastecol" :: colspec1 :: colspec2 :: file1 :: file2 :: _ ->
         let colspec1 = parse_colspec ~count_zero colspec1 in
         let colspec2 = parse_colspec ~count_zero colspec2 in
         cmd_pastecol ~input_sep ~output_sep ~chan colspec1 colspec2 file1 file2
     | ("join"|"collate") :: colspec1 :: colspec2 :: ((_::_::_) as files) ->
         let colspec1 = parse_colspec ~count_zero colspec1 in
         let colspec2 = parse_colspec ~count_zero colspec2 in
         cmd_join ~input_sep ~output_sep ~chan colspec1 colspec2 files
     | "square" :: files ->
         cmd_square ~input_sep ~output_sep ~chan files
     | "sub" :: r :: c :: rows :: cols :: files ->
         let r = int_of_string r in
         let r = if not count_zero then r-1 else r in
         let c = int_of_string c in
         let c = if not count_zero then c-1 else c in
         let rows = int_of_string rows in
         let cols = int_of_string cols in
         cmd_sub ~input_sep ~output_sep ~chan r c rows cols files
     | "replace" :: colspec :: update :: files ->
         let colspec = parse_colspec ~count_zero colspec in
         cmd_replace ~input_sep ~output_sep ~chan colspec update files
     | ("setcolumns"|"set_columns"|"set-columns"|
            "setcols"|"set_cols"|"set-cols") :: cols :: files ->
         let cols = int_of_string cols in
         cmd_set_columns ~input_sep ~output_sep ~chan cols files
     | ("setrows"|"set_rows"|"set-rows") :: rows :: files ->
         let rows = int_of_string rows in
         cmd_set_rows ~input_sep ~output_sep ~chan rows files
     | ("head"|"take") :: rows :: files ->
         let rows = int_of_string rows in
         cmd_head ~input_sep ~output_sep ~chan rows files
     | "drop" :: rows :: files ->
         let rows = int_of_string rows in
         cmd_drop ~input_sep ~output_sep ~chan rows files
     | "transpose" :: files ->
         cmd_transpose ~input_sep ~output_sep ~chan files
     | "format" :: fmt :: files ->
        cmd_format ~input_sep fmt files
     | "call" :: command :: files ->
         cmd_call ~input_sep command files
     | "trim" :: flags :: files ->
         let flags = trim_flags flags in
         cmd_trim ~input_sep ~output_sep ~chan flags files
     | _ ->
         prerr_endline (Sys.executable_name ^ " --help for usage");
         exit 2
  );

  if output_file <> "" then close_out chan
