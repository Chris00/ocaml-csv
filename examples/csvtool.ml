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
        Failure "int_of_string" ->
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
  | Col c :: rest -> 1 + width_of_colspec rest
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
         with Failure "nth" -> "") :: loop rest
    | Range (s, e) :: rest ->
        let width = e-s+1 in
        let range = take width (drop s row) in
        let range = List.hd (Csv.set_columns width [range]) in
        List.append range (loop rest)
    | ToEnd e :: rest ->
        List.append (drop e row) (loop rest)
  in
  loop colspec

(* The actual commands. *)
let cmd_cols ~input_sep ~output_sep ~chan colspec files =
  List.iter (
    fun filename ->
      let csv = Csv.load ~separator:input_sep filename in
      let csv = List.map (cols_of_colspec colspec) csv in
      Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv
  ) files

let cmd_namedcols ~input_sep ~output_sep ~chan names files =
  List.iter (
    fun filename ->
      let csv = Csv.load ~separator:input_sep filename in
      let header, data =
        match csv with
        | [] -> failwith "no rows in this CSV file"
        | h :: t -> h, t in
      (* Do the headers requested exist in the CSV file?  If not,
       * throw an error.
       *)
      List.iter (
        fun name ->
          if not (List.mem name header) then
            failwith ("namedcol: requested header not in CSV file: " ^ name)
      ) names;
      let data = Csv.associate header data in
      let data = List.map (
        fun row -> List.map (fun name -> List.assoc name row) names
      ) data in
      Csv.output_all (Csv.to_channel ~separator:output_sep chan) data
  ) files

let cmd_width ~input_sep ~chan files =
  let width = List.fold_left (
    fun width filename ->
      let csv = Csv.load ~separator:input_sep filename in
      let width = max width (Csv.columns csv) in
      width
  ) 0 files in
  fprintf chan "%d\n" width

let cmd_height ~input_sep ~chan files =
  let height = List.fold_left (
    fun height filename ->
      let csv = Csv.load ~separator:input_sep filename in
      let height = height + Csv.lines csv in
      height
  ) 0 files in
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
  List.iter (
    fun filename ->
      let in_chan, close =
        match filename with
        | "-" -> stdin, false
        | filename -> open_in filename, true in
      Csv.iter f (Csv.of_channel ~separator:input_sep in_chan);
      if close then close_in in_chan
  ) files

let cmd_set_columns ~input_sep ~output_sep ~chan cols files =
  (* Avoid loading the whole file into memory. *)
  let f row =
    let csv = [row] in
    let csv = Csv.set_columns cols csv in
    Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv
  in
  List.iter (
    fun filename ->
      let in_chan, close =
        match filename with
        | "-" -> stdin, false
        | filename -> open_in filename, true in
      Csv.iter f (Csv.of_channel ~separator:input_sep in_chan);
      if close then close_in in_chan
  ) files

let cmd_set_rows ~input_sep ~output_sep ~chan rows files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.set_rows rows csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

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
  in
  List.iter (
    fun filename ->
      if !nr_rows > 0 then (
        let in_chan, close =
          match filename with
          | "-" -> stdin, false
          | filename -> open_in filename, true in
        Csv.iter f (Csv.of_channel ~separator:input_sep in_chan);
        if close then close_in in_chan
      )
  ) files

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
  List.iter (
    fun filename ->
      let in_chan, close =
        match filename with
        | "-" -> stdin, false
        | filename -> open_in filename, true in
      Csv.iter f (Csv.of_channel ~separator:input_sep in_chan);
      if close then close_in in_chan
  ) files

let cmd_square ~input_sep ~output_sep ~chan files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.square csv in
  Csv.output_all (Csv.to_channel ~separator:output_sep chan) csv

let cmd_sub ~input_sep ~output_sep ~chan r c rows cols files =
  let csv = List.concat (List.map (Csv.load ~separator:input_sep) files) in
  let csv = Csv.sub r c rows cols csv in
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

let cmd_call ~input_sep ~output_sep ~chan command files =
  (* Avoid loading the whole file into memory. *)
  let f row =
    let cmd =
      command ^ " " ^ String.concat " " (List.map Filename.quote row) in
    let code = Sys.command cmd in
    if code <> 0 then (
      eprintf "%s: terminated with exit code %d\n" command code;
      exit code
    )
  in
  List.iter (
    fun filename ->
      let in_chan, close =
        match filename with
        | "-" -> stdin, false
        | filename -> open_in filename, true in
      Csv.iter f (Csv.of_channel ~separator:input_sep in_chan);
      if close then close_in in_chan
  ) files

let rec uniq = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: xs when Pervasives.compare x y = 0 ->
      uniq (x :: xs)
  | x :: y :: xs ->
      x :: uniq (y :: xs)

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
    List.hd (Csv.set_columns value_width [[""]]) in
  let multiple_values =
    List.hd (Csv.set_columns value_width [["!MULTIPLE VALUES"]]) in

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

let rec cmd_trim ~input_sep ~output_sep ~chan (top, left, right, bottom) files =
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
     | "call" :: command :: files ->
         cmd_call ~input_sep ~output_sep ~chan command files
     | "trim" :: flags :: files ->
         let flags = trim_flags flags in
         cmd_trim ~input_sep ~output_sep ~chan flags files
     | _ ->
         prerr_endline (Sys.executable_name ^ " --help for usage");
         exit 2
  );

  if output_file <> "" then close_out chan
