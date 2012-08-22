(* File: csv.mli

   Copyright (C) 2006

     Richard Jones
     email: rjones@redhat.com

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Read and write the CSV (comma separated values) format.

    @author Richard Jones <rjones\@redhat.com>
    @author Christophe Troestler <Christophe.Troestler\@umons.ac.be>
 *)


type t = string list list
(** Representation of CSV data in memory. *)


(** {2 Input/output objects} *)

(** The most basic input object for best interoperability. *)
class type in_obj_channel =
object
  method input : string -> int -> int -> int
    (** [input buf ofs len] reads up to [len] octets from the channel
        and puts them in the substring [buf.[ofs .. ofs+len-1]].
        Returns the number of octets actually read (and stored).  When
        the channel is non-blocking, and there are currently no bytes
        to read, the number 0 will be returned.

        @raise End_of_file when there are no more bytes to read. *)
  method close_in : unit -> unit
    (** Closes the channel for input. *)
end

(** The most basic output object for best interoperability. *)
class type out_obj_channel =
object
  method output : string -> int -> int -> int
    (** [output s ofs len] writes up to [len] bytes of the substring
        [s.[ofs .. ofs+len-1]].  Return the number of bytes actually
        written.  When the channel is non-blocking, and there are
        currently no bytes to write, the number 0 must be returned.  *)
  method close_out : unit -> unit
    (** Flushes the buffer, if any, and closes the channel for output. *)
end



(** {2 Input} *)

exception Failure of int * int * string
  (** [Failure(nrecord, nfield, msg)] is raised to indicate a parsing
      error for the field number [nfield] on the record number
      [nrecord], the description [msg] says what is wrong. *)

type in_channel
(** Stateful handle to input CSV files. *)

val of_in_obj : ?separator:char -> ?excel_tricks:bool ->
  in_obj_channel -> in_channel
(** [of_in_obj ?separator ?excel_tricks in_chan] creates a new "channel"
    to access the data in CSV form available from the channel [in_chan].

    @param separator What character the separator is.  The default is
    [','].  You should be aware however that, in the countries where
    comma is used as a decimal separator, Excel will use [';'] as the
    separator.

    @param excel_tricks enables Excel tricks, namely the fact that '"'
    followed by '0' in a quoted string means ASCII NULL and the fact
    that a field of the form ="..." only returns the string inside the
    quotes.  Default: [true].
*)

val of_channel : ?separator:char -> ?excel_tricks:bool ->
  Pervasives.in_channel -> in_channel
  (** Same as {!Csv.of_in_obj} except that the data is read from a
      standard channel. *)

val load : ?separator:char -> ?excel_tricks:bool-> string -> t
  (** [load fname] loads the CSV file [fname].  If [filename] is ["-"]
      then load from [stdin].

      @param separator What character the separator is.  The default
      is [','].  You should be aware however that, in the countries
      where comma is used as a decimal separator, Excel will use [';']
      as the separator.

      @param excel_tricks enables Excel tricks, namely the fact that '"'
      followed by '0' in a quoted string means ASCII NULL and the fact
      that a field of the form ="..." only returns the string inside the
      quotes.  Default: [true].  *)

val load_in : ?separator:char -> ?excel_tricks:bool ->
  Pervasives.in_channel -> t
  (** [load_in ch] loads a CSV file from the input channel [ch].
      See {!Csv.load} for the meaning of [separator] and [excel_tricks]. *)


val to_in_obj : in_channel -> in_obj_channel
  (** For efficiency reasons, the [in_channel] buffers the data from
      the original channel.  If you want to examine the data by other
      means than the methods below (say after a failure), you need to
      use this function in order not to "loose" data in the
      buffer.  *)

val close_in : in_channel -> unit
  (** [close_in ic] closes the channel [ic].  The underlying channel
      is closed as well. *)


val next : in_channel -> string list
  (** [next ic] returns the next record in the CSV file.

      @raise End_of_file if no more record can be read.

      @raise Csv.Failure if the CSV format is not respected.  The
      partial record read is available with [#current_record]. *)

val fold_left : ('a -> string list -> 'a) -> 'a -> in_channel -> 'a
  (** [fold_left f a ic] computes (f ... (f (f a r0) r1) ... rN)
      where r1,...,rN are the records in the CSV file.  If [f]
      raises an exception, the record available at that moment is
      accessible through {!Csv.current_record}. *)

val fold_right : (string list -> 'a -> 'a) -> in_channel -> 'a -> 'a
  (** [fold_right f ic a] computes (f r1 ... (f rN-1 (f rN a)) ...)
      where r1,...,rN-1, rN are the records in the CSV file.  All
      records are read before applying [f] so this method is not
      convenient if your file is large. *)

val iter : f:(string list -> unit) -> in_channel -> unit
  (** [iter f ic] iterates [f] on all remaining records.  If [f]
      raises an exception, the record available at that moment is
      accessible through {!Csv.current_record}. *)

val input_all : in_channel -> t
  (** [input_all ic] return a list of the CSV records till the end of
      the file. *)

val current_record : in_channel -> string list
  (** The current record under examination.  This is useful in order
      to gather the parsed data in case of [Failure]. *)


val load_rows : ?separator:char -> ?excel_tricks:bool ->
  (string list -> unit) -> Pervasives.in_channel -> unit
  (** @deprecated use {!Csv.iter} on a {!Csv.in_channel} created with
      {!Csv.of_channel}. *)

(************************************************************************)
(** {2 Output} *)

type out_channel

val to_out_obj : ?separator:char -> ?excel_tricks:bool ->
  out_obj_channel -> out_channel
  (** [to_out_obj ?separator ?excel_tricks out_chan] creates a new "channel"
      to output the data in CSV form.

      @param separator What character the separator is.  The default is [','].

      @param excel_tricks enables Excel tricks, namely the fact that
      '\000' is represented as '"' followed by '0' and the fact that a
      field with leading or trailing spaces or a leading '0' will be
      encoded as ="..."  (to avoid Excel "helping" you).  Default:
      [false].  *)

val to_channel : ?separator:char -> ?excel_tricks:bool ->
  Pervasives.out_channel -> out_channel
  (** Same as {!Csv.to_out_obj} but output to a standard channel. *)


val output_record : out_channel -> string list -> unit
  (** [output_record oc r] write the record [r] is CSV form to the
      channel [oc]. *)

val output_all : out_channel -> t -> unit
  (** [output_all oc csv] outputs all records in [csv] to the channel
      [oc]. *)

val save_out : ?separator:char -> ?excel_tricks:bool ->
  Pervasives.out_channel -> t -> unit
  (** @deprecated Save string list list to a channel. *)

val save : ?separator:char -> ?excel_tricks:bool -> string -> t -> unit
  (** [save fname csv] Save the [csv] data to the file [fname]. *)

val print : ?separator:char -> ?excel_tricks:bool -> t -> unit
  (** Print the CSV data. *)

val print_readable : t -> unit
  (** Print the CSV data to [stdout] in a human-readable format.  Not
      much is guaranteed about how the CSV is printed, except that it
      will be easier to follow than a "raw" output done with
      {!Csv.print}.  This is a one-way operation.  There is no easy way
      to parse the output of this command back into CSV data.  *)

val save_out_readable : Pervasives.out_channel -> t -> unit
  (** As for {!Csv.print_readable}, allowing the output to be sent to
      a channel.  *)


(************************************************************************)
(** {2 Functions acting on CSV data loaded in memory} *)

val lines : t -> int
  (** Return the number of lines in a CSV data. *)

val columns : t -> int
  (** Work out the (maximum) number of columns in a CSV file.  Note
      that each line may be a different length, so this finds the one
      with the most columns.  *)

val trim : ?top:bool -> ?left:bool -> ?right:bool -> ?bottom:bool -> t -> t
  (** This takes a CSV file and trims empty cells.
   *
   * All four of the option arguments ([~top], [~left], [~right], [~bottom])
   * default to [true].
   *
   * The exact behaviour is:
   *
   * [~right]: If true, remove any empty cells at the right hand end of
   * any row.  The number of columns in the resulting CSV structure will
   * not necessarily be the same for each row.
   *
   * [~top]: If true, remove any empty rows (no cells, or containing just empty
   * cells) from the top of the CSV structure.
   *
   * [~bottom]: If true, remove any empty rows from the bottom of the
   * CSV structure.
   *
   * [~left]: If true, remove any empty columns from the left of the
   * CSV structure.  Note that [~left] and [~right] are quite different:
   * [~left] considers the whole CSV structure, whereas [~right] considers
   * each row in isolation.
   *)


val square : t -> t
  (** Make the CSV data "square" (actually rectangular).  This pads
      out each row with empty cells so that all rows are the same
      length as the longest row.  After this operation, every row will
      have length {!Csv.columns}.  *)

val is_square : t -> bool
  (** Return true iff the CSV is "square" (actually rectangular).
      This means that each row has the same number of cells.  *)

val set_columns : int -> t -> t
  (** [set_columns cols csv] makes the CSV data square by forcing the
      width to the given number of [cols].  Any short rows are padded
      with blank cells.  Any long rows are truncated.  *)

val set_rows : int -> t -> t
  (** [set_rows rows csv] makes the CSV data have exactly [rows] rows
      by adding empty rows or truncating rows as necessary.

      Note that [set_rows] does not make the CSV square.  If you want it
      to be square, call either {!Csv.square} or {!Csv.set_columns}
      after.  *)

val set_size : int -> int -> t -> t
  (** [set_size rows cols csv] makes the CSV data square by forcing
      the size to [rows * cols], adding blank cells or truncating as
      necessary.  It is the same as calling [set_columns cols
      (set_rows rows csv)] *)

val sub : int -> int -> int -> int -> t -> t
  (** [sub r c rows cols csv] returns a subset of [csv].  The subset is
      defined as having top left corner at row [r], column [c] (counting
      from [0]) and being [rows] deep and [cols] wide.

      The returned CSV will be "square".  *)

val compare : t -> t -> int
  (** Compare two CSV files for equality, ignoring blank cells at the
      end of a row, and empty rows appended to one or the other.  This
      is "semantic" equality - roughly speaking, the two CSV files
      would look the same if opened in a spreadsheet program.  *)

val concat : t list -> t
  (** Concatenate CSV files so that they appear side by side, arranged
      left to right across the page.  Each CSV file (except the final
      one) is first squared.

      (To concatenate CSV files so that they appear from top to
      bottom, just use [List.concat]).  *)

val to_array : t -> string array array
val of_array : string array array -> t
  (** Convenience functions to convert to and from a matrix
   representation.  [to_array] will produce a ragged matrix (not all
   rows will have the same length) unless you call {!Csv.square}
   first.  *)

val associate : string list -> t -> (string * string) list list
(** [associate header data] takes a block of data and converts each
  * row in turn into an assoc list which maps column header to data cell.
  *
  * Typically a spreadsheet will have the format:
  * {v
  *   header1   header2   header3
  *   data11    data12    data13
  *   data21    data22    data23
  *     ...
  * v}
  *
  * This function arranges the data into a more usable form which is
  * robust against changes in column ordering.  The output of the
  * function is:
  * {v
  *   [ ["header1", "data11"; "header2", "data12"; "header3", "data13"];
  *     ["header1", "data21"; "header2", "data22"; "header3", "data23"];
  *     etc. ]
  * v}
  *
  * Each row is turned into an assoc list (see [List.assoc]).
  *
  * If a row is too short, it is padded with empty cells ([""]).  If
  * a row is too long, it is truncated.
  *
  * You would typically call this function as:
  *
  * {v
  * let header, data = match csv with h :: d -> h, d | [] -> assert false;;
  * let data = Csv.associate header data;;
  * v}
  *
  * The header strings are shared, so the actual space in memory consumed
  * by the spreadsheet is not much larger.
  *)
