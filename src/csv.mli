(* File: csv.mli

   Copyright (C) 2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Read and write the CSV (comma separated values) format.

    @version 0.1
    @author Christophe Troestler <Christophe.Troestler\@umons.ac.be>
 *)


type t = string list list

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
    quotes.  Default: [false].
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
      quotes.  Default: [false].  *)


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


val print : ?separator:char -> ?excel_tricks:bool -> t -> unit
  (** Print string list list - same as [save_out stdout] *)
