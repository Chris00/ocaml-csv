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
      [nrecord].  [msg] is a description of what is wrong. *)

(** [new in_channel ?delim in_chan] creates a new "channel" to access
    the data in CSV form available from the channel [in_chan].

    @param delim What character the delimiter is.  The default is
    [','].  You should be aware however that, in the countries where
    comma is used as a decimal separator, Excel will use [';'] as the
    separator. *)
class in_channel : ?delim:char -> ?excel_tricks:bool -> in_obj_channel ->
object

  method next : unit -> string list
    (** [#next()] returns the next record in the CSV file.

        @raise End_of_file if no more record can be read.

        @raise Csv.Failure if the CSV format is not respected.  The
        partial record read is available with [#current_record]. *)

  method fold_left : ('a -> string list -> 'a) -> 'a -> 'a
    (** [#fold_left f a] computes (f ... (f (f a r0) r1) ... rN) where
        r1,...,rN are the records in the CSV file.  If [f] raises an
        exception, the record available at that moment is accessible
        through [#current_record]. *)

  method fold_right : (string list -> 'a -> 'a) -> 'a -> 'a
    (** [#fold_left f a] computes (f r1 ... (f rN-1 (f rN a)) ...)
        where r1,...,rN-1, rN are the records in the CSV file.  All
        records are read before applying [f] so this method is not
        convenient if you file is large. *)

  method current_record : string list
    (** The current record under examination.  This is useful in order
        to gather the parsed data in case of [Failure]. *)

  method input : string -> int -> int -> int
    (** See {!Csv.in_obj_channel.input}.  For efficiency reasons, this
        object buffers the data from the original channel.  If you
        want to examine the data by other means than the methods above
        (say after a failure), you need to use this method in order
        not to "loose" data in the buffer.

        @raise Sys_error if the channel is closed.
        @raise Invalid_argument if the parameters do not specify a
        valid substring. *)

  method close_in : unit -> unit
    (** Closes the channel for input (the original channel is also
        closed). *)
end


class of_channel : ?delim:char -> ?excel_tricks:bool ->
  Pervasives.in_channel -> in_channel


(** {2 Output} *)

class out_channel : ?delim:char -> ?excel_tricks:bool -> out_obj_channel ->
  object
    method write_record : string list -> bool
      (** [#write_row r] *)

    method close_out : unit -> unit
      (** Flushes the buffer, if any, and closes the channel for output. *)
  end

class to_channel : ?delim:char -> ?excel_tricks:bool ->
  Pervasives.out_channel -> out_channel
