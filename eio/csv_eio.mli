(* File: csv_eio.mli

   Copyright (C) 2023-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Lwt interface to the CSV library.

   This module only offers Lwt input/output functions for CSV files.
   {!Csv} provides additional functions to transform CSV data. *)

type t = Csv.t


(** {2 Input} *)

type in_channel
(** Stateful handle to input CSV files. *)

val of_source : ?separator:char -> ?strip: bool ->
                 ?has_header: bool -> ?header: string list ->
                 ?backslash_escape: bool -> ?excel_tricks:bool ->
                 ?fix:bool -> ?skip_bom:bool ->
                 _ Eio.Flow.source -> in_channel
(** See {!Csv.of_in_obj}. *)

val load : ?separator:char -> ?strip: bool ->
           ?backslash_escape: bool -> ?excel_tricks:bool -> ?fix:bool ->
           ?skip_bom:bool -> _ Eio.Path.t -> t
(** See {!Csv.load} *)

val load_in : ?separator:char -> ?strip: bool ->
              ?backslash_escape: bool -> ?excel_tricks:bool -> ?fix:bool ->
              ?skip_bom:bool -> _ Eio.Flow.source -> t
(** See {!Csv.load_in}. *)

val next : in_channel -> string list
(** See {!Csv.next} *)

val fold_left : f:('a -> string list -> 'a) ->
                init:'a -> in_channel -> 'a
(** See {!Csv.fold_left}. *)

val fold_right : f:(string list -> 'a -> 'a) -> in_channel -> 'a -> 'a
(** See {!Csv.fold_right}. *)

val iter : f:(string list -> unit) -> in_channel -> unit
(** See {!Csv.inter}. *)

val current_record : in_channel -> string list
(** See {!Csv.current_record}. *)


(** {2 Output} *)

type out_channel

val to_channel : ?separator:char ->
                 ?backslash_escape: bool -> ?excel_tricks:bool ->
                 ?quote_all:bool ->
                 Eio.Buf_write.t -> out_channel
(** See {!Csv.to_channel}. *)

val output_record : out_channel -> string list -> unit
(** See {!Csv.output_record}. *)

val output_all : out_channel -> t -> unit
(** See {!Csv.output_all}. *)

val save : ?separator:char -> ?backslash_escape: bool -> ?excel_tricks:bool ->
           ?quote_all:bool ->
           _ Eio.Path.t -> t -> unit
(** See {!Csv.save}. *)

val print : ?separator:char -> ?backslash_escape: bool -> ?excel_tricks:bool ->
            ?quote_all:bool ->
            stdout:_ Eio.Flow.sink -> t -> unit
(** See {!Csv.print}. *)


(** {2 Functions to access rows when a header is present} *)

(** Represent a row with header.  Compatible with {!Csv.Row}. *)
module Row : module type of Csv.Row

module Rows : sig
  val header : in_channel -> string list
  (** The header declared for this channel.  *)

  val set_header : ?replace: bool -> in_channel -> string list -> unit
  (** See {!Csv.Rows.set_header}. *)

  val next : in_channel -> Row.t
  (** See {!Csv.Rows.next}. *)

  val fold_left : f:('a -> Row.t -> 'a) ->
                  init:'a -> in_channel -> 'a
  (** See {!Csv.fold_left}. *)

  val fold_right : f:(Row.t -> 'a -> 'a) -> in_channel -> 'a -> 'a
  (** See {!Csv.fold_right}. *)

  val iter : f:(Row.t -> unit) -> in_channel -> unit
  (** See {!Csv.iter}. *)

  val input_all : in_channel -> Row.t list
  (** See {!Csv.input_all}. *)

  val load : ?separator:char -> ?strip: bool ->
             ?has_header: bool -> ?header: string list ->
             ?backslash_escape: bool -> ?excel_tricks:bool ->
             ?fix: bool -> ?skip_bom:bool ->
             _ Eio.Path.t -> Row.t list
  (** See {!Csv.load}. *)

  val current : in_channel -> Row.t
  (** See {!Csv.current_record}. *)
end

;;
