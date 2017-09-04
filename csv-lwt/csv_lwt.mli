(* File: csv_lwt.mli

   Copyright (C) 2017-

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

val of_channel : ?separator:char -> ?strip: bool ->
                 ?has_header: bool -> ?header: string list ->
                 ?backslash_escape: bool -> ?excel_tricks:bool ->
                 ?fix:bool ->
                 Lwt_io.input_channel -> in_channel Lwt.t
(** See {!Csv.of_in_obj}. *)

val load : ?separator:char -> ?strip: bool ->
           ?backslash_escape: bool -> ?excel_tricks:bool -> ?fix:bool ->
           string -> t Lwt.t
(** See {!Csv.load} *)

val load_in : ?separator:char -> ?strip: bool ->
              ?backslash_escape: bool -> ?excel_tricks:bool -> ?fix:bool ->
              Lwt_io.input_channel -> t Lwt.t
(** See {!Csv.load_in}. *)

val close_in : in_channel -> unit Lwt.t
(** [close_in ic] closes the channel [ic].  The underlying channel is
   closed as well. *)

val next : in_channel -> string list Lwt.t
(** See {!Csv.next} *)

val fold_left : f:('a -> string list -> 'a Lwt.t) ->
                init:'a -> in_channel -> 'a Lwt.t
(** See {!Csv.fold_left}. *)

val fold_right : f:(string list -> 'a -> 'a Lwt.t) -> in_channel -> 'a -> 'a Lwt.t
(** See {!Csv.fold_right}. *)

val iter : f:(string list -> unit Lwt.t) -> in_channel -> unit Lwt.t
(** See {!Csv.inter}. *)

val current_record : in_channel -> string list
(** See {!Csv.current_record}. *)


(** {2 Output} *)

type out_channel

val to_channel : ?separator:char ->
                 ?backslash_escape: bool -> ?excel_tricks:bool ->
                 ?quote_all:bool ->
                 Lwt_io.output_channel -> out_channel
(** See {!Csv.to_channel}. *)

val close_out : out_channel -> unit Lwt.t
(** See {!Csv.close_out}. *)

val output_record : out_channel -> string list -> unit Lwt.t
(** See {!Csv.output_record}. *)

val output_all : out_channel -> t -> unit Lwt.t
(** See {!Csv.output_all}. *)

val save : ?separator:char -> ?backslash_escape: bool -> ?excel_tricks:bool ->
           ?quote_all:bool ->
           string -> t -> unit Lwt.t
(** See {!Csv.save}. *)

val print : ?separator:char -> ?backslash_escape: bool -> ?excel_tricks:bool ->
            ?quote_all:bool ->
            t -> unit Lwt.t
(** See {!Csv.print}. *)


(** {2 Functions to access rows when a header is present} *)

(** Represent a row with header.  Compatible with {!Csv.Row}. *)
module Row : module type of Csv.Row

module Rows : sig
  val header : in_channel -> string list
  (** The header declared for this channel.  *)

  val set_header : ?replace: bool -> in_channel -> string list -> unit
  (** See {!Csv.Rows.set_header}. *)

  val next : in_channel -> Row.t Lwt.t
  (** See {!Csv.Rows.next}. *)

  val fold_left : f:('a -> Row.t -> 'a Lwt.t) ->
                  init:'a -> in_channel -> 'a Lwt.t
  (** See {!Csv.fold_left}. *)

  val fold_right : f:(Row.t -> 'a -> 'a Lwt.t) -> in_channel -> 'a -> 'a Lwt.t
  (** See {!Csv.fold_right}. *)

  val iter : f:(Row.t -> unit Lwt.t) -> in_channel -> unit Lwt.t
  (** See {!Csv.iter}. *)

  val input_all : in_channel -> Row.t list Lwt.t
  (** See {!Csv.input_all}. *)

  val load : ?separator:char -> ?strip: bool ->
             ?has_header: bool -> ?header: string list ->
             ?backslash_escape: bool -> ?excel_tricks:bool ->
             ?fix: bool ->
             string -> Row.t list Lwt.t
  (** See {!Csv.load}. *)

  val current : in_channel -> Row.t
  (** See {!Csv.current_record}. *)
end

;;
