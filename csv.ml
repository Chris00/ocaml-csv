(* File: csv.ml

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

(* MOTIVATION.  There are already several solutions to parse CSV files
   in OCaml.  They did not suit my needs however because:

   - The files I need to deal with have a header which does not always
   reflect the data structure (say the first row are the names of
   neurones but there are two columns per name).  In other words I
   want to be ablr to deal with heterogeneous files.

   - I do not want to the the whole file at once (I may but I just
   want to be able to choose).  Higher order functions like fold are
   fine provided the read stops at the line an exception is raised
   (so it can be reread again).

   - For similarly encoded line, being able to specify once a decoder
   and then use a type safe version would be nice.

   - Speed is not neglected (we would like to be able to parse a
   ~2.5Mb file under 0.1 sec on my machine).

   We follow the CVS format documentation available at
   http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm
*)


class type in_obj_channel =
object
  method input : string -> int -> int -> int
  method close_in : unit -> unit
end

class type out_obj_channel =
object
  method output : string -> int -> int -> int
  method close_out : unit -> unit
end

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y


(*
 * Input
 *)

exception Failure of int * int * string


let is_space c = c = ' ' || c = '\t' (* See documentation *)

(* Given a buffer, returns its content stripped of final whitespace. *)
let strip_contents buf =
  let n = ref(Buffer.length buf - 1) in
  while !n >= 0 && is_space(Buffer.nth buf !n) do decr n done;
  Buffer.sub buf 0 (!n + 1)


(* We buffer the input as this allows the be efficient while using
   very basic input channels.  The drawback is that if we want to use
   another tool, there will be data hold in the buffer.  That is why
   we allow to convert a CSV handle to an object sharing the same
   buffer.  Because if this, we actually decided to implement the CSV
   handle as an object that is coercible to a input-object.

   FIXME: This is not made for non-blocking channels.  Can we fix it? *)
class in_channel ?(delim=',') ?(excel_tricks=false) in_chan =
object(self)
  val in_chan = (in_chan: in_obj_channel)
  val in_buf = String.create 0x1FFF; (* 8k *)
  (* The data in the in_buf is at indexes i s.t. in0 <= i < in1.
     Invariant: 0 <= in0 ; in1 <= 0x1FFF (the buffer length)
     in1 < 0 indicates a closed channel. *)
  val mutable in0 = 0
  val mutable in1 = 0
  val mutable end_of_file = false
    (* If we encounter an End_of_file exception, we set this flag to
       avoid reading again because we do not know how the channel will
       react past an end of file.  That allows us to assume that
       reading past an end of file will keep raising End_of_file. *)
  val current_field = Buffer.create 0xFF (* buffer reused to scan fields *)
  val mutable record = [] (* The current record *)
  val mutable record_n = 0 (* For error messages *)
  val delim = delim
  val excel_tricks = excel_tricks

  method close_in() =
    if in1 >= 0 then begin
      in0 <- 0;
      in1 <- -1;
      in_chan#close_in(); (* may raise an exception *)
    end

  (* [fill_in_buf chan] refills in_buf if needed (when empty).  After
     this [in0 < in1] or [in0 = in1 = 0], the latter indicating that
     there is currently no bytes to read (for a non-blocking channel).

     @raise End_of_file if there are no more bytes to read. *)
  method private fill_in_buf =
    if end_of_file then raise End_of_file;
    if in0 >= in1 then begin
      try
        in0 <- 0;
        in1 <- in_chan#input in_buf 0 0x1FFF;
      with End_of_file ->
        end_of_file <- true;
        raise End_of_file
    end

  method private unsafe_input buf ofs len =
    self#fill_in_buf;
    let r = min len (in1 - in0) in
    String.blit in_buf in0 buf ofs r;
    in0 <- in0 + r;
    r

  method input s ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length s
    then invalid_arg "Csv.in_channel#input";
    if in1 < 0 then raise(Sys_error "Bad file descriptor");
    self#unsafe_input s ofs len

  (* Return the next character but do not mark it as read. *)
  method private unsafe_peek =
    self#fill_in_buf;
    String.unsafe_get in_buf in0

  (* Skip the possible '\n' following a '\r'.  Reaching End_of_file is
     not considered an error -- just do nothing. *)
  method private skip_CR =
    try if self#unsafe_peek = '\n' then in0 <- in0 + 1
    with End_of_file -> ()

  (* Skip all spaces: after this [in0] is a non-space char.
     @raise End_of_file *)
  method private skip_spaces =
    while in0 < in1 && is_space(String.unsafe_get in_buf in0) do
      in0 <- in0 + 1
    done;
    while in0 >= in1 do
      self#fill_in_buf;
      while in0 < in1 && is_space(String.unsafe_get in_buf in0) do
        in0 <- in0 + 1
      done;
    done

  (* Unquoted field.  Read till a delimiter, a newline, or the
     end of the file.  Skip the next delimiter or newline.
     @return [true] if more fields follow, [false] if the record
     is complete. *)
  method private add_unquoted_field =
    let rec examine i =
      if i >= in1 then (
        (* End not found, need to look at the next chunk *)
        Buffer.add_substring current_field in_buf in0 (i - in0);
        in0 <- i;
        self#fill_in_buf; (* or raise End_of_file *)
        examine 0
      )
      else
        let c = String.unsafe_get in_buf i in
        if c = delim || c = '\n' || c = '\r' then (
          Buffer.add_substring current_field in_buf in0 (i - in0);
          in0 <- i + 1;
          record <- (strip_contents current_field) :: record;
          if c = '\r' then (self#skip_CR; false) else (c = delim)
        )
        else examine (i+1)
    in
    try examine in0
    with End_of_file ->
      record <- (strip_contents current_field) :: record;
      false

  (* Quoted field.  Read till a closing quote, a newline, or the end
     of the file and decode the field.  Skip the next delimiter or
     newline.  @return [true] if more fields follow, [false] if the
     record is complete. *)
  method private add_quoted_field field_no =
    let after_quote = ref false in (* preserved through exn *)
    let rec examine i =
      if i >= in1 then (
        (* End of field not found, need to look at the next chunk *)
        Buffer.add_substring current_field in_buf in0 (i - in0);
        in0 <- i;
        self#fill_in_buf; (* or raise End_of_file *)
        examine 0
      )
      else
        let c = String.unsafe_get in_buf i in
        if !after_quote then (
          if c = '\"' then (
            after_quote := false;
            (* [c] is kept so a quote will be included in the field *)
            examine (i+1)
          )
          else if c = delim || is_space c || c = '\n' || c = '\r' then (
            seek_delim() (* field already saved; in0=i; after_quote=true *)
          )
          else if excel_tricks && c = '0' then (
            (* Supposedly, '"' '0' means ASCII NULL *)
            after_quote := false;
            Buffer.add_char current_field '\000';
            in0 <- i + 1; (* skip the '0' *)
            examine (i+1)
          )
          else raise(Failure(record_n, field_no, "Bad '\"' in quoted field"))
        )
        else if c = '\"' then (
          after_quote := true;
          (* Save the field so far, without the quote *)
          Buffer.add_substring current_field in_buf in0 (i - in0);
          in0 <- i + 1; (* skip the quote *)
          examine in0
        )
        else examine (i+1)
    and seek_delim() =
      self#fill_in_buf; (* or raise End_of_file *)
      let c = String.unsafe_get in_buf in0 in
      in0 <- in0 + 1;
      if is_space c then seek_delim() (* skip space *)
      else if c = delim || c = '\n' || c = '\r' then (
        record <- Buffer.contents current_field :: record;
        if c = '\r' then (self#skip_CR; false) else (c = delim)
      )
      else raise(Failure(record_n, field_no,
                         "Non-space char after closing the quoted field"))
    in
    try examine in0
    with End_of_file ->
      (* Add the field even if not closed well *)
      record <- Buffer.contents current_field :: record;
      if !after_quote then false
      else raise(Failure(record_n, field_no,
                         "Quoted field closed by end of file"))

  (* We suppose to be at the beginning of a field.  Add the next field
     to [record].  @return [true] if more fields follow, [false] if
     the record is complete.

     Return  Failure (if there is a format error), End_of_line
     (if the row is complete) or End_of_file (if there is not more
     data to read). *)
  method private add_next_field field_no =
    Buffer.clear current_field;
    try
      self#skip_spaces; (* or raise End_of_file *)
      let c = self#unsafe_peek in
      if c = '\"' then (in0 <- in0 + 1; self#add_quoted_field field_no)
      else if excel_tricks && c = '=' then begin
        in0 <- in0 + 1; (* mark '=' as read *)
        try
          if self#unsafe_peek = '\"' then (
            (* Excel trick ="..." to prevent spaces around the field
               to be removed. *)
            in0 <- in0 + 1; (* skip '"' *)
            self#add_quoted_field field_no
          )
          else (
            Buffer.add_char current_field '=';
            self#add_unquoted_field
          )
        with End_of_file ->
          record <-  "=" :: record;
          false
      end
      else self#add_unquoted_field
    with End_of_file ->
      (* If it is the first field, coming from [next()], the field is
         made of spaces.  If after the first, we are sure we read a
         delimiter before (but maybe the field is empty).  Thus add en
         empty field. *)
      record <-  "" :: record;
      false

  method next() =
    if in1 < 0 then raise(Sys_error "Bad file descriptor");
    self#fill_in_buf; (* or End_of_file which means no more records *)
    record <- [];
    record_n <- record_n + 1; (* the current line being read *)
    let more_fields = ref true
    and field_no = ref 0 in
    while !more_fields do
      more_fields := self#add_next_field !field_no;
      incr field_no;
    done;
    record <- List.rev record;
    record


  method current_record = record


  method fold_left : 'a . ('a -> string list -> 'a) -> 'a -> 'a =
    fun f a0 ->
      let a = ref a0 in
      try
        while true do
          a := f !a (self#next())
        done;
        assert false
      with End_of_file -> !a

  method fold_right : 'a . (string list -> 'a -> 'a) -> 'a -> 'a =
    fun f a0 ->
      (* We to collect all records before applying [f]. *)
      let lr = self#fold_left (fun l r -> r :: l) [] in
      List.fold_left (fun a r -> f r a) a0 lr

end

class of_channel ?delim ? excel_tricks fh =
  in_channel ?delim ?excel_tricks
    (object
       val fh = fh
       method input s ofs len =
         try
           let r = input fh s ofs len in
           if r = 0 then raise End_of_file;
           r
         with Sys_blocked_io -> 0
       method close_in() = close_in fh
     end)



(*
 * Output
 *)

class out_channel ?(delim=',') ?(excel_tricks=false) out_chan =
object(self)
  val out_chan = (out_chan: out_obj_channel)
  val delim = String.make 1 delim

  method private write_escaped s =
    ignore(out_chan#output s 0 (String.length s))

  method write_record data =
    List.fold_left (fun first field ->
                      if first then ignore(out_chan#output delim 0 1);
                      self#write_escaped field;
                      false
                   ) true data


  (*   method printf fmt = *)

  method close_out = out_chan#close_out
end


class to_channel ?delim ?excel_tricks fh =
  out_channel ?delim ?excel_tricks
    (object
       val fh = fh
       method output s ofs len = output fh s ofs len; len
       method close_out () = close_out fh
     end)
