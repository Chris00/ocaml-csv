(* File: csv_utils.ml

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

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y
let max x y = if (x:int) >= y then x else y

(* Add Buffer.add_subbytes for all compiler versions.
   Copied from the OCaml stdlib. *)
module Buffer = struct
    include Buffer

    let add_subbytes b s offset len =
      add_substring b (Bytes.unsafe_to_string s) offset len
  end

(* Enhance the List module with tail rec functions. *)
module List = struct
  include List

  (* Implementation of [map] in JSC Core. *)
  let map_slow l ~f = List.rev (List.rev_map f l)

  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
       let f1 = f x1 in
       [f1]
    | [x1; x2] ->
       let f1 = f x1 in
       let f2 = f x2 in
       [f1; f2]
    | [x1; x2; x3] ->
       let f1 = f x1 in
       let f2 = f x2 in
       let f3 = f x3 in
       [f1; f2; f3]
    | [x1; x2; x3; x4] ->
       let f1 = f x1 in
       let f2 = f x2 in
       let f3 = f x3 in
       let f4 = f x4 in
       [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
       let f1 = f x1 in
       let f2 = f x2 in
       let f3 = f x3 in
       let f4 = f x4 in
       let f5 = f x5 in
       f1 :: f2 :: f3 :: f4 :: f5 :: (if ctr > 1000
                                      then map_slow ~f tl
                                      else count_map ~f tl (ctr + 1))

  let map f l = count_map ~f l 0

  (* Implementation of [append] in JSC core. *)
  let slow_append l1 l2 = List.rev_append (List.rev l1) l2

  let rec count_append l1 l2 count =
    match l1 with
    | []               ->                         l2
    | [x1]             -> x1                   :: l2
    | [x1; x2]         -> x1 :: x2             :: l2
    | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
    | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
       x1 :: x2 :: x3 :: x4 :: x5 :: (if count > 1000
                                      then slow_append tl l2
                                      else count_append tl l2 (count + 1))

  let append l1 l2 = count_append l1 l2 0

  (* Tail recursive [combine]. *)
  let rec rev_combine acc l1 l2 =
    match l1, l2 with
    | ([], []) -> acc
    | (a1::l1, a2::l2) -> rev_combine ((a1, a2) :: acc) l1 l2
    | (_, _) -> invalid_arg "List.combine"

  let slow_combine l1 l2 = List.rev (rev_combine [] l1 l2)

  let rec count_combine l1 l2 count =
    match l1, l2 with
    | ([], []) -> []
    | ([x1], [y1]) -> [x1, y1]
    | ([x1; x2], [y1; y2]) -> [x1, y1; x2, y2]
    | ([x1; x2; x3], [y1; y2; y3]) -> [x1, y1; x2, y2; x3, y3]
    | ([x1; x2; x3; x4], [y1; y2; y3; y4]) -> [x1, y1; x2, y2; x3, y3; x4, y4]
    | (x1 :: x2 :: x3 :: x4 :: tl1), (y1 :: y2 :: y3 :: y4 :: tl2) ->
       (x1, y1) :: (x2, y2) :: (x3, y3) :: (x4, y4)
       :: (if count > 1000 then slow_combine tl1 tl2
           else count_combine tl1 tl2 (count + 1))
    | (_, _) -> invalid_arg "List.combine"

  let combine l1 l2 = count_combine l1 l2 0

end


(*
 * Helpers for input
 *)

let is_space_or_tab c = c = ' ' || c = '\t' (* See documentation *)
let is_real_space c = c = ' ' (* when separator = '\t' *)

(* Given a buffer, returns its content stripped of *final* whitespace. *)
let rstrip_contents buf =
  let n = ref(Buffer.length buf - 1) in
  while !n >= 0 && is_space_or_tab(Buffer.nth buf !n) do decr n done;
  Buffer.sub buf 0 (!n + 1)

(* Return the substring after stripping its final space.  It is
   assumed the substring parameters are valid. *)
let rstrip_substring buf ofs len =
  let n = ref(ofs + len - 1) in
  while !n >= ofs && is_space_or_tab(Bytes.unsafe_get buf !n) do decr n done;
  Bytes.sub_string buf ofs (!n - ofs + 1)

let do_nothing _ = ()

(* Array: char escaped with '\\' → char.
   Keep in sync with [escape]. *)
let unescape =
  let escaped_by c =
    match Char.unsafe_chr c with
    | '0' -> '\000' (* \0 = NULL *)
    | 'b' -> '\b'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'Z' -> '\026' (* Ctrl + Z, used by MySQL. *)
    | c -> c (* unchanged *) in
  Array.init 256 escaped_by
