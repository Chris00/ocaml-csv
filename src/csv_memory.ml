(* File: csv_memory.ml

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

(* REMARK: This file in copied into csv.ml — instead of being in
   csv.ml and including the preprocessed version of csv.pp.ml — in
   order for the exception [Failure] to be well qualified when printed
   by the default exception handler. *)

(*
 * Acting on CSV data in memory
 *)

let lines = List.length

let columns csv =
  let m = ref 0 in
  List.iter (fun row -> m := max !m (List.length row)) csv;
  !m


let rec dropwhile f = function
  | [] -> []
  | x :: xs when f x -> dropwhile f xs
  | xs -> xs


let rec empty_row = function
  | [] -> true
  | "" :: xs -> empty_row xs
  | _ :: _ -> false

let trim ?(top=true) ?(left=true) ?(right=true) ?(bottom=true) csv =
  let csv = if top then dropwhile empty_row csv else csv in
  let csv =
    if right then
      List.map (fun row ->
                  let row = List.rev row in
                  let row = dropwhile ((=) "") row in
                  let row = List.rev row in
                  row) csv
    else csv in
  let csv =
    if bottom then (
      let csv = List.rev csv in
      let csv = dropwhile empty_row csv in
      let csv = List.rev csv in
      csv
    ) else csv in

  let and_empty_left_cell (col_empty, one_nonempty_row) = function
    | [] -> col_empty, one_nonempty_row
    | "" :: _ -> col_empty, true
    | _ -> false, true in
  let empty_left_col =
    List.fold_left and_empty_left_cell (true, false) in
  let remove_left_col =
    List.map (function [] -> [] | _ :: xs -> xs) in
  let rec loop csv =
    let left_col_empty, one_nonempty_row = empty_left_col csv in
    if left_col_empty && one_nonempty_row then
      loop(remove_left_col csv)
    else
      csv
  in

  let csv = if left then loop csv else csv in

  csv

let square csv =
  let columns = columns csv in
  List.map (
    fun row ->
      let n = List.length row in
      let row = List.rev row in
      let rec loop acc = function
        | 0 -> acc
        | i -> "" :: loop acc (i-1)
      in
      let row = loop row (columns - n) in
      List.rev row
  ) csv

let is_square csv =
  let columns = columns csv in
  List.for_all (fun row -> List.length row = columns) csv

let rec set_columns ~cols = function
  | [] -> []
  | r :: rs ->
      let rec loop i cells =
        if i < cols then (
          match cells with
          | [] -> "" :: loop (succ i) []
          | c :: cs -> c :: loop (succ i) cs
        )
        else []
      in
      loop 0 r :: set_columns ~cols rs

let rec set_rows ~rows csv =
  if rows > 0 then (
    match csv with
    | [] -> [] :: set_rows ~rows:(pred rows) []
    | r :: rs -> r :: set_rows ~rows:(pred rows) rs
  )
  else []

let set_size ~rows ~cols csv =
  set_columns ~cols (set_rows ~rows csv)

(* from extlib: *)
let rec drop n = function
  | _ :: l when n > 0 -> drop (n-1) l
  | l -> l

let sub ~r ~c ~rows ~cols csv =
  let csv = drop r csv in
  let csv = List.map (drop c) csv in
  let csv = set_rows ~rows csv in
  let csv = set_columns ~cols csv in
  csv

(* Compare two rows for semantic equality - ignoring any blank cells
 * at the end of each row.
 *)
let rec compare_row (row1 : string list) row2 =
  match row1, row2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let c = compare x y in
      if c <> 0 then c else compare_row xs ys
  | "" :: xs , [] ->
      compare_row xs []
  | _ :: _, [] ->
      1
  | [], "" :: ys ->
      compare_row [] ys
  | [], _ :: _ ->
      -1

(* Semantic equality for CSV files. *)
let rec compare (csv1 : t) csv2 =
  match csv1, csv2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let c = compare_row x y in
      if c <> 0 then c else compare xs ys
  | x :: xs, [] ->
      let c = compare_row x [] in
      if c <> 0 then c else compare xs []
  | [], y :: ys ->
      let c = compare_row [] y in
      if c <> 0 then c else compare [] ys

(* Concatenate - arrange left to right. *)
let rec concat = function
  | [] -> []
  | [csv] -> csv
  | left_csv :: csvs ->
      (* Concatenate the remaining CSV files. *)
      let right_csv = concat csvs in

      (* Set the height of the left and right CSVs to the same. *)
      let nr_rows = max (lines left_csv) (lines right_csv) in
      let left_csv = set_rows ~rows:nr_rows left_csv in
      let right_csv = set_rows ~rows:nr_rows right_csv in

      (* Square off the left CSV. *)
      let left_csv = square left_csv in

      (* Prepend the right CSV rows with the left CSV rows. *)
      List.map (
        fun (left_row, right_row) -> List.append left_row right_row
      ) (List.combine left_csv right_csv)

let transpose =
  (* Suppose the CSV data is presented with the last row first.  Then
     new rows may be constructed in a tail rec way.  We use mutable
     rows in order to preserve tail recursiveness. *)
  (* Return the new 1st row; whether all rows are empty. *)
  let rec row_of_1st_col tr_row empty = function
    | [] -> (tr_row, empty)     (* No more rows *)
    | r :: rows ->
       match !r with
       | [] ->                           (* Last row empty *)
          let tr_row = if tr_row = [] then tr_row else "" :: tr_row in
          row_of_1st_col tr_row empty rows
       | a :: tl ->
          r := tl;
          let tr_row = if a = "" && tr_row = [] then [] else a :: tr_row in
          row_of_1st_col tr_row false rows  in
  let rec tr tr_csv csv =
    let row, empty = row_of_1st_col [] true csv in (* remove [csv] 1st col *)
    if empty then List.rev tr_csv
    else tr (row :: tr_csv) csv in
  fun csv -> tr [] (List.rev_map ref csv)

let to_array csv =
  Array.of_list (List.map Array.of_list csv)

let of_array csv =
  List.map Array.to_list (Array.to_list csv)

let rec combine ~header row = match header, row with
  | [], _ -> []
  | _, [] -> List.map (fun h -> (h, "")) header
  | h0 :: h, x :: r -> (h0, x) :: combine ~header:h r

let associate header data =
  List.map (fun row -> combine ~header row) data

let map ~f csv =
  List.map (fun row -> List.map (fun el -> f el) row) csv


let save_out_readable chan csv =
  (* Escape all the strings in the CSV file first. *)
  (* XXX Why are we doing this?  I commented it out anyway.
  let csv = List.map (List.map String.escaped) csv in
  *)

  (* Find the width of each column. *)
  let widths =
    (* Don't consider rows with only a single element - typically
     * long titles.
     *)
    let csv = List.filter (function [_] -> false | _ -> true) csv in

    (* Square the CSV file - makes the next step simpler to implement. *)
    let csv = square csv in

    match csv with
      | [] -> []
      | row1 :: rest ->
          let lengths_row1 = List.map String.length row1 in
          let lengths_rest = List.map (List.map String.length) rest in
          let max2rows r1 r2 =
            let rp =
              try List.combine r1 r2
              with
                Invalid_argument _ ->
                  failwith (Printf.sprintf "Csv.save_out_readable: internal \
                              error: length r1 = %d, length r2 = %d"
                              (List.length r1) (List.length r2)) in
            List.map (fun ((a : int), (b : int)) -> max a b) rp
          in
          List.fold_left max2rows lengths_row1 lengths_rest in

  (* Print out each cell at the correct width. *)
  let rec repeat f = function
    | 0 -> ()
    | i -> f (); repeat f (i-1)
  in
  List.iter (
    function
    | [cell] ->                         (* Single column. *)
        output_string chan cell;
        output_char chan '\n'
    | row ->                            (* Other. *)
        (* Pair up each cell with its max width. *)
        let row =
          let rec loop = function
            | ([], _) -> []
            | (_, []) -> failwith "Csv.save_out_readable: internal error"
            | (cell :: cells, width :: widths) ->
                (cell, width) :: loop (cells, widths)
          in
          loop (row, widths) in
        List.iter (
          fun (cell, width) ->
            output_string chan cell;
            let n = String.length cell in
            repeat (fun () -> output_char chan ' ') (width - n + 1)
        ) row;
        output_char chan '\n'
  ) csv

let print_readable = save_out_readable stdout
