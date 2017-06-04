(* File: csv_row.ml

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

(*
 * Representation of rows accessible by both keys and numbers
 *)

module Header = struct
  module M = Map.Make(String)

  type t = { names : string array;
             index : int M.t }
  (* This is a correspondence between names and column numbers, in
     both directions.  Names "" are not active and must not be in the
     index. *)

  let empty = { names = [| |];  index = M.empty }

  let get t i = try t.names.(i) with _ -> ""
  let find t name = M.find name t.index

  let of_names names =
    let names = Array.of_list names in
    let index = ref M.empty in
    for i = 0 to Array.length names - 1 do
      if names.(i) <> "" then
        if M.mem names.(i) !index then
          names.(i) <- "" (* remove duplicate binding *)
        else index := M.add names.(i) i !index
    done;
    { names;  index = !index }

  let names t = Array.to_list t.names

  (* [main] names take precedence over [t] ones. *)
  let merge ~main t =
    let index = ref main.index in
    if Array.length main.names >= Array.length t.names then (
      let names = Array.copy main.names in
      for i = 0 to Array.length t.names - 1 do
        if names.(i) = "" && t.names.(i) <> ""
           && not(M.mem t.names.(i) !index) then (
          names.(i) <- t.names.(i);
          index := M.add names.(i) i !index
        )
      done;
      { names;  index = !index }
    )
    else (
      let names = Array.make (Array.length t.names) "" in
      for i = 0 to Array.length main.names - 1 do
        if main.names.(i) <> "" then
          names.(i) <- main.names.(i)
        else if t.names.(i) <> "" && not(M.mem t.names.(i) !index) then (
          names.(i) <- t.names.(i);
          index := M.add names.(i) i !index
        )
      done;
      for i = Array.length main.names to Array.length names - 1 do
        if t.names.(i) <> "" && not(M.mem t.names.(i) !index) then (
          names.(i) <- t.names.(i);
          index := M.add names.(i) i !index
        )
      done;
      { names;  index = !index }
    )
end


module Row = struct
  (* Datastructure with double access (integer and key). *)
  type t = { header : Header.t;  row: string array }

  let make header row = { header;  row = Array.of_list row }

  let get t i = try t.row.(i) with _ -> ""

  let find t key =
    try t.row.(Header.find t.header key)
    with _ -> ""

  let to_list t = Array.to_list t.row

  let to_assoc t =
    let l = ref [] in
    for i = Array.length t.row - 1 downto 0 do
      l := (Header.get t.header i, t.row.(i)) :: !l
    done;
    !l

  let with_header t h =
    let h = Header.of_names h in
    { t with header = Header.merge ~main:h t.header }
end

type t = Row.t
(* Clearer export *)

