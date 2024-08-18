open Syntax

type scope = {
  vars : (string * ty) list;
  structs : (string * (string * ty) list) list;
  unions : (string * (string * ty) list) list;
  enums : (string * string list) list;
}
[@@deriving show]

type stack = scope list

let curr : scope ref = ref { vars = []; structs = []; unions = []; enums = [] }
let stack : scope list ref = ref []

let push () =
  stack := !curr :: !stack;
  curr := { vars = []; structs = []; unions = []; enums = [] }

let pop () =
  curr := List.hd !stack;
  stack := List.tl !stack

let add_var pair = curr := { !curr with vars = pair :: !curr.vars }
let add_struct pair = curr := { !curr with structs = pair :: !curr.structs }
let add_union pair = curr := { !curr with unions = pair :: !curr.unions }
let add_enum pair = curr := { !curr with enums = pair :: !curr.enums }

let is_typedef name decl =
  let rec aux = function
    | TFun (ty, _) | TPtr ty | TArr (ty, _) -> aux ty
    | TBase base -> List.mem ScsTypedef base
  in
  fst decl = name && aux (snd decl)

let find_var name =
  let rec aux = function
    | [] -> None
    | x :: xs when List.mem_assoc name x.vars ->
        let ty = List.assoc name x.vars in
        if is_typedef name (name, ty) then aux xs else Some ty
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.vars then Some (List.assoc name !curr.vars)
  else aux !stack

let find_typedef name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match List.find_opt (is_typedef name) x.vars with
        | Some x -> Some x
        | None -> aux xs)
  in
  match List.find_opt (is_typedef name) !curr.vars with
  | Some x -> Some x
  | None -> aux !stack

let find_struct name =
  let rec aux = function
    | [] -> None
    | x :: _ when List.mem_assoc name x.structs ->
        Some (List.assoc name x.structs)
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.structs then Some (List.assoc name !curr.structs)
  else aux !stack

let find_union name =
  let rec aux = function
    | [] -> None
    | x :: _ when List.mem_assoc name x.unions ->
        Some (List.assoc name x.unions)
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.unions then Some (List.assoc name !curr.unions)
  else aux !stack
