open Syntax

type scope = {
  mutable vars : (string * ty) list;
  mutable tags : (string * ty) list;
}

let curr : scope ref = ref { vars = []; tags = [] }
let stack : scope list ref = ref []

let push () =
  stack := !curr :: !stack;
  curr := { vars = []; tags = [] }

let pop () =
  curr := List.hd !stack;
  stack := List.tl !stack

let add_var var = !curr.vars <- var :: !curr.vars
let add_tag tag = !curr.tags <- tag :: !curr.tags

let is_typedef name decl =
  let rec aux = function
    | TFun (ty, _) | TPtr ty | TArr (ty, _) -> aux ty
    | TBase base -> List.mem ScsTypedef base
  in
  fst decl = name && aux (snd decl)

let find_var name =
  let rec aux = function
    | [] -> None
    | x :: _ when List.mem_assoc name x.vars -> Some (List.assoc name x.vars)
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
