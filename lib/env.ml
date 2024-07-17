open Syntax

type scope = { vars : (string * ty) list; tags : (string * ty) list }

let env : scope list ref = ref []
