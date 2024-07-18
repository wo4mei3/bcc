open Syntax

type token =
  | XOR_EQ
  | WHILE
  | VOLATILE
  | VA_START
  | VA_LIST
  | VA_END
  | VA_ARG
  | USING
  | UNSAFE
  | UNION
  | TYPE_ID of string
  | TYPEDEF
  | TVOID
  | TUNSIGNED
  | TSIGNED
  | TSHORT
  | TLONG
  | TINT
  | TFLOAT
  | TDOUBLE
  | TCHAR
  | TBOOL
  | SWITCH
  | SUB_EQ
  | STRUCT
  | STR of string
  | STATIC
  | STAR
  | SIZEOF
  | SEMI
  | RSHIFT_EQ
  | RSHIFT
  | RPAREN
  | RETURN
  | REGISTER
  | RBRACKET
  | RBRACE
  | QUESTION
  | PLUS
  | OR_EQ
  | OROR
  | OR
  | NULL
  | NOT
  | NORETURN
  | NE
  | MUL_EQ
  | MOD_EQ
  | MOD
  | MINUS
  | LT
  | LSHIFT_EQ
  | LSHIFT
  | LPAREN
  | LIFETIME
  | LID of string
  | LE
  | LBRACKET
  | LBRACE
  | KIND
  | INT of string
  | INLINE
  | INC
  | IF
  | ID of string
  | HAT
  | GT
  | GOTO
  | GE
  | FOR
  | FLOAT of string
  | EXTERN
  | EQEQ
  | EQ
  | EOF
  | ENUM
  | ELSE
  | ELLIPSIS
  | DYN
  | DROP
  | DOT
  | DO
  | DIV_EQ
  | DIV
  | DEPTH
  | DEFAULT
  | DEC
  | CONTINUE
  | CONST
  | COMMA
  | COLON
  | CHAR of string
  | CASE
  | BREAK
  | BANG
  | AUTO
  | ARROW
  | AND_EQ
  | ANDAND
  | AND
  | ADD_EQ
[@@deriving show]

(*
open Env 

type declarator =
| DeclPtr of declarator * depth * kind * qualifier list
| DeclIdent of string
| DeclArr of declarator * expr
| DeclFun of declarator * expr decl list

*)

let lookup_tokens : token list ref = ref []

let peek1 lexer lexbuf =
  match !lookup_tokens with
  | [] ->
      let tok = lexer lexbuf in
      lookup_tokens := !lookup_tokens @ [ tok ];
      tok
  | x :: _ -> x

let peek2 lexer lexbuf =
  match !lookup_tokens with
  | [] ->
      let tok = lexer lexbuf in
      lookup_tokens := !lookup_tokens @ [ tok ];
      let tok = lexer lexbuf in
      lookup_tokens := !lookup_tokens @ [ tok ];
      tok
  | _ :: [] ->
      let tok = lexer lexbuf in
      lookup_tokens := !lookup_tokens @ [ tok ];
      tok
  | _ :: x :: _ -> x

let next lexer lexbuf =
  if !lookup_tokens = [] then ignore (peek1 lexer lexbuf);
  lookup_tokens := List.tl !lookup_tokens

let expect lexer lexbuf token =
  if peek1 lexer lexbuf = token then next lexer lexbuf
  else failwith (Printf.sprintf "%s expected" (show_token token))

let consume lexer lexbuf token =
  if peek1 lexer lexbuf <> token then false
  else (
    next lexer lexbuf;
    true)

let is_typename = function
  | TVOID | TBOOL | VA_LIST | TYPE_ID _ | TCHAR | TSHORT | TINT | TLONG | TFLOAT
  | TDOUBLE | TSIGNED | TUNSIGNED | TYPEDEF | EXTERN | STATIC | AUTO | REGISTER
  | CONST | VOLATILE | INLINE | NORETURN ->
      true
  | _ -> false

let rec parse_declspec' lexer lexbuf unique nonunique =
  match peek1 lexer lexbuf with
  | (TVOID | TBOOL | VA_LIST | TYPE_ID _) when not (unique || nonunique) ->
      next lexer lexbuf;
      parse_declspec' lexer lexbuf true nonunique
  | (TCHAR | TSHORT | TINT | TLONG | TFLOAT | TDOUBLE | TSIGNED | TUNSIGNED)
    when not unique ->
      next lexer lexbuf;
      parse_declspec' lexer lexbuf unique true
  | TVOID | TBOOL | VA_LIST | TYPE_ID _ | TCHAR | TSHORT | TINT | TLONG | TFLOAT
  | TDOUBLE | TSIGNED | TUNSIGNED ->
      failwith "expected declarator"
  | TYPEDEF | EXTERN | STATIC | AUTO | REGISTER | CONST | VOLATILE | INLINE
  | NORETURN ->
      next lexer lexbuf;
      parse_declspec' lexer lexbuf unique nonunique
  | _ when unique || nonunique -> ()
  | _ -> failwith "expected a type specifier"

let parse_declspec lexer lexbuf = parse_declspec' lexer lexbuf false false

let rec parse_declarator lexer lexbuf =
  match peek1 lexer lexbuf with
  | STAR ->
      next lexer lexbuf;
      parse_declarator lexer lexbuf
  | ID _ | LPAREN -> parse_direct_declarator lexer lexbuf
  | _ -> failwith "expected a declarator"

and parse_direct_declarator lexer lexbuf =
  (match peek1 lexer lexbuf with
  | ID _ -> next lexer lexbuf
  | LPAREN ->
      next lexer lexbuf;
      parse_declarator lexer lexbuf;
      expect lexer lexbuf RPAREN
  | _ -> failwith "expected a parse_direct_declaratator");
  parse_type_suffix lexer lexbuf

and parse_abstract_declarator lexer lexbuf =
  match peek1 lexer lexbuf with
  | STAR ->
      next lexer lexbuf;
      parse_declarator lexer lexbuf
  | LPAREN -> parse_abstract_direct_declarator lexer lexbuf
  | _ -> failwith "expected a declarator"

and parse_abstract_direct_declarator lexer lexbuf =
  (match peek1 lexer lexbuf with
  | LPAREN ->
      next lexer lexbuf;
      parse_abstract_declarator lexer lexbuf;
      expect lexer lexbuf RPAREN
  | _ -> failwith "expected a parse_direct_declaratator");
  parse_type_suffix lexer lexbuf

and parse_type_suffix lexer lexbuf =
  match peek1 lexer lexbuf with
  | LPAREN ->
      next lexer lexbuf;
      if peek1 lexer lexbuf = TVOID && peek2 lexer lexbuf = RPAREN then (
        next lexer lexbuf;
        next lexer lexbuf)
      else parse_func_params lexer lexbuf
  | LBRACKET ->
      next lexer lexbuf;
      parse_array_dims lexer lexbuf
  | _ -> ()

and parse_func_params lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        parse_declspec lexer lexbuf;
        parse_declarator lexer lexbuf;
        aux lexer lexbuf
    | RPAREN ->
        next lexer lexbuf;
        parse_type_suffix lexer lexbuf
    | _ -> failwith "expected a RBRACKET"
  in
  match peek1 lexer lexbuf with
  | RPAREN ->
      next lexer lexbuf;
      parse_type_suffix lexer lexbuf
  | _ ->
      parse_declspec lexer lexbuf;
      parse_declarator lexer lexbuf;
      aux lexer lexbuf

and parse_array_dims lexer lexbuf =
  match peek1 lexer lexbuf with
  | INT _ ->
      next lexer lexbuf;
      expect lexer lexbuf RBRACKET;
      parse_type_suffix lexer lexbuf
  | _ ->
      expect lexer lexbuf RBRACKET;
      parse_type_suffix lexer lexbuf

and parse_typename lexer lexbuf =
  parse_declspec lexer lexbuf;
  parse_abstract_declarator lexer lexbuf

let rec parse_primary lexer lexbuf =
  match peek1 lexer lexbuf with
  | ID n ->
      next lexer lexbuf;
      EVar n
  | INT i ->
      next lexer lexbuf;
      EConst (VInt i)
  | CHAR c ->
      next lexer lexbuf;
      EConst (VChar c)
  | STR s ->
      next lexer lexbuf;
      EConst (VStr s)
  | LPAREN ->
      (*next lexer lexbuf;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf RPAREN;
      e*)
      failwith "notimpl"
  | _ ->
      print_endline (show_token (List.hd !lookup_tokens));
      failwith "expected a expression"

and parse_postfix lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    (*| LPAREN ->
        next lexer lexbuf;
        let rec aux2 lexer lexbuf =
          match peek1 lexer lexbuf with
          | COMMA ->
              next lexer lexbuf;
              parse_assign lexer lexbuf :: aux2 lexer lexbuf
          | RPAREN ->
              next lexer lexbuf;
              []
          | _ -> failwith "PCall"
        in
        let e =
          EPostfix
            ( e,
              PCall
                (if consume lexer lexbuf RPAREN then [] else aux2 lexer lexbuf)
            )
        in
        next lexer lexbuf;
        aux lexer lexbuf e
    | LBRACKET ->
        next lexer lexbuf;
        let e =
          EPostfix
            ( e,
              PIdx
                (if consume lexer lexbuf RBRACKET then None
                 else Some (parse_expr lexer lexbuf)) )
        in
        next lexer lexbuf;
        aux lexer lexbuf e*)
    | DOT ->
        next lexer lexbuf;
        let id =
          match peek1 lexer lexbuf with
          | ID n | TYPE_ID n -> n
          | _ -> failwith "PDot"
        in
        next lexer lexbuf;
        let e = EPostfix (e, PDot id) in
        aux lexer lexbuf e
    | ARROW ->
        next lexer lexbuf;
        let id =
          match peek1 lexer lexbuf with
          | ID n | TYPE_ID n -> n
          | _ -> failwith "PDot"
        in
        next lexer lexbuf;
        let e = EPostfix (e, PArrow id) in
        aux lexer lexbuf e
    | INC ->
        next lexer lexbuf;
        let e = EPostfix (e, PInc) in
        aux lexer lexbuf e
    | DEC ->
        next lexer lexbuf;
        let e = EPostfix (e, PDec) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_primary lexer lexbuf in
  aux lexer lexbuf e

and parse_unary lexer lexbuf =
  match peek1 lexer lexbuf with
  | PLUS ->
      next lexer lexbuf;
      EUnary (Plus, parse_cast lexer lexbuf)
  | MINUS ->
      next lexer lexbuf;
      EUnary (Minus, parse_cast lexer lexbuf)
  | AND ->
      next lexer lexbuf;
      EUnary (Ref, parse_cast lexer lexbuf)
  | STAR ->
      next lexer lexbuf;
      EUnary (Deref, parse_cast lexer lexbuf)
  | BANG ->
      next lexer lexbuf;
      EUnary (LogNot, parse_cast lexer lexbuf)
  | NOT ->
      next lexer lexbuf;
      EUnary (BitNot, parse_cast lexer lexbuf)
  | INC ->
      next lexer lexbuf;
      EUnary (Inc, parse_cast lexer lexbuf)
  | DEC ->
      next lexer lexbuf;
      EUnary (Dec, parse_cast lexer lexbuf)
  | _ -> parse_postfix lexer lexbuf

and parse_cast lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | LPAREN, tok when is_typename tok ->
      next lexer lexbuf;
      parse_typename lexer lexbuf;
      next lexer lexbuf;
      parse_cast lexer lexbuf
  | _ -> parse_unary lexer lexbuf
(*
let parse_mul lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | STAR ->
        next lexer lexbuf;
        parse_cast lexer lexbuf;
        aux lexer lexbuf
    | DIV ->
        next lexer lexbuf;
        parse_cast lexer lexbuf;
        aux lexer lexbuf
    | MOD ->
        next lexer lexbuf;
        parse_cast lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_cast lexer lexbuf;
  aux lexer lexbuf

and parse_add lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | PLUS ->
        next lexer lexbuf;
        parse_mul lexer lexbuf;
        aux lexer lexbuf
    | MINUS ->
        next lexer lexbuf;
        parse_mul lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_mul lexer lexbuf;
  aux lexer lexbuf

and parse_shift lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | LSHIFT ->
        next lexer lexbuf;
        parse_add lexer lexbuf;
        aux lexer lexbuf
    | RSHIFT ->
        next lexer lexbuf;
        parse_add lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_add lexer lexbuf;
  aux lexer lexbuf

and parse_relational lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | LT ->
        next lexer lexbuf;
        parse_shift lexer lexbuf;
        aux lexer lexbuf
    | GT ->
        next lexer lexbuf;
        parse_shift lexer lexbuf;
        aux lexer lexbuf
    | LE ->
        next lexer lexbuf;
        parse_shift lexer lexbuf;
        aux lexer lexbuf
    | GE ->
        next lexer lexbuf;
        parse_shift lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_shift lexer lexbuf;
  aux lexer lexbuf

and parse_equality lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | EQEQ ->
        next lexer lexbuf;
        parse_relational lexer lexbuf;
        aux lexer lexbuf
    | NE ->
        next lexer lexbuf;
        parse_relational lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_relational lexer lexbuf;
  aux lexer lexbuf

and parse_and lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | AND ->
        next lexer lexbuf;
        parse_equality lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_equality lexer lexbuf;
  aux lexer lexbuf

and parse_xor lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | HAT ->
        next lexer lexbuf;
        parse_and lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_and lexer lexbuf;
  aux lexer lexbuf

and parse_or lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | OR ->
        next lexer lexbuf;
        parse_xor lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_xor lexer lexbuf;
  aux lexer lexbuf

and parse_logand lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | ANDAND ->
        next lexer lexbuf;
        parse_or lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_or lexer lexbuf;
  aux lexer lexbuf

and parse_logor lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | OROR ->
        next lexer lexbuf;
        parse_logand lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_logand lexer lexbuf;
  aux lexer lexbuf

and parse_conditional lexer lexbuf =
  parse_logor lexer lexbuf;
  match peek1 lexer lexbuf with
  | QUESTION ->
      next lexer lexbuf;
      parse_expr lexer lexbuf;
      expect lexer lexbuf COLON;
      parse_conditional lexer lexbuf
  | _ -> ()

and parse_assign lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | ADD_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | SUB_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | MUL_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | DIV_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | MOD_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | AND_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | OR_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | XOR_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | LSHIFT_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | RSHIFT_EQ ->
        next lexer lexbuf;
        parse_conditional lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_conditional lexer lexbuf;
  aux lexer lexbuf

and parse_expr lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        parse_assign lexer lexbuf;
        aux lexer lexbuf
    | _ -> ()
  in
  parse_assign lexer lexbuf;
  aux lexer lexbuf

let rec parse_init lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | LBRACE, RBRACE ->
      next lexer lexbuf;
      next lexer lexbuf
  | LBRACE, COMMA ->
      next lexer lexbuf;
      next lexer lexbuf;
      expect lexer lexbuf RBRACE
  | LBRACE, _ ->
      next lexer lexbuf;
      parse_init_list lexer lexbuf;
      ignore (consume lexer lexbuf COMMA);
      expect lexer lexbuf RBRACE
  | _ -> parse_assign lexer lexbuf

and parse_init_list lexer lexbuf =
  let rec aux lexer lexbuf =
    if
      peek1 lexer lexbuf = RBRACE
      || (peek1 lexer lexbuf = COMMA && peek2 lexer lexbuf = RBRACE)
    then ()
    else (
      expect lexer lexbuf COMMA;
      parse_init lexer lexbuf;
      aux lexer lexbuf)
  in
  parse_assign lexer lexbuf;
  aux lexer lexbuf

let parse_init_declarator lexer lexbuf =
  parse_declarator lexer lexbuf;
  if consume lexer lexbuf EQ then parse_init lexer lexbuf;
  expect lexer lexbuf SEMI

let parse_declaration lexer lexbuf =
  parse_declspec lexer lexbuf;
  parse_init_declarator lexer lexbuf

let rec parse_stmt lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | RETURN, SEMI ->
      next lexer lexbuf;
      next lexer lexbuf
  | BREAK, _ ->
      next lexer lexbuf;
      expect lexer lexbuf SEMI
  | CONTINUE, _ ->
      next lexer lexbuf;
      expect lexer lexbuf SEMI
  | RETURN, _ ->
      next lexer lexbuf;
      parse_expr lexer lexbuf;
      expect lexer lexbuf SEMI;
      parse_stmt lexer lexbuf
  | IF, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      parse_expr lexer lexbuf;
      expect lexer lexbuf RPAREN;
      parse_stmt lexer lexbuf;
      if not (consume lexer lexbuf ELSE) then () else parse_stmt lexer lexbuf
  | SWITCH, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      parse_expr lexer lexbuf;
      expect lexer lexbuf RPAREN;
      parse_stmt lexer lexbuf
  | ID _, COLON ->
      next lexer lexbuf;
      next lexer lexbuf;
      parse_stmt lexer lexbuf
  | CASE, _ ->
      next lexer lexbuf;
      parse_conditional lexer lexbuf;
      expect lexer lexbuf COLON;
      parse_stmt lexer lexbuf
  | DEFAULT, _ ->
      next lexer lexbuf;
      expect lexer lexbuf COLON;
      parse_stmt lexer lexbuf
  | FOR, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      if is_typename (peek1 lexer lexbuf) then (
        parse_declaration lexer lexbuf;
        if peek1 lexer lexbuf <> SEMI then parse_expr lexer lexbuf;
        expect lexer lexbuf SEMI;
        if peek1 lexer lexbuf <> RPAREN then parse_expr lexer lexbuf)
      else (
        if peek1 lexer lexbuf <> SEMI then parse_expr lexer lexbuf;
        expect lexer lexbuf SEMI;
        if peek1 lexer lexbuf <> SEMI then parse_expr lexer lexbuf;
        expect lexer lexbuf SEMI;
        if peek1 lexer lexbuf <> RPAREN then parse_expr lexer lexbuf);
      expect lexer lexbuf RPAREN;
      parse_stmt lexer lexbuf
  | WHILE, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      parse_expr lexer lexbuf;
      expect lexer lexbuf RPAREN;
      parse_stmt lexer lexbuf
  | DO, _ ->
      next lexer lexbuf;
      parse_stmt lexer lexbuf;
      expect lexer lexbuf WHILE;
      expect lexer lexbuf LPAREN;
      parse_expr lexer lexbuf;
      expect lexer lexbuf RPAREN;
      expect lexer lexbuf SEMI
  | GOTO, ID _ ->
      next lexer lexbuf;
      next lexer lexbuf;
      expect lexer lexbuf SEMI
  | LBRACE, _ -> parse_compound_stmt lexer lexbuf
  | ty, _ when is_typename ty -> parse_declaration lexer lexbuf
  | _ ->
      parse_expr lexer lexbuf;
      expect lexer lexbuf SEMI

and parse_compound_stmt lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | LBRACE, RBRACE ->
      next lexer lexbuf;
      expect lexer lexbuf RBRACE
  | LBRACE, _ ->
      next lexer lexbuf;
      parse_stmt lexer lexbuf;
      expect lexer lexbuf RBRACE
  | _ -> failwith "expected LBRACE"

let parse_function lexer lexbuf =
  parse_declspec lexer lexbuf;
  parse_declarator lexer lexbuf;
  parse_compound_stmt lexer lexbuf
*)