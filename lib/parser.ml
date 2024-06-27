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

let next () = lookup_tokens := List.tl !lookup_tokens

let expected lexer lexbuf token =
  if peek1 lexer lexbuf = token then next () else failwith "hoge expected"

let consume lexer lexbuf token =
  if peek1 lexer lexbuf = token then false
  else (
    next ();
    true)

let rec parse_declspec lexer lexbuf unique nonunique =
  match peek1 lexer lexbuf with
  | (TVOID | TBOOL | VA_LIST | TYPE_ID _) when not (unique || nonunique) ->
      next ();
      parse_declspec lexer lexbuf true nonunique
  | (TCHAR | TSHORT | TINT | TLONG | TFLOAT | TDOUBLE | TSIGNED | TUNSIGNED)
    when not unique ->
      next ();
      parse_declspec lexer lexbuf unique true
  | TVOID | TBOOL | VA_LIST | TYPE_ID _ | TCHAR | TSHORT | TINT | TLONG | TFLOAT
  | TDOUBLE | TSIGNED | TUNSIGNED ->
      failwith "expected declarator"
  | TYPEDEF | EXTERN | STATIC | AUTO | REGISTER | CONST | VOLATILE | INLINE
  | NORETURN ->
      next ();
      parse_declspec lexer lexbuf unique nonunique
  | _ when unique || nonunique -> ()
  | _ -> failwith "expected a type specifier"

let rec parse_declarator lexer lexbuf =
  match peek1 lexer lexbuf with
  | STAR ->
      next ();
      parse_declarator lexer lexbuf
  | ID _ | LPAREN -> parse_direct_declarator lexer lexbuf
  | _ -> failwith "expected a declarator"

and parse_direct_declarator lexer lexbuf =
  (match peek1 lexer lexbuf with
  | ID _ -> next ()
  | LPAREN ->
      next ();
      parse_declarator lexer lexbuf;
      expected lexer lexbuf RPAREN
  | _ -> failwith "expected a parse_direct_declaratator");
  parse_type_suffix lexer lexbuf

and parse_type_suffix lexer lexbuf =
  match peek1 lexer lexbuf with
  | LPAREN ->
      next ();
      if peek1 lexer lexbuf = TVOID && peek2 lexer lexbuf = RPAREN then (
        next ();
        next ())
      else parse_func_params lexer lexbuf
  | _ -> print_endline "aaa"

and parse_func_params lexer lexbuf = expected lexer lexbuf RPAREN
