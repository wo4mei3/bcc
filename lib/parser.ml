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

let tok2ds = function
  | TVOID -> TsVoid
  | TBOOL -> TsBool
  | VA_LIST -> TsVarlist
  | TYPE_ID n -> TsTypedef n
  | TCHAR -> TsChar
  | TSHORT -> TsShort
  | TINT -> TsInt
  | TLONG -> TsLong
  | TFLOAT -> TsFloat
  | TDOUBLE -> TsDouble
  | TSIGNED -> TsSigned
  | TUNSIGNED -> TsUnsigned
  | TYPEDEF -> ScsTypedef
  | EXTERN -> ScsExtern
  | STATIC -> ScsStatic
  | AUTO -> ScsAuto
  | REGISTER -> ScsRegister
  | CONST -> TqConst
  | VOLATILE -> TqVolatile
  | INLINE -> FsInline
  | NORETURN -> FsNoreturn
  | _ -> failwith "tok2ds"

let rec parse_declspec' lexer lexbuf unique nonunique =
  match peek1 lexer lexbuf with
  | (TVOID | TBOOL | VA_LIST) as tok when not (unique || nonunique) ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf true nonunique
  | TYPE_ID n when not (unique || nonunique) ->
      next lexer lexbuf;
      TsTypedef n :: parse_declspec' lexer lexbuf true nonunique
  | (TCHAR | TSHORT | TINT | TLONG | TFLOAT | TDOUBLE | TSIGNED | TUNSIGNED) as
    tok
    when not unique ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf unique true
  | TVOID | TBOOL | VA_LIST | TYPE_ID _ | TCHAR | TSHORT | TINT | TLONG | TFLOAT
  | TDOUBLE | TSIGNED | TUNSIGNED ->
      failwith "expected declarator"
  | ( TYPEDEF | EXTERN | STATIC | AUTO | REGISTER | CONST | VOLATILE | INLINE
    | NORETURN ) as tok ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf unique nonunique
  | _ when unique || nonunique -> []
  | _ -> failwith "expected a type specifier"

let parse_declspec lexer lexbuf =
  TBase (parse_declspec' lexer lexbuf false false)

type declarator =
  | DeclPtr of declarator
  | DeclIdent of string
  | DeclArr of declarator * expr option
  | DeclFun of declarator * decl list

let make_decl ty d =
  let name = ref "" in
  let rec aux ty = function
    | DeclPtr d -> aux (TPtr ty) d
    | DeclIdent n ->
        name := n;
        ty
    | DeclArr (d, sz) -> aux (TArr (ty, sz)) d
    | DeclFun (d, dl) -> aux (TFun (ty, dl)) d
  in
  (!name, aux ty d)

let rec parse_declarator lexer lexbuf =
  match peek1 lexer lexbuf with
  | STAR ->
      next lexer lexbuf;
      DeclPtr (parse_declarator lexer lexbuf)
  | ID _ | LPAREN -> parse_direct_declarator lexer lexbuf
  | _ -> failwith "expected a declarator"

and parse_direct_declarator lexer lexbuf =
  let d =
    match peek1 lexer lexbuf with
    | ID n ->
        next lexer lexbuf;
        DeclIdent n
    | LPAREN ->
        next lexer lexbuf;
        let d = parse_declarator lexer lexbuf in
        expect lexer lexbuf RPAREN;
        d
    | _ -> failwith "expected a parse_direct_declaratator"
  in
  parse_type_suffix lexer lexbuf d

and parse_abstract_declarator lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | STAR, (STAR | LPAREN | LBRACKET) ->
      next lexer lexbuf;
      DeclPtr (parse_abstract_declarator lexer lexbuf)
  | STAR, _ ->
      next lexer lexbuf;
      DeclPtr (DeclIdent "")
  | LPAREN, _ | LBRACKET, _ -> parse_abstract_direct_declarator lexer lexbuf
  | _ -> DeclIdent ""

and parse_abstract_direct_declarator lexer lexbuf =
  let d =
    match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
    | LPAREN, (STAR | LPAREN | LBRACKET) ->
        next lexer lexbuf;
        let d = parse_abstract_declarator lexer lexbuf in
        expect lexer lexbuf RPAREN;
        d
    | LPAREN, _ | LBRACKET, _ -> parse_type_suffix lexer lexbuf (DeclIdent "")
    | _ -> failwith "expected a abstract direct declaratator"
  in
  parse_type_suffix lexer lexbuf d

and parse_type_suffix lexer lexbuf d =
  match peek1 lexer lexbuf with
  | LPAREN ->
      next lexer lexbuf;
      if peek1 lexer lexbuf = TVOID && peek2 lexer lexbuf = RPAREN then (
        next lexer lexbuf;
        next lexer lexbuf;
        DeclFun (d, [ ("", TBase [ TsVoid ]) ]))
      else parse_func_params lexer lexbuf d
  | LBRACKET ->
      next lexer lexbuf;
      parse_array_dims lexer lexbuf d
  | _ -> d

and parse_func_params lexer lexbuf d =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        let ty = parse_declspec lexer lexbuf in
        let d = parse_declarator lexer lexbuf in
        make_decl ty d :: aux lexer lexbuf
    | RPAREN ->
        next lexer lexbuf;
        []
    | _ -> failwith "expected a RBRACKET"
  in
  match peek1 lexer lexbuf with
  | RPAREN ->
      next lexer lexbuf;
      parse_type_suffix lexer lexbuf (DeclFun (d, []))
  | _ ->
      let ty = parse_declspec lexer lexbuf in
      let d' = parse_declarator lexer lexbuf in
      DeclFun (d, make_decl ty d' :: aux lexer lexbuf)

and parse_array_dims lexer lexbuf d =
  match peek1 lexer lexbuf with
  | RBRACKET ->
      next lexer lexbuf;
      parse_type_suffix lexer lexbuf (DeclArr (d, None))
  | _ ->
      let e = parse_conditional lexer lexbuf in
      expect lexer lexbuf RBRACKET;
      parse_type_suffix lexer lexbuf (DeclArr (d, Some e))

and parse_typename lexer lexbuf =
  let ty = parse_declspec lexer lexbuf in
  let d = parse_abstract_declarator lexer lexbuf in
  snd (make_decl ty d)

and parse_primary lexer lexbuf =
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
      next lexer lexbuf;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf RPAREN;
      e
  | _ ->
      print_endline (show_token (List.hd !lookup_tokens));
      failwith "expected a expression"

and parse_postfix lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | LPAREN ->
        next lexer lexbuf;
        let rec aux2 lexer lexbuf =
          match peek1 lexer lexbuf with
          | COMMA ->
              next lexer lexbuf;
              let e = parse_assign lexer lexbuf in
              e :: aux2 lexer lexbuf
          | RPAREN ->
              next lexer lexbuf;
              []
          | tok -> failwith ("PCall" ^ show_token tok)
        in
        let e =
          EPostfix
            ( e,
              PCall
                (if consume lexer lexbuf RPAREN then []
                 else
                   let e = parse_assign lexer lexbuf in
                   e :: aux2 lexer lexbuf) )
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
        aux lexer lexbuf e
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
      let ty = parse_typename lexer lexbuf in
      next lexer lexbuf;
      ECast (ty, parse_cast lexer lexbuf)
  | _ -> parse_unary lexer lexbuf

and parse_mul lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | STAR ->
        next lexer lexbuf;
        let e = EBinary (Mul, e, parse_cast lexer lexbuf) in
        aux lexer lexbuf e
    | DIV ->
        next lexer lexbuf;
        let e = EBinary (Div, e, parse_cast lexer lexbuf) in
        aux lexer lexbuf e
    | MOD ->
        next lexer lexbuf;
        let e = EBinary (Mod, e, parse_cast lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_cast lexer lexbuf in
  aux lexer lexbuf e

and parse_add lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | PLUS ->
        next lexer lexbuf;
        let e = EBinary (Add, e, parse_mul lexer lexbuf) in
        aux lexer lexbuf e
    | MINUS ->
        next lexer lexbuf;
        let e = EBinary (Sub, e, parse_mul lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_mul lexer lexbuf in
  aux lexer lexbuf e

and parse_shift lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | LSHIFT ->
        next lexer lexbuf;
        let e = EBinary (LShift, e, parse_add lexer lexbuf) in
        aux lexer lexbuf e
    | RSHIFT ->
        next lexer lexbuf;
        let e = EBinary (RShift, e, parse_add lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_add lexer lexbuf in
  aux lexer lexbuf e

and parse_relational lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | LT ->
        next lexer lexbuf;
        let e = EBinary (Lt, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | GT ->
        next lexer lexbuf;
        let e = EBinary (Gt, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | LE ->
        next lexer lexbuf;
        let e = EBinary (Le, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | GE ->
        next lexer lexbuf;
        let e = EBinary (Ge, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_shift lexer lexbuf in
  aux lexer lexbuf e

and parse_equality lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | EQEQ ->
        next lexer lexbuf;
        let e = EBinary (Eq, e, parse_relational lexer lexbuf) in
        aux lexer lexbuf e
    | NE ->
        next lexer lexbuf;
        let e = EBinary (Ne, e, parse_relational lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_relational lexer lexbuf in
  aux lexer lexbuf e

and parse_and lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | AND ->
        next lexer lexbuf;
        let e = EBinary (BitAnd, e, parse_equality lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_equality lexer lexbuf in
  aux lexer lexbuf e

and parse_xor lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | HAT ->
        next lexer lexbuf;
        let e = EBinary (BitXor, e, parse_and lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_and lexer lexbuf in
  aux lexer lexbuf e

and parse_or lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | OR ->
        next lexer lexbuf;
        let e = EBinary (BitOr, e, parse_xor lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_xor lexer lexbuf in
  aux lexer lexbuf e

and parse_logand lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | ANDAND ->
        next lexer lexbuf;
        let e = EBinary (LogAnd, e, parse_or lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_or lexer lexbuf in
  aux lexer lexbuf e

and parse_logor lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | OROR ->
        next lexer lexbuf;
        let e = EBinary (LogOr, e, parse_logand lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_logand lexer lexbuf in
  aux lexer lexbuf e

and parse_conditional lexer lexbuf =
  let e = parse_logor lexer lexbuf in
  match peek1 lexer lexbuf with
  | QUESTION ->
      next lexer lexbuf;
      let e2 = parse_expr lexer lexbuf in
      expect lexer lexbuf COLON;
      ECond (e, e2, parse_conditional lexer lexbuf)
  | _ -> e

and parse_assign lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (None, e, aux lexer lexbuf e')
    | ADD_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some Add, e, aux lexer lexbuf e')
    | SUB_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some Sub, e, aux lexer lexbuf e')
    | MUL_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some Mul, e, aux lexer lexbuf e')
    | DIV_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some Div, e, aux lexer lexbuf e')
    | MOD_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some Mod, e, aux lexer lexbuf e')
    | AND_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some BitAnd, e, aux lexer lexbuf e')
    | OR_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some BitOr, e, aux lexer lexbuf e')
    | XOR_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some BitXor, e, aux lexer lexbuf e')
    | LSHIFT_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some LShift, e, aux lexer lexbuf e')
    | RSHIFT_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (Some RShift, e, aux lexer lexbuf e')
    | _ -> e
  in
  let e = parse_conditional lexer lexbuf in
  aux lexer lexbuf e

and parse_expr lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        let e = EBinary (Comma, e, parse_assign lexer lexbuf) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_assign lexer lexbuf in
  aux lexer lexbuf e
(*
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
