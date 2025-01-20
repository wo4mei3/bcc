open Env
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
  | CONST | VOLATILE | INLINE | NORETURN | STRUCT | UNION | ENUM ->
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

let rec make_decl ty d =
  let name = ref "" in
  let rec aux ty = function
    | DeclPtr d -> aux (TPtr ty) d
    | DeclIdent n ->
        name := n;
        ty
    | DeclArr (d, sz) -> aux (TArr (ty, sz)) d
    | DeclFun (d, dl) ->
        aux (TFun (ty, List.flatten (List.map make_param dl))) d
  in
  (!name, aux ty d)

and make_param (ds, dl) =
  List.fold_left (fun l d -> make_decl (TBase ds) d :: l) [] dl

and make_decl2 (ds, dl) =
  List.fold_left (fun l (d, _) -> make_decl (TBase ds) d :: l) [] dl

let rec parse_declspec' lexer lexbuf unique nonunique =
  match peek1 lexer lexbuf with
  | (TVOID | TBOOL | VA_LIST) as tok when not (unique || nonunique) ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf true nonunique
  | STRUCT when not (unique || nonunique) ->
      let ds = parse_struct lexer lexbuf in
      ds :: parse_declspec' lexer lexbuf true nonunique
  | UNION when not (unique || nonunique) ->
      let ds = parse_union lexer lexbuf in
      ds :: parse_declspec' lexer lexbuf true nonunique
  | TYPE_ID n when not (unique || nonunique) ->
      next lexer lexbuf;
      TsTypedef n :: parse_declspec' lexer lexbuf true nonunique
  | (TCHAR | TSHORT | TINT | TLONG | TFLOAT | TDOUBLE | TSIGNED | TUNSIGNED) as
    tok
    when not unique ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf unique true
  | ( TYPEDEF | EXTERN | STATIC | AUTO | REGISTER | CONST | VOLATILE | INLINE
    | NORETURN ) as tok ->
      next lexer lexbuf;
      tok2ds tok :: parse_declspec' lexer lexbuf unique nonunique
  | _ when unique || nonunique -> []
  | _ -> failwith "expected a type specifier"

and parse_declspec lexer lexbuf = parse_declspec' lexer lexbuf false false

and parse_fields lexer lexbuf =
  match peek1 lexer lexbuf with
  | RBRACE -> []
  | _ ->
      let fields = parse_declaration lexer lexbuf in
      fields @ parse_fields lexer lexbuf

and parse_struct lexer lexbuf =
  expect lexer lexbuf STRUCT;
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | ID n, LBRACE ->
      next lexer lexbuf;
      next lexer lexbuf;
      let fields = parse_fields lexer lexbuf in
      let ts = TsStructDef (n, fields) in
      add_struct (n, List.flatten (List.map make_decl2 fields));
      expect lexer lexbuf RBRACE;
      ts
  | LBRACE, _ ->
      next lexer lexbuf;
      let ts = TsStructDef ("", parse_fields lexer lexbuf) in
      expect lexer lexbuf RBRACE;
      ts
  | ID n, _ ->
      next lexer lexbuf;
      TsStruct n
  | _ -> failwith "expected a identifier or a lbrace"

and parse_union lexer lexbuf =
  expect lexer lexbuf STRUCT;
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | ID n, LBRACE ->
      next lexer lexbuf;
      next lexer lexbuf;
      let fields = parse_fields lexer lexbuf in
      let ts = TsUnionDef (n, fields) in
      add_union (n, List.flatten (List.map make_decl2 fields));
      expect lexer lexbuf RBRACE;
      ts
  | LBRACE, _ ->
      next lexer lexbuf;
      let ts = TsUnionDef ("", parse_fields lexer lexbuf) in
      expect lexer lexbuf RBRACE;
      ts
  | ID n, _ ->
      next lexer lexbuf;
      TsStruct n
  | _ -> failwith "expected a identifier or a lbrace"

and parse_declarator lexer lexbuf =
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
    | _ -> failwith "expected a direct declarator"
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
    | _ -> failwith "expected a abstract direct declarator"
  in
  parse_type_suffix lexer lexbuf d

and parse_type_suffix lexer lexbuf d =
  match peek1 lexer lexbuf with
  | LPAREN ->
      next lexer lexbuf;
      if peek1 lexer lexbuf = TVOID && peek2 lexer lexbuf = RPAREN then (
        next lexer lexbuf;
        next lexer lexbuf;
        DeclFun (d, [ ([ TsVoid ], []) ]))
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
        let ds = parse_declspec lexer lexbuf in
        let d = parse_declarator lexer lexbuf in
        let ds' = aux2 lexer lexbuf in
        (ds, d :: ds') :: aux lexer lexbuf
    | RPAREN ->
        next lexer lexbuf;
        []
    | _ -> failwith "expected a RPAREN"
  and aux2 lexer lexbuf =
    match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
    | COMMA, tok when is_typename tok -> []
    | RPAREN, _ -> []
    | _ ->
        next lexer lexbuf;
        let d = parse_declarator lexer lexbuf in
        d :: aux2 lexer lexbuf
  in
  match peek1 lexer lexbuf with
  | RPAREN ->
      next lexer lexbuf;
      parse_type_suffix lexer lexbuf (DeclFun (d, []))
  | _ ->
      let ds = parse_declspec lexer lexbuf in
      let d' = parse_declarator lexer lexbuf in
      let ds' = aux2 lexer lexbuf in
      DeclFun (d, (ds, d' :: ds') :: aux lexer lexbuf)

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
  let ds = parse_declspec lexer lexbuf in
  let d = parse_abstract_declarator lexer lexbuf in
  snd (make_decl (TBase ds) d)

and parse_primary lexer lexbuf =
  match peek1 lexer lexbuf with
  | ID n -> (
      next lexer lexbuf;
      match find_var n with
      | Some ty -> EVar (ty, n)
      | None -> failwith ("var not find: " ^ n))
  | INT i ->
      next lexer lexbuf;
      EConst (TBase [ TsInt ], VInt i)
  | CHAR c ->
      next lexer lexbuf;
      EConst (TBase [ TsChar ], VChar c)
  | FLOAT f ->
      next lexer lexbuf;
      EConst (TBase [ TsFloat ], VFloat f)
  | STR s ->
      next lexer lexbuf;
      EConst (TPtr (TBase [ TsChar ]), VStr s)
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
            ( get_retty (expr_ty e),
              e,
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
            ( get_basety (expr_ty e),
              e,
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
        let ty =
          try List.assoc id (get_members (expr_ty e))
          with _ -> failwith ("no member found: " ^ id)
        in
        let e = EPostfix (ty, e, PDot id) in
        aux lexer lexbuf e
    | ARROW ->
        next lexer lexbuf;
        let id =
          match peek1 lexer lexbuf with
          | ID n | TYPE_ID n -> n
          | _ -> failwith "PDot"
        in
        next lexer lexbuf;
        let ty =
          try List.assoc id (get_members (get_basety (expr_ty e)))
          with _ -> failwith ("no member found: " ^ id)
        in
        let e = EPostfix (ty, e, PArrow id) in
        aux lexer lexbuf e
    | INC ->
        next lexer lexbuf;
        let e = EPostfix (expr_ty e, e, PInc) in
        aux lexer lexbuf e
    | DEC ->
        next lexer lexbuf;
        let e = EPostfix (expr_ty e, e, PDec) in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_primary lexer lexbuf in
  aux lexer lexbuf e

and parse_unary lexer lexbuf =
  match peek1 lexer lexbuf with
  | PLUS ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, Plus, e)
  | MINUS ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, Minus, e)
  | AND ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (TPtr ty, Ref, e)
  | STAR ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (get_basety ty, Deref, e)
  | BANG ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, LogNot, e)
  | NOT ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, BitNot, e)
  | INC ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, Inc, e)
  | DEC ->
      next lexer lexbuf;
      let e = parse_cast lexer lexbuf in
      let ty = expr_ty e in
      EUnary (ty, Dec, e)
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
        let e = EBinary (expr_ty e, Mul, e, parse_cast lexer lexbuf) in
        aux lexer lexbuf e
    | DIV ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Div, e, parse_cast lexer lexbuf) in
        aux lexer lexbuf e
    | MOD ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Mod, e, parse_cast lexer lexbuf) in
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
        let e = EBinary (expr_ty e, Add, e, parse_mul lexer lexbuf) in
        aux lexer lexbuf e
    | MINUS ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Sub, e, parse_mul lexer lexbuf) in
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
        let e = EBinary (expr_ty e, LShift, e, parse_add lexer lexbuf) in
        aux lexer lexbuf e
    | RSHIFT ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, RShift, e, parse_add lexer lexbuf) in
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
        let e = EBinary (expr_ty e, Lt, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | GT ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Gt, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | LE ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Le, e, parse_shift lexer lexbuf) in
        aux lexer lexbuf e
    | GE ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Ge, e, parse_shift lexer lexbuf) in
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
        let e = EBinary (expr_ty e, Eq, e, parse_relational lexer lexbuf) in
        aux lexer lexbuf e
    | NE ->
        next lexer lexbuf;
        let e = EBinary (expr_ty e, Ne, e, parse_relational lexer lexbuf) in
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
        let e = EBinary (expr_ty e, BitAnd, e, parse_equality lexer lexbuf) in
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
        let e = EBinary (expr_ty e, BitXor, e, parse_and lexer lexbuf) in
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
        let e = EBinary (expr_ty e, BitOr, e, parse_xor lexer lexbuf) in
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
        let e = EBinary (expr_ty e, LogAnd, e, parse_or lexer lexbuf) in
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
        let e = EBinary (expr_ty e, LogOr, e, parse_logand lexer lexbuf) in
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
      ECond (expr_ty e2, e, e2, parse_conditional lexer lexbuf)
  | _ -> e

and parse_assign lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', None, e, aux lexer lexbuf e')
    | ADD_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some Add, e, aux lexer lexbuf e')
    | SUB_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some Sub, e, aux lexer lexbuf e')
    | MUL_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some Mul, e, aux lexer lexbuf e')
    | DIV_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some Div, e, aux lexer lexbuf e')
    | MOD_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some Mod, e, aux lexer lexbuf e')
    | AND_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some BitAnd, e, aux lexer lexbuf e')
    | OR_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some BitOr, e, aux lexer lexbuf e')
    | XOR_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some BitXor, e, aux lexer lexbuf e')
    | LSHIFT_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some LShift, e, aux lexer lexbuf e')
    | RSHIFT_EQ ->
        next lexer lexbuf;
        let e' = parse_conditional lexer lexbuf in
        EAssign (expr_ty e', Some RShift, e, aux lexer lexbuf e')
    | _ -> e
  in
  let e = parse_conditional lexer lexbuf in
  aux lexer lexbuf e

and parse_expr lexer lexbuf =
  let rec aux lexer lexbuf e =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        let e' = parse_assign lexer lexbuf in
        let e = EBinary (expr_ty e', Comma, e, e') in
        aux lexer lexbuf e
    | _ -> e
  in
  let e = parse_assign lexer lexbuf in
  aux lexer lexbuf e

and parse_init lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | LBRACE, RBRACE ->
      next lexer lexbuf;
      next lexer lexbuf;
      IVect []
  | LBRACE, COMMA ->
      next lexer lexbuf;
      next lexer lexbuf;
      expect lexer lexbuf RBRACE;
      IVect []
  | LBRACE, _ ->
      next lexer lexbuf;
      let l = parse_init_list lexer lexbuf in
      ignore (consume lexer lexbuf COMMA);
      expect lexer lexbuf RBRACE;
      IVect l
  | _ -> IScal (parse_assign lexer lexbuf)

and parse_init_list lexer lexbuf =
  let rec aux lexer lexbuf =
    if
      peek1 lexer lexbuf = RBRACE
      || (peek1 lexer lexbuf = COMMA && peek2 lexer lexbuf = RBRACE)
    then []
    else (
      expect lexer lexbuf COMMA;
      let i = parse_init lexer lexbuf in
      i :: aux lexer lexbuf)
  in
  let i = IScal (parse_assign lexer lexbuf) in
  i :: aux lexer lexbuf

and parse_init_declarator lexer lexbuf =
  let d = parse_declarator lexer lexbuf in
  if consume lexer lexbuf EQ then (d, Some (parse_init lexer lexbuf))
  else (d, None)

and parse_declaration lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | COMMA ->
        next lexer lexbuf;
        let ds = parse_declspec lexer lexbuf in
        let d = parse_init_declarator lexer lexbuf in
        let ds' = aux2 lexer lexbuf in
        (ds, d :: ds') :: aux lexer lexbuf
    | SEMI ->
        next lexer lexbuf;
        []
    | _ -> failwith "expected a SEMIc"
  and aux2 lexer lexbuf =
    match peek1 lexer lexbuf with
    | SEMI -> []
    | COMMA ->
        next lexer lexbuf;
        let d = parse_init_declarator lexer lexbuf in
        d :: aux2 lexer lexbuf
    | _ -> failwith "expected a SEMIa"
  in
  let ds = parse_declspec lexer lexbuf in
  if consume lexer lexbuf SEMI then (ds, []) :: []
  else
    let d = parse_init_declarator lexer lexbuf in
    let ds' = aux2 lexer lexbuf in
    (ds, d :: ds') :: aux lexer lexbuf

let rec parse_stmt lexer lexbuf =
  match (peek1 lexer lexbuf, peek2 lexer lexbuf) with
  | RETURN, SEMI ->
      next lexer lexbuf;
      next lexer lexbuf;
      SReturn None
  | BREAK, _ ->
      next lexer lexbuf;
      expect lexer lexbuf SEMI;
      SBreak
  | CONTINUE, _ ->
      next lexer lexbuf;
      expect lexer lexbuf SEMI;
      SContinue
  | RETURN, _ ->
      next lexer lexbuf;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf SEMI;
      SReturn (Some e)
  | IF, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf RPAREN;
      let s = parse_stmt lexer lexbuf in
      if not (consume lexer lexbuf ELSE) then SIfElse (e, s, SExpr None)
      else SIfElse (e, s, parse_stmt lexer lexbuf)
  | SWITCH, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf RPAREN;
      SSwitch (e, parse_stmt lexer lexbuf)
  | CASE, _ ->
      next lexer lexbuf;
      let e = parse_conditional lexer lexbuf in
      expect lexer lexbuf COLON;
      SCase e
  | DEFAULT, _ ->
      next lexer lexbuf;
      expect lexer lexbuf COLON;
      SDefault
  | FOR, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      if is_typename (peek1 lexer lexbuf) then (
        let decls = parse_declaration lexer lexbuf in
        push ();
        List.iter add_var (List.flatten (List.map make_decl2 decls));
        let e2 =
          if peek1 lexer lexbuf <> SEMI then Some (parse_expr lexer lexbuf)
          else None
        in
        expect lexer lexbuf SEMI;
        let e3 =
          if peek1 lexer lexbuf <> RPAREN then Some (parse_expr lexer lexbuf)
          else None
        in
        expect lexer lexbuf RPAREN;
        let ret =
          SFor1 (decls, e2, e3, parse_compound_stmt_with_no_action lexer lexbuf)
        in
        pop ();
        ret)
      else
        let e1 =
          if peek1 lexer lexbuf <> SEMI then Some (parse_expr lexer lexbuf)
          else None
        in
        expect lexer lexbuf SEMI;
        let e2 =
          if peek1 lexer lexbuf <> SEMI then Some (parse_expr lexer lexbuf)
          else None
        in
        expect lexer lexbuf SEMI;
        let e3 =
          if peek1 lexer lexbuf <> RPAREN then Some (parse_expr lexer lexbuf)
          else None
        in
        SFor2 (e1, e2, e3, parse_stmt lexer lexbuf)
  | WHILE, _ ->
      next lexer lexbuf;
      expect lexer lexbuf LPAREN;
      let e = parse_expr lexer lexbuf in
      expect lexer lexbuf RPAREN;
      SWhile (e, parse_stmt lexer lexbuf)
  | DO, _ ->
      next lexer lexbuf;
      let s = parse_stmt lexer lexbuf in
      expect lexer lexbuf WHILE;
      expect lexer lexbuf LPAREN;
      let s = SDoWhile (s, parse_expr lexer lexbuf) in
      expect lexer lexbuf RPAREN;
      expect lexer lexbuf SEMI;
      s
  | LBRACE, _ -> parse_compound_stmt lexer lexbuf
  | ty, _ when is_typename ty ->
      let decls = parse_declaration lexer lexbuf in
      List.iter add_var (List.flatten (List.map make_decl2 decls));
      SDecl decls
  | _ ->
      let e = parse_expr lexer lexbuf in
      print_endline "d";
      expect lexer lexbuf SEMI;
      SExpr (Some e)

and parse_compound_stmt lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | RBRACE ->
        next lexer lexbuf;
        []
    | _ ->
        let s = parse_stmt lexer lexbuf in
        s :: aux lexer lexbuf
  in
  expect lexer lexbuf LBRACE;
  push ();
  let stmts = aux lexer lexbuf in
  pop ();
  SStmts stmts

and parse_compound_stmt_with_no_action lexer lexbuf =
  let rec aux lexer lexbuf =
    match peek1 lexer lexbuf with
    | RBRACE ->
        next lexer lexbuf;
        []
    | _ ->
        let s = parse_stmt lexer lexbuf in
        s :: aux lexer lexbuf
  in
  expect lexer lexbuf LBRACE;
  let stmts = aux lexer lexbuf in
  SStmts stmts

let parse_item lexer lexbuf =
  if consume lexer lexbuf SEMI then End
  else if not (is_typename (peek1 lexer lexbuf)) then End
  else
    let ds = parse_declspec lexer lexbuf in
    let rec parse_decl lexer lexbuf =
      if consume lexer lexbuf SEMI then []
      else if consume lexer lexbuf COMMA then
        let d = parse_declarator lexer lexbuf in
        let i =
          if consume lexer lexbuf EQ then Some (parse_init lexer lexbuf)
          else None
        in
        (d, i) :: parse_decl lexer lexbuf
      else failwith "expected a semicolon"
    in
    if consume lexer lexbuf SEMI then Decl (ds, [])
    else
      let d = parse_declarator lexer lexbuf in
      match peek1 lexer lexbuf with
      | LBRACE -> Function ((ds, d), parse_compound_stmt lexer lexbuf)
      | _ ->
          if consume lexer lexbuf SEMI then Decl (ds, (d, None) :: [])
          else if consume lexer lexbuf EQ then (
            let decl =
              Decl
                ( ds,
                  (d, Some (parse_init lexer lexbuf)) :: parse_decl lexer lexbuf
                )
            in
            print_endline "a";
            expect lexer lexbuf SEMI;

            decl)
          else
            let decl = Decl (ds, (d, None) :: parse_decl lexer lexbuf) in
            print_endline "b";
            expect lexer lexbuf SEMI;
            decl

let rec parse_unit lexer lexbuf =
  match parse_item lexer lexbuf with
  | End -> []
  | item -> item :: parse_unit lexer lexbuf
