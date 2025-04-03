{
open Env
open Parser

exception LexerError of string

let escaped_chars = [
  ('\\', "\\");
  ('\'', "'");
  ('"', "\"");
  ('n', "\n");
  ('t', "\t");
  ('b', "\b");
  ('r', "\r");
]

let escaped_conv char =
  try 
    List.assoc char escaped_chars
  with Not_found ->
    failwith "escaped_conv error"
}

let digit = ['0'-'9']
let dec = ['1'-'9'] digit*
let hex = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+
let oct = '0' ['0'-'7']*
let integer = dec | hex | oct
let exp = ['E' 'e'] ['+' '-']? digit+
let float1 = digit+ exp
let float2 = digit* '.' digit+ exp?
let float3 = digit+ '.' digit* exp?
let fnum = float1 | float2 | float3
let space = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*
let escapes = ['a' 'b' 't' 'n' 'v' 'f' 'r' '"' '\'' '?' '\\']
let char = [^'\\'] | '\\' (escapes | ['0'-'7']+ | 'x' ['0'-'9' 'a'-'f' 'A'-'F']+)

rule token = parse
| space       { token lexbuf }
| '\n'        { Lexing.new_line lexbuf; token lexbuf }
| ';'         { SEMI }
| ','         { COMMA }
| "_Bool"      { TBOOL }
| "int"       { TINT }
| "long"      { TLONG }
| "short"     { TSHORT }
| "signed"    { TSIGNED }
| "unsigned"  { TUNSIGNED }
| "char"      { TCHAR }
| "float"     { TFLOAT }
| "double"    { TDOUBLE }
| "void"      { TVOID }
| "struct"    { STRUCT }
| "union"     { UNION }
| "enum"      { ENUM }
| "typedef"   { TYPEDEF }
| "auto"      { AUTO }
| "static"    { STATIC }
| "extern"    { EXTERN }
| "inline"    { INLINE }
| "const"     { CONST }
| "volatile"  { VOLATILE }
| "if"        { IF }
| "else"      { ELSE }
| "while"     { WHILE }
| "do"        { DO }
| "for"       { FOR }
| "return"    { RETURN }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "goto"      { GOTO }
| "switch"    { SWITCH }
| "case"      { CASE }
| "default"   { DEFAULT }
| '+'         { PLUS }
| "++"        { INC }
| '-'         { MINUS }
| '!'         { BANG }
| '?'         { QUESTION }
| ':'         { COLON }
| "--"        { DEC }
| '*'         { STAR }
| '/'         { DIV }
| '%'         { MOD }
| "<<"        { LSHIFT }
| ">>"        { RSHIFT }
| '.'         { DOT }
| "->"        { ARROW }
| '&'         { AND }
| '^'         { HAT }
| '|'         { OR }
| "&&"        { ANDAND }
| "||"        { OROR }
| '~'         { NOT }
| "=="        { EQEQ }
| "!="        { NE }
| "="         { EQ }
| "+="        { ADD_EQ }
| "-="        { SUB_EQ }
| "*="        { MUL_EQ }
| "/="        { DIV_EQ }
| "%="        { MOD_EQ }
| "<<="       { LSHIFT_EQ }
| ">>="       { RSHIFT_EQ }
| "&="        { AND_EQ }
| "^="        { XOR_EQ }
| "|="        { OR_EQ }
| "<"         { LT }
| ">"         { GT }
| "<="        { LE }
| ">="        { GE }
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| '['         { LBRACKET }
| ']'         { RBRACKET }
| "sizeof"    { SIZEOF }
| "..."       { ELLIPSIS }
| "//"        { commentbis lexbuf }
| "#"         { commentbis lexbuf }
| "/*"        { comment lexbuf }
| integer as i                  { INT (i) }
| (integer as i) ['u' 'U']      { INT (i) }
| (integer as i) ['l' 'L']      { INT (i) }
| (integer as i) ("ul" | "UL")  { INT (i) }
| '\'' (char as c) '\''         { CHAR (c) }
| (fnum as f) [ 'f' 'F' ]?      { FLOAT (f) }
| (fnum as f) [ 'l' 'L' ]       { FLOAT( f) }
| '"'                           { STR (string "" lexbuf) }
| "__builtin_va_start"          { VA_START }
| "__builtin_va_arg"            { VA_ARG }
| "__builtin_va_end"            { VA_END }
| "__builtin_va_list"           { VA_LIST }
| ident  as n                   { match find_typedef n with
                                  | Some _ -> TYPE_ID n
                                  | None -> ID n 
                                }
| eof   { EOF }
| _     { raise (LexerError ("illegal token '%s'" ^ Lexing.lexeme lexbuf)) }

and string acc = parse
| '"'   { acc }
| '\\' (['\\' '"' 'n' 't' 'b' 'r'] as c)
        { string (acc ^ (escaped_conv c)) lexbuf }
| eof   { failwith "lexer token error" }
| _     { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }

and comment = parse
| "*/"  { token lexbuf }
| '\n'  { Lexing.new_line lexbuf; comment lexbuf }
| eof   { raise (LexerError "unterminated comment") }
| _     { comment lexbuf }

and commentbis = parse
| '\n'  { Lexing.new_line lexbuf; token lexbuf }
| eof   { EOF }
| _     { commentbis lexbuf }
