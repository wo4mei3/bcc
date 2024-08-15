let () = print_endline "Hello, World!"

let () =
  let lexbuf =
    Lexing.from_string "void (*signal(int sig, void(*func)(int a)))(int b);\n"
  in
  let item = Bcc.Parser.parse_item Bcc.Lexer.token lexbuf in
  print_endline (Bcc.Syntax.show_item item);
  Bcc.Parser.next Bcc.Lexer.token lexbuf;
  (*let lexbuf = Lexing.from_string "(void*)a = b[0] = c(1,2)" in
    let expr = Bcc.Parser.parse_expr Bcc.Lexer.token lexbuf in
    print_endline (Bcc.Syntax.show_expr expr);
    Bcc.Parser.next Bcc.Lexer.token lexbuf;*)
  let lexbuf =
    Lexing.from_string
      "int main () { int int; struct X; struct X{int a; int b;} a; a.a;}"
  in
  let item = Bcc.Parser.parse_item Bcc.Lexer.token lexbuf in
  print_endline (Bcc.Syntax.show_item item)
