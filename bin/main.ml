let () = print_endline "Hello, World!"

let () =
  let lexbuf =
    Lexing.from_string "void (*signal(int sig, void(*func)(int a)))(int b);\n"
  in
  let item = Bcc.Parser.parse_item Bcc.Lexer.token lexbuf in
  print_endline (Bcc.Syntax.show_item item);
  Bcc.Parser.next Bcc.Lexer.token lexbuf;
  let lexbuf =
    Lexing.from_string
      "struct X;int;int main () { int int; struct X a; struct X{int a; int \
       b;};  a.a;}"
  in
  let item = Bcc.Parser.parse_unit Bcc.Lexer.token lexbuf in
  print_endline (Bcc.Syntax.show_unit item)
