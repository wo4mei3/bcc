let () = print_endline "Hello, World!"

let () =
  let lexbuf = Lexing.from_string "int int (*a(int a, int b[][]))[][](void)" in
    Bcc.Parser.parse_declspec Bcc.Lexer.token lexbuf;
    Bcc.Parser.parse_declarator Bcc.Lexer.token lexbuf;
    let lexbuf = Lexing.from_string "++a.a" in
    let expr = Bcc.Parser.parse_unary Bcc.Lexer.token lexbuf in
    print_endline (Bcc.Syntax.show_expr expr);
    Bcc.Parser.next Bcc.Lexer.token lexbuf(*;
  let lexbuf = Lexing.from_string "int main () {int a;}" in
  Bcc.Parser.parse_function Bcc.Lexer.token lexbuf*)
