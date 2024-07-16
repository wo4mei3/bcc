let () = print_endline "Hello, World!"

let () =
  (*let lexbuf = Lexing.from_string "int int (*a(int a, int b[][]))[][](void)" in
    Bcc.Parser.parse_declspec Bcc.Lexer.token lexbuf;
    Bcc.Parser.parse_declarator Bcc.Lexer.token lexbuf;
    let lexbuf = Lexing.from_string "a().a + 5" in
    Bcc.Parser.parse_expr Bcc.Lexer.token lexbuf;
    Bcc.Parser.next Bcc.Lexer.token lexbuf;*)
  let lexbuf = Lexing.from_string "int main () {for (;;) {a().a + 5;}}" in
  Bcc.Parser.parse_function Bcc.Lexer.token lexbuf
