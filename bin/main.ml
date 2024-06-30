let () = print_endline "Hello, World!"

let () =
  let lexbuf = Lexing.from_string "int int (*a(int a, int b[4]))[](void)" in
  Bcc.Parser.parse_declspec Bcc.Lexer.token lexbuf false false;
  Bcc.Parser.parse_declarator Bcc.Lexer.token lexbuf;
  let lexbuf = Lexing.from_string "1 + 2 - 3 + 4 > 4 ? 4 : 5  = 1 = 1 , 2" in
  Bcc.Parser.parse_expr Bcc.Lexer.token lexbuf
