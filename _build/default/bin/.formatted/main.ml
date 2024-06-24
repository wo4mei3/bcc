let () = print_endline "Hello, World!"

let () =
  let lexbuf = Lexing.from_string "int void int a" in
  Bcc.Parser.parse_declspec Bcc.Lexer.token lexbuf false
