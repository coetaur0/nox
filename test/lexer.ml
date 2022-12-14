open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string tokens =
  let source = Source.make string in
  let lexer = Lexer.make source in
  let rec check' tokens =
    match tokens with
    | (kind, value) :: rest ->
      let token = Lexer.next lexer in
      assert_equal kind token.kind;
      assert_equal value (Source.read source token.span);
      check' rest
    | _ -> ()
  in
  check' tokens

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_comments _ =
  check "// This is a comment\n// And another one" [(Token.Eof, "")]

let check_keywords _ =
  check
    "fn let if else"
    [(Token.Fn, "fn");
     (Token.Let, "let");
     (Token.If, "if");
     (Token.Else, "else");
     (Token.Eof, "")]

let check_names _ =
  check
    "id x1 _a snake_case camelCase PascalCase _"
    [(Token.Name, "id");
     (Token.Name, "x1");
     (Token.Name, "_a");
     (Token.Name, "snake_case");
     (Token.Name, "camelCase");
     (Token.Name, "PascalCase");
     (Token.Name, "_");
     (Token.Eof, "")]

let check_numbers _ =
  check
    "0 1. 42 1337.7331"
    [(Token.Number, "0");
     (Token.Number, "1.");
     (Token.Number, "42");
     (Token.Number, "1337.7331");
     (Token.Eof, "")]

let check_booleans _ =
  check
    "true false"
    [(Token.Boolean, "true");
    (Token.Boolean, "false");
    (Token.Eof, "")]

let check_operators _ =
  check
    "= || && == != <= >= < > + - * / !"
    [(Token.Assign, "=");
     (Token.Or, "||");
     (Token.And, "&&");
     (Token.Eq, "==");
     (Token.Ne, "!=");
     (Token.Le, "<=");
     (Token.Ge, ">=");
     (Token.Lt, "<");
     (Token.Gt, ">");
     (Token.Add, "+");
     (Token.Sub, "-");
     (Token.Mul, "*");
     (Token.Div, "/");
     (Token.Not, "!");
     (Token.Eof, "")]

let check_punctuation _ =
  check
    "(){},;"
    [(Token.LParen, "(");
    (Token.RParen, ")");
    (Token.LBrace, "{");
    (Token.RBrace, "}");
    (Token.Comma, ",");
    (Token.Semicolon, ";");
    (Token.Eof, "")]

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Lexer tests"
  >:::
  [("Comments" >:: check_comments);
   ("Keywords" >:: check_keywords);
   ("Names" >:: check_names);
   ("Numbers" >:: check_numbers);
   ("Booleans" >:: check_booleans);
   ("Operators" >:: check_operators);
   ("Punctuation" >:: check_punctuation)]

let () =
  run_test_tt_main tests
