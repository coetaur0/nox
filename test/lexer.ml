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

let check_comments _ = check "// This is a comment\n// And another one" [(Token.Eof, "")]

let check_keywords _ =
  check "open fun let while if else match"
    [ (Token.Open, "open");
      (Token.Fun, "fun");
      (Token.Let, "let");
      (Token.While, "while");
      (Token.If, "if");
      (Token.Else, "else");
      (Token.Match, "match");
      (Token.Eof, "") ]

let check_names _ =
  check "id x1 _a snake_case camelCase PascalCase _"
    [ (Token.Name, "id");
      (Token.Name, "x1");
      (Token.Name, "_a");
      (Token.Name, "snake_case");
      (Token.Name, "camelCase");
      (Token.Name, "PascalCase");
      (Token.Name, "_");
      (Token.Eof, "") ]

let check_cases _ =
  check ":1 :Some :None"
    [(Token.Case, ":1"); (Token.Case, ":Some"); (Token.Case, ":None"); (Token.Eof, "")]

let check_numbers _ =
  check "0 1. 42 1337.7331"
    [ (Token.Number, "0");
      (Token.Number, "1.");
      (Token.Number, "42");
      (Token.Number, "1337.7331");
      (Token.Eof, "") ]

let check_booleans _ =
  check "true false" [(Token.Boolean, "true"); (Token.Boolean, "false"); (Token.Eof, "")]

let check_strings _ =
  check "\"Some string\" \"Unterminated string!"
    [(Token.String, "\"Some string\""); (Token.BadString, "\"Unterminated string!")]

let check_operators _ =
  check "= <- || && == != <= >= < > + - * / .. ! & @"
    [ (Token.Assign, "=");
      (Token.Update, "<-");
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
      (Token.Concat, "..");
      (Token.Not, "!");
      (Token.Ref, "&");
      (Token.Deref, "@");
      (Token.Eof, "") ]

let check_punctuation _ =
  check "(){}[],;|.=>$"
    [ (Token.LParen, "(");
      (Token.RParen, ")");
      (Token.LBrace, "{");
      (Token.RBrace, "}");
      (Token.LBracket, "[");
      (Token.RBracket, "]");
      (Token.Comma, ",");
      (Token.Semicolon, ";");
      (Token.Pipe, "|");
      (Token.Dot, ".");
      (Token.Arrow, "=>");
      (Token.Unknown, "$");
      (Token.Eof, "") ]

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Lexer tests"
  >::: [ "Comments" >:: check_comments;
         "Keywords" >:: check_keywords;
         "Names" >:: check_names;
         "Cases" >:: check_cases;
         "Numbers" >:: check_numbers;
         "Booleans" >:: check_booleans;
         "Strings" >:: check_strings;
         "Operators" >:: check_operators;
         "Punctuation" >:: check_punctuation ]

let () = run_test_tt_main tests
