open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    assert_equal expected (Printer.ast_repr stmts)
  with Parser.SyntaxError _ -> assert_failure "Expect a valid program"

let check_errors string expected =
  let source = Source.make string in
  try
    ignore (Parser.parse source);
    assert_failure "Expect an invalid program"
  with Parser.SyntaxError diagnostics ->
    assert_equal (List.equal ( = ) (List.map Printer.diagnostic_repr diagnostics) expected) true

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_fn _ =
  check "fun f(x, y, b) {if b {x + y} else {x - y}}"
    "fun f(x, y, b) {if b {(x + y)} else {(x - y)}}";
  check "fun t() {true}" "fun t() {true}"

let check_invalid_fn _ =
  check_errors "fun f x) {x}" ["1:7..1:8: expect a '('."];
  check_errors "fun f(x {x}" ["1:9..1:10: expect a ')'."]

let check_var _ =
  check "let x = 42" "let x = 42.";
  check "let x = {let y = 42; y + 2}" "let x = {let y = 42.; (y + 2.)}"

let check_invalid_var _ =
  check_errors "let = 42" ["1:5..1:6: expect a variable name."];
  check_errors "let x 42" ["1:7..1:9: expect a '='."];
  check_errors "let x =" ["1:8..1:8: expect an expression."]

let check_update _ =
  check "x <- 3" "x <- 3.";
  check "&y <- @z" "&y <- @z"

let check_binary_expr _ =
  check "x - 42 * 3.4 + 10 > 255" "(((x - (42. * 3.4)) + 10.) > 255.)";
  check "true || false && !b" "(true || (false && !b))";
  check "\"Hello, \" .. \"World!\"" "(\"Hello, \" .. \"World!\")"

let check_invalid_binary_expr _ = check_errors "42 -" ["1:5..1:5: expect an expression."]

let check_unary_expr _ =
  check "!!true" "!!true";
  check "---3" "---3.";
  check "&42" "&42.";
  check "@&3" "@&3."

let check_invalid_unary_expr _ = check_errors "!" ["1:2..1:2: expect an expression."]

let check_block_expr _ =
  check "{let x = 10; let y = x - 2; y * 3}" "{let x = 10.; let y = (x - 2.); (y * 3.)}";
  check "{{()}}" "{{()}}"

let check_invalid_block_expr _ = check_errors "{let x = 3; x" ["1:14..1:14: expect a '}'."]

let check_record_expr _ =
  check "[a = 42, b = true]" "[a = 42., b = true | []]";
  check "[]" "[]";
  check "[|[a = 42]]" "[a = 42. | []]"

let check_invalid_record_expr _ =
  check_errors "[= 3]" ["1:2..1:3: expect a field name."];
  check_errors "[a 42]" ["1:4..1:6: expect a '='."]

let check_if_expr _ =
  check "if c == 0 {42} else if c > 0 {-42} else {1}"
    "if (c == 0.) {42.} else if (c > 0.) {-42.} else {1.}";
  check "if true {42}" "if true {42.} else {}"

let check_invalid_if_expr _ =
  check_errors "if true {0} else" ["1:17..1:17: expect a '{'."; "1:17..1:17: expect a '}'."]

let check_match_expr _ =
  check "match v { :Some x => x, :None n => 0 }" "match v { :Some x => x, :None n => 0.,  }";
  check "match :A () { :A a => true, :B b => true, any => false }"
    "match :A () { :A a => true, :B b => true, any => false }"

let check_invalid_match_expr _ =
  check_errors "match x :A a => a, :B b => b}" ["1:9..1:11: expect a '{'."];
  check_errors "match :A 1 { :A => 1 }" ["1:17..1:19: expect a variable name."];
  check_errors "match :A 1 { :A a 1 }" ["1:19..1:20: expect a '=>'."]

let check_application_expr _ =
  check "f()" "f()";
  check "f(32, 1)" "f(32., 1.)";
  check "h(true)(42)" "h(true)(42.)"

let check_invalid_application_expr _ = check_errors "f(2" ["1:4..1:4: expect a ')'."]

let check_select_expr _ =
  check "a.b.c" "a.b.c";
  check "[a = 42].a" "[a = 42. | []].a";
  check "f().g().x" "f().g().x"

let check_lambda_expr _ =
  check "<> {true}" "<> {true}";
  check "<x, y> {x + y}" "<x, y> {(x + y)}"

let check_invalid_lamba_expr _ =
  check_errors "<x {x}" ["1:4..1:5: expect a '>'."];
  check_errors "<x> x" ["1:5..1:6: expect a '{'."; "1:6..1:6: expect a '}'."]

let check_variant_expr _ =
  check ":A true" ":A true";
  check ":X ()" ":X ()"

let check_invalid_variant_expr _ = check_errors ":A" ["1:3..1:3: expect an expression."]

let check_var_expr _ = check "x" "x"

let check_number_expr _ = check "1337.7331" "1337.7331"

let check_boolean_expr _ =
  check "true" "true";
  check "false" "false"

let check_string_expr _ = check "\"Some string!\"" "\"Some string!\""

let check_unit_expr _ = check "()" "()"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Parser tests"
  >::: [ "Functions" >:: check_fn;
         "Invalid functions" >:: check_invalid_fn;
         "Variables" >:: check_var;
         "Invalid variables" >:: check_invalid_var;
         "Updates" >:: check_update;
         "Binary expressions" >:: check_binary_expr;
         "Invalid binary expressions" >:: check_invalid_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "Invalid unary expressions" >:: check_invalid_unary_expr;
         "Block expressions" >:: check_block_expr;
         "Invalid block expressions" >:: check_invalid_block_expr;
         "Record expressions" >:: check_record_expr;
         "Invalid record expressions" >:: check_invalid_record_expr;
         "If expressions" >:: check_if_expr;
         "Invalid if expressions" >:: check_invalid_if_expr;
         "Match expressions" >:: check_match_expr;
         "Invalid match expressions" >:: check_invalid_match_expr;
         "Application expressions" >:: check_application_expr;
         "Invalid application expressions" >:: check_invalid_application_expr;
         "Selection expressions" >:: check_select_expr;
         "Variant expressions" >:: check_variant_expr;
         "Invalid variant expressions" >:: check_invalid_variant_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Invalid lambda expressions" >:: check_invalid_lamba_expr;
         "Variable expressions" >:: check_var_expr;
         "Number expressions" >:: check_number_expr;
         "Boolean expressions" >:: check_boolean_expr;
         "String expressions" >:: check_string_expr;
         "Unit expression" >:: check_unit_expr ]

let () = run_test_tt_main tests
