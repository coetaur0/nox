open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    ignore (Typechecker.infer Environment.empty stmts);
    let (_, value) = Interpreter.run Environment.empty stmts in
    assert_equal expected (Printer.value_repr value)
  with
  | Parser.SyntaxError _ -> assert_failure "Expect a valid program"
  | Typechecker.TypeError _ -> assert_failure "Expect a well-typed program"

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_polymorphic_fn _ =
  check "fun f(x) {x}; f(true); f(1)" "1.";
  check "fun f(a, b) {if true {a} else {b}}; f" "<closure>"

let check_recursive_fn _ =
  check "fun fib(n) {if n == 0 {0} else if n == 1 {1} else {fib(n - 1) + fib(n - 2)}}; fib(5)" "5.";
  check "fun fact(n) {if n <= 0 {1} else {fact(n - 1) * n}}; fact(5)" "120."

let check_mutually_recursive_fn _ =
  check
    "fun even(n) {if n == 0 {true} else {odd(n - 1)}} fun odd(n) {if n == 0 {false} else {even(n - \
     1)}}; even(12)"
    "true"

let check_empty_fn _ = check "fun f() {}; f()" "()"

let check_var _ =
  check "let x = 10; x" "10.";
  check "let b = true; b" "true";
  check "let u = (); u" "()"

let check_while_loop _ =
  check "let c = &3; let r = &1; while @c > 0 { r <- @r * 2; c <- @c - 1 }; @r" "8."

let check_binary_expr _ =
  check "true || false" "true";
  check "10 < 100 == 9 > 3" "true";
  check "10 + 10" "20.";
  check "\"Hello, \" .. \"World!\"" "\"Hello, World!\""

let check_unary_expr _ =
  check "!!false" "false";
  check "-10" "-10."

let check_if_expr _ =
  check "if true {10} else {-10}" "10.";
  check "if 10 > 100 {true} else if -10 > -11 {false} else {true}" "false";
  check "if true {}" "()"

let check_match_expr _ =
  check "match :A 42 { :A a => 2 * a, _ => 0 }" "84.";
  check "match :None () { :Some b => b, _ => false }" "false"

let check_application_expr _ =
  check "fun f(x, y) {x + y}; f(1, 2)" "3.";
  check "fun mkref(x) { &x }; mkref(3)" "&3.";
  check "fun assign(r, x) { r <- x; x }; assign(&42, 43)" "43."

let check_record_expr _ =
  check "{num = 42, bool = true, str = \"some string\"}"
    "{bool = true, num = 42., str = \"some string\"}";
  check "{}" "{}"

let check_select_expr _ = check "{a = 32, b = ()}.a" "32."

let check_variant_expr _ = check ":A 42" ":A 42."

let check_array_expr _ =
  check "[1, 3, 5, 7, 9]" "[1., 3., 5., 7., 9.]";
  check "[[true, false], [false]]" "[[true, false], [false]]"

let check_index_expr _ =
  check "[1, 3, 5, 7, 9][2]" "5.";
  check "[[true, false], [false]][0][1]" "false"

let check_lambda_expr _ =
  check "<x> {x}" "<closure>";
  check "<x> {let y = <z> {x(z)}; y}" "<closure>"

let check_number _ =
  check "42" "42.";
  check "-1337.5" "-1337.5"

let check_boolean _ =
  check "true" "true";
  check "false" "false"

let check_string _ = check "\"Some string\"" "\"Some string\""

let check_unit _ = check "()" "()"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Interpreter tests"
  >::: [ "Polymorphic functions" >:: check_polymorphic_fn;
         "Recursive functions" >:: check_recursive_fn;
         "Mutually recursive functions" >:: check_mutually_recursive_fn;
         "Empty functions" >:: check_empty_fn;
         "Variables" >:: check_var;
         "While loops" >:: check_while_loop;
         "Binary expressions" >:: check_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "If expressions" >:: check_if_expr;
         "Match expressions" >:: check_match_expr;
         "Application expressions" >:: check_application_expr;
         "Record expressions" >:: check_record_expr;
         "Select expressions" >:: check_select_expr;
         "Variant expressions" >:: check_variant_expr;
         "Array expressions" >:: check_array_expr;
         "Index expressions" >:: check_index_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Number literals" >:: check_number;
         "Boolean literals" >:: check_boolean;
         "String literals" >:: check_string;
         "Unit literals" >:: check_unit ]

let () = run_test_tt_main tests
