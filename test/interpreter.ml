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
  check "fn f(x) {x}; f(true); f(1)" "1.";
  check "fn f(a, b) {if true {a} else {b}}; f" "<closure>"

let check_recursive_fn _ =
  check "fn fib(n) {if n == 0 {0} else if n == 1 {1} else {fib(n - 1) + fib(n - 2)}}; fib(5)" "5.";
  check "fn fact(n) {if n <= 0 {1} else {fact(n - 1) * n}}; fact(5)" "120."

let check_mutually_recursive_fn _ =
  check
    "fn even(n) {if n == 0 {true} else {odd(n - 1)}} fn odd(n) {if n == 0 {false} else {even(n - \
     1)}}; even(12)"
    "true"

let check_empty_fn _ = check "fn f() {}; f()" "()"

let check_var _ =
  check "let x = 10; x" "10.";
  check "let b = true; b" "true";
  check "let u = (); u" "()"

let check_binary_expr _ =
  check "true || false" "true";
  check "10 < 100 == 9 > 3" "true";
  check "10 + 10" "20."

let check_unary_expr _ =
  check "!!false" "false";
  check "-10" "-10."

let check_if_expr _ =
  check "if true {10} else {-10}" "10.";
  check "if 10 > 100 {true} else if -10 > -11 {false} else {true}" "false";
  check "if true {}" "()"

let check_application_expr _ = check "fn f(x, y) {x + y}; f(1, 2)" "3."

let check_lambda_expr _ =
  check "<x> {x}" "<closure>";
  check "<x> {let y = <z> {x(z)}; y}" "<closure>"

let check_number _ =
  check "42" "42.";
  check "-1337.5" "-1337.5"

let check_boolean _ =
  check "true" "true";
  check "false" "false"

let check_unit _ = check "()" "()"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Interpreter tests"
  >::: [ "Polymorphic functions" >:: check_polymorphic_fn;
         "Recursive functions" >:: check_recursive_fn;
         "Mutually recursive functions" >:: check_mutually_recursive_fn;
         "Empty functions" >:: check_empty_fn;
         "Variables" >:: check_var;
         "Binary expressions" >:: check_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "If expressions" >:: check_if_expr;
         "Application expressions" >:: check_application_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Number literals" >:: check_number;
         "Boolean literals" >:: check_boolean;
         "Unit literals" >:: check_unit ]

let () = run_test_tt_main tests
