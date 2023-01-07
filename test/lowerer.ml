open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    ignore (Typechecker.infer stmts);
    let ir = Lowerer.lower stmts in
    assert_equal expected (Printer.ir_repr ir)
  with
  | Parser.SyntaxError _ -> assert_failure "Expect a valid program"
  | Typechecker.TypeError _ -> assert_failure "Expect a well-typed program"

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_fn _ =
  check "fn add(x, y) {x + y}" "fn add0(x1, y2) {return (x1 + y2)}; return ()";
  check "fn fib(n) {if n == 0 {0} else if n == 1 {1} else {fib(n - 1) + fib(n - 2)}}; fib"
    "fn fib0(n1) {if (n1 == 0.) {return 0.} else {if (n1 == 1.) {return 1.} else {return (fib0((n1 \
     - 1.)) + fib0((n1 - 2.)))}}}; return fib0"

let check_var _ =
  check "let x = 42; x" "let x0; x0 = 42.; return x0";
  check "let f = <x> {x + 1}; f(1)" "let f0; f0 = <x1> {return (x1 + 1.)}; return f0(1.)"

let check_binary_expr _ =
  check "55 * 3 + 1 - 2" "return (((55. * 3.) + 1.) - 2.)";
  check "let x = 42; x + 33" "let x0; x0 = 42.; return (x0 + 33.)"

let check_unary_expr _ =
  check "--3" "return --3.";
  check "!true" "return !true"

let check_block_expr _ =
  check "{let x = 1; if x < 1 {true} else {false}}"
    "let x0; x0 = 1.; if (x0 < 1.) {return true} else {return false}"

let check_if_expr _ =
  check "let n = 1; if n < 1 {-1} else if n == 1 {0} else {1}"
    "let n0; n0 = 1.; if (n0 < 1.) {return -1.} else {if (n0 == 1.) {return 0.} else {return 1.}}"

let check_application_expr _ =
  check "fn f(x) {x + 1}; f(2)" "fn f0(x1) {return (x1 + 1.)}; return f0(2.)";
  check "<x> {x * 2}(3)" "return <x0> {return (x0 * 2.)}(3.)"

let check_lambda_expr _ = check "<x, y> {x + y}" "return <x0, y1> {return (x0 + y1)}"

let check_number _ = check "42" "return 42."

let check_boolean _ =
  check "true" "return true";
  check "false" "return false"

let check_unit _ = check "()" "return ()"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "AST lowering tests"
  >::: [ "Functions" >:: check_fn;
         "Variables" >:: check_var;
         "Binary expressions" >:: check_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "Block expressions" >:: check_block_expr;
         "If expressions" >:: check_if_expr;
         "Application expressions" >:: check_application_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Number literals" >:: check_number;
         "Boolean literals" >:: check_boolean;
         "Unit literal" >:: check_unit ]

let () = run_test_tt_main tests
