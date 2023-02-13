open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    let (_, ty) = Typechecker.infer Environment.empty stmts in
    assert_equal expected (Printer.type_repr ty)
  with
  | Parser.SyntaxError _ -> assert_failure "Expect a valid program"
  | Typechecker.TypeError _ -> assert_failure "Expect a well-typed program"

let check_error string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    ignore (Typechecker.infer Environment.empty stmts);
    assert_failure "Expect an ill-typed program"
  with
  | Parser.SyntaxError _ -> assert_failure "Expect a valid program"
  | Typechecker.TypeError diagnostic -> assert_equal expected (Printer.diagnostic_repr diagnostic)

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_polymorphic_fn _ =
  check "fun f(x) {x}; f(true); f(1)" "number";
  check "fun f(a, b) {if true {a} else {b}}; f" "('a, 'a) -> 'a";
  check "fun mkref(x) { &x }; mkref" "('a) -> &'a";
  check "fun deref(r) { @r }; deref" "(&'a) -> 'a";
  check "fun assign(r, x) { r <- x }; assign" "(&'a, 'a) -> unit";
  check "fun get_x(rec) { rec.x }; get_x" "([x : 'a | 'b]) -> 'a";
  check "fun add_x(lhs, rhs) { lhs.x + rhs.x }; add_x"
    "([x : number | 'a], [x : number | 'b]) -> number"

let check_recursive_fn _ =
  check "fun fib(n) {if n == 0 {0} else if n == 1 {1} else {fib(n - 1) + fib(n - 2)}}; fib"
    "(number) -> number";
  check "fun fact(n) {if n <= 0 {1} else {fact(n - 1) * n}}; fact" "(number) -> number"

let check_mutually_recursive_fn _ =
  check "fun f() {g()} fun g() {42}; f" "() -> number";
  check
    "fun even(n) {if n == 0 {true} else {odd(n - 1)}} fun odd(n) {if n == 0 {false} else {even(n - \
     1)}}; even"
    "(number) -> boolean"

let check_empty_fn _ = check "fun f() {}; f" "() -> unit"

let check_invalid_fn _ =
  check_error "fun wrong(b) {if b {b + 1} else {b - 1}}; wrong"
    "1:18..1:19: expect a value of type boolean, but found a number value."

let check_generic_var _ = check "let id = <x> {x}; id(()); id(1); id(true)" "boolean"

let check_var _ =
  check "let x = 10; x" "number";
  check "let b = true; b" "boolean";
  check "let u = (); u" "unit"

let check_update _ = check "let ref = &42; ref <- 44" "unit"

let check_invalid_update _ =
  check_error "let x = 42; x <- 44"
    "1:13..1:14: expect a value of type &'a, but found a number value."

let check_binary_expr _ =
  check "true || false" "boolean";
  check "10 < 100 == 9 > 3" "boolean";
  check "10 + 10" "number";
  check "\"Hello, \" .. \"World!\"" "string"

let check_invalid_binary_expr _ =
  check_error "10 == false" "1:7..1:12: expect a value of type number, but found a boolean value.";
  check_error "true + 1" "1:1..1:5: expect a value of type number, but found a boolean value."

let check_unary_expr _ =
  check "!!true" "boolean";
  check "--10" "number";
  check "&42" "&number";
  check "@&true" "boolean"

let check_invalid_unary_expr _ =
  check_error "!10" "1:2..1:4: expect a value of type boolean, but found a number value.";
  check_error "-false" "1:2..1:7: expect a value of type number, but found a boolean value.";
  check_error "@12" "1:2..1:4: expect a value of type &'a, but found a number value."

let check_if_expr _ =
  check "if true {10} else {-10}" "number";
  check "if 10 < 100 {true} else if -10 > -11 {true} else {false}" "boolean";
  check "if true {}" "unit"

let check_invalid_if_expr _ =
  check_error "if 10 {-1} else {1}"
    "1:4..1:6: expect a value of type boolean, but found a number value.";
  check_error "if true {5}" "1:9..1:12: expect a value of type unit, but found a number value."

let check_application_expr _ = check "fun f(x, y) {x + y}; f(1, 2)" "number"

let check_invalid_application_expr _ =
  check_error "fun f(b, c) {b == c}; f(true, 1)"
    "1:31..1:32: expect a value of type boolean, but found a number value.";
  check_error "fun get_x(r) { r.x }; get_x([b = true])"
    "1:29..1:39: record doesn't contain label 'x'."

let check_record_expr _ =
  check "[a = 42, b = true, c = \"str\"]" "[a : number, b : boolean, c : string | []]";
  check "[a = 42 | [a = true]]" "[a : number | []]";
  check "[a = 42, b = true | [b = 33]]" "[a : number, b : boolean | []]"

let check_select_expr _ =
  check "[a = 42, b = true].a" "number";
  check "[a = [a = true, b = ()]].a.a" "boolean"

let check_invalid_select_expr _ =
  check_error "[a = true].b" "1:12..1:13: record doesn't contain label 'b'."

let check_lambda_expr _ =
  check "<x> {x}" "('a) -> 'a";
  check "<x> {let y = <z> {x(z)}; y}" "(('b) -> 'd) -> ('b) -> 'd"

let check_number _ =
  check "42" "number";
  check "-1337.5" "number"

let check_boolean _ =
  check "true" "boolean";
  check "false" "boolean"

let check_string _ = check "\"A string value\"" "string"

let check_unit _ = check "()" "unit"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Type checker tests"
  >::: [ "Polymorphic functions" >:: check_polymorphic_fn;
         "Recursive functions" >:: check_recursive_fn;
         "Mutually recursive functions" >:: check_mutually_recursive_fn;
         "Empty functions" >:: check_empty_fn;
         "Invalid functions" >:: check_invalid_fn;
         "Generic variables" >:: check_generic_var;
         "Variables" >:: check_var;
         "Updates" >:: check_update;
         "Invalid updates" >:: check_invalid_update;
         "Binary expressions" >:: check_binary_expr;
         "Invalid binary expressions" >:: check_invalid_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "Invalid unary expressions" >:: check_invalid_unary_expr;
         "If expressions" >:: check_if_expr;
         "Invalid if expressions" >:: check_invalid_if_expr;
         "Application expressions" >:: check_application_expr;
         "Invalid application expressions" >:: check_invalid_application_expr;
         "Record expressions" >:: check_record_expr;
         "Select expressions" >:: check_select_expr;
         "Invalid select expressions" >:: check_invalid_select_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Number literals" >:: check_number;
         "Boolean literals" >:: check_boolean;
         "String literals" >:: check_string;
         "Unit literals" >:: check_unit ]

let () = run_test_tt_main tests
