open Nox
open OUnit2

(* ----- Utility functions ---------------------------------------------------------------------- *)

let check string expected =
  let source = Source.make string in
  try
    let stmts = Parser.parse source in
    ignore (Typechecker.infer Environment.empty stmts);
    let ir = Lowerer.lower Lowerer.init_env stmts in
    let lua = Lua.emit ir in
    assert_equal expected lua
  with
  | Parser.SyntaxError _ -> assert_failure "Expect a valid program"
  | Typechecker.TypeError _ -> assert_failure "Expect a well-typed program"

(* ----- Test functions ------------------------------------------------------------------------- *)

let check_polymorphic_fn _ =
  check "fun f(x) {x}; f(true); f(1)"
    "local f0\nf0 = function(x1)\n  return x1\nend\n_ = f0(true)\nreturn f0(1.)"

let check_recursive_fn _ =
  check "fun fact(n) {if n <= 0 {1} else {fact(n - 1) * n}}; fact(5)"
    "local fact0\n\
     fact0 = function(n1)\n\
    \  if (n1 <= 0.) then\n\
    \    return 1.\n\
    \  else\n\
    \    return (fact0((n1 - 1.)) * n1)\n\
    \  end\n\
     end\n\
     return fact0(5.)"

let check_mutually_recursive_fn _ =
  check
    "fun even(n) {if n == 0 {true} else {odd(n - 1)}} fun odd(n) {if n == 0 {false} else {even(n - \
     1)}}; even(12)"
    "local even0\n\
     local odd1\n\
     even0 = function(n2)\n\
    \  if (n2 == 0.) then\n\
    \    return true\n\
    \  else\n\
    \    return odd1((n2 - 1.))\n\
    \  end\n\
     end\n\
     odd1 = function(n3)\n\
    \  if (n3 == 0.) then\n\
    \    return false\n\
    \  else\n\
    \    return even0((n3 - 1.))\n\
    \  end\n\
     end\n\
     return even0(12.)"

let check_nested_fn _ =
  check "fun f(x) {fun g(x) {x + 1}; g(x)}; f(3)"
    "local f0\n\
     f0 = function(x1)\n\
    \  local g2\n\
    \  g2 = function(x3)\n\
    \    return (x3 + 1.)\n\
    \  end\n\
    \  return g2(x1)\n\
     end\n\
     return f0(3.)"

let check_empty_fn _ =
  check "fun f() {}; f()" "local f0\nf0 = function()\n  return nil\nend\nreturn f0()"

let check_var _ = check "let x = 42; x" "local x0\nx0 = 42.\nreturn x0"

let check_closure_capture _ =
  check "let x = 42; fun f() {x * 2}; let y = f(); y"
    "local x0\n\
     x0 = 42.\n\
     local f1\n\
     f1 = function()\n\
    \  return (x0 * 2.)\n\
     end\n\
     local y2\n\
     y2 = f1()\n\
     return y2"

let check_update _ =
  check "let x = &42; x <- @x + 1" "local x0\nx0 = ({42.})\nx0[1] = (x0[1] + 1.)\nreturn nil";
  check "&42 <- 43" "({42.})[1] = 43.\nreturn nil"

let check_binary_expr _ =
  check "55 * 3 + 1 - 2" "return (((55. * 3.) + 1.) - 2.)";
  check "let x = 42; x + 33" "local x0\nx0 = 42.\nreturn (x0 + 33.)";
  check "\"Hello, \" .. \"World!\"" "return (\"Hello, \" .. \"World!\")"

let check_unary_expr _ =
  check "--3" "return --3.";
  check "!true" "return not true"

let check_block_expr _ =
  check "{let x = 1; if x < 1 {true} else {false}}"
    "local x0\nx0 = 1.\nif (x0 < 1.) then\n  return true\nelse\n  return false\nend"

let check_if_expr _ =
  check "let n = 1; if n < 1 {-1} else if n == 1 {0} else {1}"
    "local n0\n\
     n0 = 1.\n\
     if (n0 < 1.) then\n\
    \  return -1.\n\
     else\n\
    \  if (n0 == 1.) then\n\
    \    return 0.\n\
    \  else\n\
    \    return 1.\n\
    \  end\n\
     end"

let check_application_expr _ =
  check "fun f(x) {x + 1}; f(2)"
    "local f0\nf0 = function(x1)\n  return (x1 + 1.)\nend\nreturn f0(2.)"

let check_lambda_expr _ = check "<x, y> {x + y}" "return function(x0, y1)\n  return (x0 + y1)\nend"

let check_number _ = check "42" "return 42."

let check_boolean _ =
  check "true" "return true";
  check "false" "return false"

let check_string _ = check "\"Some string\"" "return \"Some string\""

let check_unit _ = check "()" "return nil"

(* ----- Tests ---------------------------------------------------------------------------------- *)

let tests =
  "Lua codegen tests"
  >::: [ "Polymorphic functions" >:: check_polymorphic_fn;
         "Recursive functions" >:: check_recursive_fn;
         "Mutually recursive functions" >:: check_mutually_recursive_fn;
         "Nested functions" >:: check_nested_fn;
         "Empty functions" >:: check_empty_fn;
         "Variables" >:: check_var;
         "Closure captured variables" >:: check_closure_capture;
         "Updates" >:: check_update;
         "Binary expressions" >:: check_binary_expr;
         "Unary expressions" >:: check_unary_expr;
         "Block expressions" >:: check_block_expr;
         "If expressions" >:: check_if_expr;
         "Application expressions" >:: check_application_expr;
         "Lambda expressions" >:: check_lambda_expr;
         "Number literals" >:: check_number;
         "Boolean literals" >:: check_boolean;
         "String literals" >:: check_string;
         "Unit literal" >:: check_unit ]

let () = run_test_tt_main tests
