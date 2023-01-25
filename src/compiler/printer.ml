(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec list_repr list string_fn separator =
  match list with
  | [value] -> string_fn value
  | value :: rest ->
    Printf.sprintf "%s%s%s" (string_fn value) separator (list_repr rest string_fn separator)
  | [] -> ""

(* ----- Span representation functions ---------------------------------------------------------- *)

let position_repr position = Printf.sprintf "%d:%d" Source.(position.line) Source.(position.column)

let span_repr span =
  Printf.sprintf "%s..%s" (position_repr Source.(span.left)) (position_repr Source.(span.right))

(* ----- Diagnostic representation functions ---------------------------------------------------- *)

let diagnostic_repr diagnostic =
  Printf.sprintf "%s: %s." (span_repr Diagnostic.(diagnostic.span)) Diagnostic.(diagnostic.message)

(* ----- AST representation functions ----------------------------------------------------------- *)

let binop_repr op =
  match op with
  | Ast.Or -> "||"
  | Ast.And -> "&&"
  | Ast.Eq -> "=="
  | Ast.Ne -> "!="
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Concat -> ".."

let unop_repr op =
  match op with
  | Ast.Not -> "!"
  | Ast.Neg -> "-"

let rec ast_repr stmts = list_repr stmts ast_stmt_repr "; "

and ast_stmt_repr stmt =
  match Ast.(stmt.value) with
  | Ast.Fn fns ->
    list_repr fns
      (fun (name, params, body) ->
        Printf.sprintf "fn %s(%s) %s" name (list_repr params (fun p -> p) ", ") (ast_expr_repr body)
        )
      " "
  | Ast.Let (name, value) -> Printf.sprintf "let %s = %s" name (ast_expr_repr value)
  | Ast.Expr expr -> ast_expr_repr {value = expr; span = stmt.span}

and ast_expr_repr expr =
  match Ast.(expr.value) with
  | Ast.Binary (op, lhs, rhs) ->
    Printf.sprintf "(%s %s %s)" (ast_expr_repr lhs) (binop_repr op) (ast_expr_repr rhs)
  | Ast.Unary (op, operand) -> Printf.sprintf "%s%s" (unop_repr op) (ast_expr_repr operand)
  | Ast.Block stmts -> Printf.sprintf "{%s}" (ast_repr stmts)
  | Ast.If (cond, thn, els) ->
    Printf.sprintf "if %s %s else %s" (ast_expr_repr cond) (ast_expr_repr thn) (ast_expr_repr els)
  | Ast.App (callee, args) ->
    Printf.sprintf "%s(%s)" (ast_expr_repr callee) (list_repr args ast_expr_repr ", ")
  | Ast.Lambda (params, body) ->
    Printf.sprintf "<%s> %s" (list_repr params (fun p -> p) ", ") (ast_expr_repr body)
  | Ast.Var x -> x
  | Ast.Number num -> string_of_float num
  | Ast.Boolean bool -> string_of_bool bool
  | Ast.String string -> Printf.sprintf "\"%s\"" string
  | Ast.Unit -> "()"
  | Ast.Invalid -> "<invalid expression>"

(* ----- Types representation functions --------------------------------------------------------- *)

let rec type_repr ty =
  match ty with
  | Types.Fn (params, return) ->
    Printf.sprintf "(%s) -> %s" (list_repr params type_repr ", ") (type_repr return)
  | Types.Generic x -> x
  | Types.Var {contents = Bound ty'} -> type_repr ty'
  | Types.Var {contents = Free (x, _)} -> x
  | Types.Number -> "number"
  | Types.Boolean -> "boolean"
  | Types.String -> "string"
  | Types.Unit -> "unit"

(* ----- Runtime values representation functions ------------------------------------------------ *)

let value_repr = function
  | Values.Closure _ -> "<closure>"
  | Values.NativeFn _ -> "<native fn>"
  | Values.Number num -> string_of_float num
  | Values.Boolean bool -> string_of_bool bool
  | Values.String string -> Printf.sprintf "\"%s\"" string
  | Values.Unit -> "()"

(* ----- IR representation functions ------------------------------------------------------------ *)

let rec ir_repr stmts = list_repr stmts ir_stmt_repr "; "

and ir_stmt_repr = function
  | Ir.Fn fns ->
    list_repr fns
      (fun (name, params, body) ->
        Printf.sprintf "fn %s(%s) {%s}" name (list_repr params (fun p -> p) ", ") (ir_repr body) )
      " "
  | Ir.Decl name -> Printf.sprintf "let %s" name
  | Ir.Assign (name, value) -> Printf.sprintf "%s = %s" name (ir_expr_repr value)
  | Ir.If (cond, thn, els) ->
    Printf.sprintf "if %s {%s} else {%s}" (ir_expr_repr cond) (ir_repr thn) (ir_repr els)
  | Ir.Return value -> Printf.sprintf "return %s" (ir_expr_repr value)

and ir_expr_repr = function
  | Ir.Binary (op, lhs, rhs) ->
    Printf.sprintf "(%s %s %s)" (ir_expr_repr lhs) (binop_repr op) (ir_expr_repr rhs)
  | Ir.Unary (op, operand) -> Printf.sprintf "%s%s" (unop_repr op) (ir_expr_repr operand)
  | Ir.App (callee, args) ->
    Printf.sprintf "%s(%s)" (ir_expr_repr callee) (list_repr args ir_expr_repr ", ")
  | Ir.Lambda (params, body) ->
    Printf.sprintf "<%s> {%s}" (list_repr params (fun p -> p) ", ") (ir_repr body)
  | Ir.Var x -> x
  | Ir.Number num -> string_of_float num
  | Ir.Boolean bool -> string_of_bool bool
  | Ir.String string -> Printf.sprintf "\"%s\"" string
  | Ir.Unit -> "()"
