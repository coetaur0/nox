(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec list_repr list string_fn separator =
  match list with
  | [value] -> string_fn value
  | value :: rest -> string_fn value ^ separator ^ list_repr rest string_fn separator
  | [] -> ""

(* ----- Span representation functions ---------------------------------------------------------- *)

let position_repr position =
  string_of_int Source.(position.line) ^ ":" ^ string_of_int Source.(position.column)

let span_repr span = position_repr Source.(span.left) ^ ".." ^ position_repr Source.(span.right)

(* ----- Diagnostic representation functions ---------------------------------------------------- *)

let diagnostic_repr diagnostic =
  span_repr Diagnostic.(diagnostic.span) ^ ": " ^ Diagnostic.(diagnostic.message) ^ "."

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

let unop_repr op =
  match op with
  | Ast.Not -> "!"
  | Ast.Neg -> "-"

let rec ast_repr stmts = list_repr stmts ast_stmt_repr "; "

and ast_stmt_repr stmt =
  match Ast.(stmt.value) with
  | Ast.Fn (name, params, body) ->
    "fn " ^ name ^ "(" ^ list_repr params (fun p -> p) ", " ^ ") " ^ ast_expr_repr body
  | Ast.Let (name, value) -> "let " ^ name ^ " = " ^ ast_expr_repr value
  | Ast.Expr expr -> ast_expr_repr {value = expr; span = stmt.span}

and ast_expr_repr expr =
  match Ast.(expr.value) with
  | Ast.Binary (op, lhs, rhs) ->
    "(" ^ ast_expr_repr lhs ^ " " ^ binop_repr op ^ " " ^ ast_expr_repr rhs ^ ")"
  | Ast.Unary (op, operand) -> unop_repr op ^ ast_expr_repr operand
  | Ast.Block stmts -> "{" ^ ast_repr stmts ^ "}"
  | Ast.If (cond, thn, els) ->
    let els_string =
      match els with
      | Some expr -> " else " ^ ast_expr_repr expr
      | None -> ""
    in
    "if " ^ ast_expr_repr cond ^ " " ^ ast_expr_repr thn ^ els_string
  | Ast.App (callee, args) -> ast_expr_repr callee ^ "(" ^ list_repr args ast_expr_repr ", " ^ ")"
  | Ast.Lambda (params, body) ->
    "<" ^ list_repr params (fun p -> p) ", " ^ "> " ^ ast_expr_repr body
  | Ast.Var x -> x
  | Ast.Number num -> string_of_float num
  | Ast.Boolean bool -> string_of_bool bool
  | Ast.Unit -> "()"
  | Ast.Invalid -> "<invalid expression>"

(* ----- Types representation functions --------------------------------------------------------- *)

let rec type_repr ty =
  match ty with
  | Types.Fn (params, return) -> "(" ^ list_repr params type_repr ", " ^ ") -> " ^ type_repr return
  | Types.Generic x -> x
  | Types.Var {contents = Bound ty'} -> type_repr ty'
  | Types.Var {contents = Free (x, _)} -> x
  | Types.Number -> "number"
  | Types.Boolean -> "boolean"
  | Types.Unit -> "unit"
