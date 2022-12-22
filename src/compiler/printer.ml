(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec string_of_list list string_fn separator =
  match list with
  | [value] -> string_fn value
  | value :: rest -> string_fn value ^ separator ^ string_of_list rest string_fn separator
  | [] -> ""

(* ----- Span representation functions ---------------------------------------------------------- *)

let string_of_position position =
  string_of_int Source.(position.line) ^ ":" ^ string_of_int Source.(position.column)

let string_of_span span =
  string_of_position Source.(span.left) ^ ".." ^ string_of_position Source.(span.right)

(* ----- Diagnostic representation functions ---------------------------------------------------- *)

let string_of_diagnostic diagnostic =
  string_of_span Diagnostic.(diagnostic.span) ^ ": " ^ Diagnostic.(diagnostic.message) ^ "."

(* ----- AST representation functions ----------------------------------------------------------- *)

let string_of_binop op =
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

let string_of_unop op =
  match op with
  | Ast.Not -> "!"
  | Ast.Neg -> "-"

let rec string_of_stmts stmts = string_of_list stmts string_of_stmt "; "

and string_of_stmt stmt =
  match Ast.(stmt.value) with
  | Ast.Fn (name, params, body) ->
    "fn " ^ name.value ^ "("
    ^ string_of_list params (fun p -> p.value) ", "
    ^ ") " ^ string_of_expr body
  | Ast.Let (name, value) -> "let " ^ name.value ^ " = " ^ string_of_expr value
  | Ast.Expr expr -> string_of_expr {value = expr; span = stmt.span}

and string_of_expr expr =
  match Ast.(expr.value) with
  | Ast.Binary (op, lhs, rhs) ->
    "(" ^ string_of_expr lhs ^ " " ^ string_of_binop op ^ " " ^ string_of_expr rhs ^ ")"
  | Ast.Unary (op, operand) -> string_of_unop op ^ string_of_expr operand
  | Ast.Block stmts -> "{" ^ string_of_stmts stmts ^ "}"
  | Ast.If (cond, thn, els) ->
    let els_string =
      match els with
      | Some expr -> " else " ^ string_of_expr expr
      | None -> ""
    in
    "if " ^ string_of_expr cond ^ " " ^ string_of_expr thn ^ els_string
  | Ast.App (callee, args) ->
    string_of_expr callee ^ "(" ^ string_of_list args string_of_expr ", " ^ ")"
  | Ast.Lambda (params, body) ->
    "<" ^ string_of_list params (fun p -> p.value) ", " ^ "> " ^ string_of_expr body
  | Ast.Var x -> x
  | Ast.Number num -> string_of_float num
  | Ast.Boolean bool -> string_of_bool bool
  | Ast.Unit -> "()"
  | Ast.Invalid -> "<invalid expression>"

(* ----- Types representation functions --------------------------------------------------------- *)

let rec string_of_type ty =
  match ty with
  | Types.Fn (params, return) ->
    "(" ^ string_of_list params string_of_type ", " ^ ") -> " ^ string_of_type return
  | Types.Generic x -> x
  | Types.Var {contents = Bound ty'} -> string_of_type ty'
  | Types.Var {contents = Free (x, _)} -> x
  | Types.Number -> "number"
  | Types.Boolean -> "boolean"
  | Types.Unit -> "unit"
