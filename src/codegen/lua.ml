(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec indent = function
  | n when n <= 0 -> ""
  | n -> "  " ^ indent (n - 1)

(* ----- Code emitting functions ---------------------------------------------------------------- *)

let rec hoist_fns level = function
  | Ir.Fn (name, _, _) :: rest -> indent level ^ "local " ^ name ^ "\n" ^ hoist_fns level rest
  | _ :: rest -> hoist_fns level rest
  | [] -> ""

let emit_binop = function
  | Ast.Or -> "or"
  | Ast.And -> "and"
  | Ast.Eq -> "=="
  | Ast.Ne -> "~="
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"

let emit_unop = function
  | Ast.Not -> "not "
  | Ast.Neg -> "-"

let rec emit_stmts level stmts =
  let rec emit_stmts' level = function
    | stmt :: rest -> indent level ^ emit_stmt level stmt ^ "\n" ^ emit_stmts' level rest
    | [] -> ""
  in
  hoist_fns level stmts ^ emit_stmts' level stmts

and emit_stmt level = function
  | Ir.Fn (name, params, body) ->
    name ^ " = function("
    ^ Printer.list_repr params (fun p -> p) ", "
    ^ ")\n"
    ^ emit_stmts (level + 1) body
    ^ indent level ^ "end"
  | Ir.Decl name -> "local " ^ name
  | Ir.Assign (name, value) -> name ^ " = " ^ emit_expr level value
  | Ir.If (cond, thn, els) ->
    "if " ^ emit_expr level cond ^ " then\n"
    ^ emit_stmts (level + 1) thn
    ^ indent level ^ "else\n"
    ^ emit_stmts (level + 1) els
    ^ indent level ^ "end"
  | Ir.Return value -> "return " ^ emit_expr level value

and emit_expr level = function
  | Ir.Binary (op, lhs, rhs) ->
    "(" ^ emit_expr level lhs ^ " " ^ emit_binop op ^ " " ^ emit_expr level rhs ^ ")"
  | Ir.Unary (op, operand) -> emit_unop op ^ emit_expr level operand
  | Ir.App (callee, args) ->
    emit_expr level callee ^ "(" ^ Printer.list_repr args (emit_expr level) ", " ^ ")"
  | Ir.Lambda (params, body) ->
    "function("
    ^ Printer.list_repr params (fun p -> p) ", "
    ^ ")\n"
    ^ emit_stmts (level + 1) body
    ^ indent level ^ "end"
  | Ir.Var x -> x
  | Ir.Number num -> string_of_float num
  | Ir.Boolean bool -> string_of_bool bool
  | Ir.Unit -> "nil"

let emit stmts = emit_stmts 0 stmts
