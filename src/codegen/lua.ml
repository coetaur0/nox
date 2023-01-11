(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec indent = function
  | n when n <= 0 -> ""
  | n -> "  " ^ indent (n - 1)

(* ----- Code emitting functions ---------------------------------------------------------------- *)

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

let rec collect_decls level = function
  | stmt :: rest ->
    let (decls, fns, stmts) = collect_decls level rest in
    let (decl, fn, stmts) =
      match stmt with
      | Ir.Fn (name, _, _) ->
        (indent level ^ "local " ^ name ^ "\n", emit_stmt level stmt ^ "\n", stmts)
      | Ir.Decl x -> (indent level ^ "local " ^ x ^ "\n", "", stmts)
      | _ -> ("", "", stmt :: stmts)
    in
    (decl ^ decls, fn ^ fns, stmts)
  | [] -> ("", "", [])

and emit_stmts level stmts =
  let (decls, fns, stmts) = collect_decls level stmts in
  decls ^ fns ^ Printer.list_repr stmts (emit_stmt level) "\n"

and emit_stmt level stmt =
  indent level
  ^
  match stmt with
  | Ir.Fn (name, params, body) ->
    name ^ " = function("
    ^ Printer.list_repr params (fun p -> p) ", "
    ^ ")\n"
    ^ emit_stmts (level + 1) body
    ^ "\n" ^ indent level ^ "end"
  | Ir.Decl name -> "local " ^ name
  | Ir.Assign (name, value) -> name ^ " = " ^ emit_expr level value
  | Ir.If (cond, thn, els) ->
    "if " ^ emit_expr level cond ^ " then\n"
    ^ emit_stmts (level + 1) thn
    ^ "\n" ^ indent level ^ "else\n"
    ^ emit_stmts (level + 1) els
    ^ "\n" ^ indent level ^ "end"
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
    ^ "\n" ^ indent level ^ "end"
  | Ir.Var x -> x
  | Ir.Number num -> string_of_float num
  | Ir.Boolean bool -> string_of_bool bool
  | Ir.Unit -> "nil"

let emit stmts = emit_stmts 0 stmts
