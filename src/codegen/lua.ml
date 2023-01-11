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
        (Printf.sprintf "%slocal %s\n" (indent level) name, emit_stmt level stmt, stmts)
      | Ir.Decl x -> (Printf.sprintf "%slocal %s\n" (indent level) x, "", stmts)
      | _ -> ("", "", stmt :: stmts)
    in
    (decl ^ decls, fn ^ fns, stmts)
  | [] -> ("", "", [])

and emit_stmts level stmts =
  let (decls, fns, stmts) = collect_decls level stmts in
  Printf.sprintf "%s%s%s" decls fns (Printer.list_repr stmts (emit_stmt level) "")

and emit_stmt level stmt =
  Printf.sprintf "%s%s\n" (indent level)
    ( match stmt with
    | Ir.Fn (name, params, body) ->
      Printf.sprintf "%s = function(%s)\n%s%send" name
        (Printer.list_repr params (fun p -> p) ", ")
        (emit_stmts (level + 1) body)
        (indent level)
    | Ir.Decl name -> Printf.sprintf "local %s" name
    | Ir.Assign (name, value) -> Printf.sprintf "%s = %s" name (emit_expr level value)
    | Ir.If (cond, thn, els) ->
      Printf.sprintf "if %s then\n%s%selse\n%s%send" (emit_expr level cond)
        (emit_stmts (level + 1) thn)
        (indent level)
        (emit_stmts (level + 1) els)
        (indent level)
    | Ir.Return value -> Printf.sprintf "return %s" (emit_expr level value) )

and emit_expr level = function
  | Ir.Binary (op, lhs, rhs) ->
    Printf.sprintf "(%s %s %s)" (emit_expr level lhs) (emit_binop op) (emit_expr level rhs)
  | Ir.Unary (op, operand) -> Printf.sprintf "%s%s" (emit_unop op) (emit_expr level operand)
  | Ir.App (callee, args) ->
    Printf.sprintf "%s(%s)" (emit_expr level callee) (Printer.list_repr args (emit_expr level) ", ")
  | Ir.Lambda (params, body) ->
    Printf.sprintf "function(%s)\n%s%send"
      (Printer.list_repr params (fun p -> p) ", ")
      (emit_stmts (level + 1) body)
      (indent level)
  | Ir.Var x -> x
  | Ir.Number num -> string_of_float num
  | Ir.Boolean bool -> string_of_bool bool
  | Ir.Unit -> "nil"

let emit stmts = emit_stmts 0 stmts
