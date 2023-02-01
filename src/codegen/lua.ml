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
  | Ast.Concat -> ".."

let rec emit_stmts level stmts = Printer.list_repr stmts (emit_stmt level) "\n"

and emit_stmt level stmt =
  match stmt with
  | Ir.Fn fns -> emit_fns level fns
  | Ir.Decl name -> Printf.sprintf "%slocal %s" (indent level) name
  | Ir.Assign (lhs, rhs) ->
    Printf.sprintf "%s%s = %s" (indent level) (emit_expr level lhs) (emit_expr level rhs)
  | Ir.If (cond, thn, els) -> emit_if level cond thn els
  | Ir.Return value -> Printf.sprintf "%sreturn %s" (indent level) (emit_expr level value)

and emit_fns level fns =
  let (decls, defs) =
    List.fold_right
      (fun (name, params, body) (decls, defs) ->
        ( Printf.sprintf "%slocal %s" (indent level) name :: decls,
          emit_fn level name params body :: defs ) )
      fns ([], [])
  in
  Printf.sprintf "%s\n%s"
    (Printer.list_repr decls (fun d -> d) "\n")
    (Printer.list_repr defs (fun d -> d) "\n")

and emit_fn level name params body =
  Printf.sprintf "%s%s = function(%s)\n%s\n%send" (indent level) name
    (Printer.list_repr params (fun p -> p) ", ")
    (emit_stmts (level + 1) body)
    (indent level)

and emit_if level cond thn els =
  Printf.sprintf "%sif %s then\n%s\n%selse\n%s\n%send" (indent level) (emit_expr level cond)
    (emit_stmts (level + 1) thn)
    (indent level)
    (emit_stmts (level + 1) els)
    (indent level)

and emit_expr level = function
  | Ir.Binary (op, lhs, rhs) ->
    Printf.sprintf "(%s %s %s)" (emit_expr level lhs) (emit_binop op) (emit_expr level rhs)
  | Ir.Unary (op, operand) -> emit_unary level op operand
  | Ir.App (callee, args) ->
    Printf.sprintf "%s(%s)" (emit_expr level callee) (Printer.list_repr args (emit_expr level) ", ")
  | Ir.Lambda (params, body) ->
    Printf.sprintf "function(%s)\n%s\n%send"
      (Printer.list_repr params (fun p -> p) ", ")
      (emit_stmts (level + 1) body)
      (indent level)
  | Ir.Var x -> x
  | Ir.Number num -> string_of_float num
  | Ir.Boolean bool -> string_of_bool bool
  | Ir.String string -> Printf.sprintf "\"%s\"" string
  | Ir.Unit -> "nil"

and emit_unary level op operand =
  let (prefix, postfix) =
    match op with
    | Ast.Not -> ("not ", "")
    | Ast.Neg -> ("-", "")
    | Ast.Ref -> ("({", "})")
    | Ast.Deref -> ("", "[1]")
  in
  Printf.sprintf "%s%s%s" prefix (emit_expr level operand) postfix

let emit stmts = emit_stmts 0 stmts
