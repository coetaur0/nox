(* ----- Lowering environment ------------------------------------------------------------------- *)

type env = string Environment.t

let init_env = Environment.of_list [("print", "print"); ("num2str", "tostring")]

(* ----- Utility functions ---------------------------------------------------------------------- *)

let current_id = ref 0

let reset_id () = current_id := 0

let gensym base =
  let id = !current_id in
  incr current_id;
  Printf.sprintf "%s%d" base id

let mangle env name =
  let name' = gensym name in
  (Environment.add name name' env, name')

let mangle_list env names =
  let (env', names') =
    List.fold_left
      (fun (env, names) name ->
        let (env', name') = mangle env name in
        (env', name' :: names) )
      (env, []) names
  in
  (env', List.rev names')

(* ----- Lowering functions --------------------------------------------------------------------- *)

let rec lower_stmts env = function
  | [Ast.{value = Expr expr; span}] -> return env Ast.{value = expr; span}
  | stmt :: rest ->
    let (env', ir_stmts) = lower_stmt env stmt in
    ir_stmts @ lower_stmts env' rest
  | [] -> [Ir.Return Ir.Unit]

and lower_stmt env node =
  match Ast.(node.value) with
  | Ast.Fn fns -> lower_fns env fns
  | Ast.Let (name, value) -> lower_let env name value
  | Ast.Expr expr -> (env, assign env "_" Ast.{value = expr; span = node.span})

and lower_fns env fns =
  let env' =
    List.fold_left (fun env' (name, _, _) -> Environment.add name (gensym name) env') env fns
  in
  let ir_fns = List.map (fun (name, params, body) -> lower_fn env' name params body) fns in
  (env', [Ir.Fn ir_fns])

and lower_fn env name params body =
  let (env', params') = mangle_list env params in
  (Environment.find name env, params', return env' body)

and return env node =
  match Ast.(node.value) with
  | Ast.Block stmts -> lower_stmts env stmts
  | Ast.If (cond, thn, els) ->
    let (ir_stmts, ir_expr) = lower_expr env cond in
    ir_stmts @ [Ir.If (ir_expr, return env thn, return env els)]
  | _ ->
    let (ir_stmts, ir_expr) = lower_expr env node in
    ir_stmts @ [Ir.Return ir_expr]

and lower_let env name value =
  let (env', name') = mangle env name in
  (env', [Ir.Decl name'] @ assign env' name' value)

and assign env name node =
  let rec assign_stmts env name = function
    | [Ast.{value = Expr expr; span}] ->
      let (ir_stmts, ir_expr) = lower_expr env Ast.{value = expr; span} in
      ir_stmts @ [Ir.Assign (name, ir_expr)]
    | stmt :: rest ->
      let (env', ir_stmts) = lower_stmt env stmt in
      ir_stmts @ assign_stmts env' name rest
    | [] -> [Ir.Assign (name, Ir.Unit)]
  in
  match Ast.(node.value) with
  | Ast.Block stmts -> assign_stmts env name stmts
  | Ast.If (cond, thn, els) ->
    let (ir_stmts, ir_expr) = lower_expr env cond in
    ir_stmts @ [Ir.If (ir_expr, assign env name thn, assign env name els)]
  | _ ->
    let (ir_stmts, ir_expr) = lower_expr env node in
    ir_stmts @ [Ir.Assign (name, ir_expr)]

and lower_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> lower_binary env op lhs rhs
  | Ast.Unary (op, operand) -> lower_unary env op operand
  | Ast.Block _ | Ast.If _ ->
    let tmp = gensym "tmp" in
    let ir_stmts = assign env tmp node in
    ([Ir.Decl tmp] @ ir_stmts, Ir.Var tmp)
  | Ast.App (callee, args) -> lower_app env callee args
  | Ast.Lambda (params, body) -> lower_lambda env params body
  | Ast.Var x -> ([], Ir.Var (Environment.find x env))
  | Ast.Number num -> ([], Ir.Number num)
  | Ast.Boolean bool -> ([], Ir.Boolean bool)
  | Ast.String string -> ([], Ir.String string)
  | Ast.Unit -> ([], Ir.Unit)
  | Ast.Invalid -> failwith "Unreachable case"

and lower_binary env op lhs rhs =
  let (lhs_stmts, lhs_expr) = lower_expr env lhs in
  let (rhs_stmts, rhs_expr) = lower_expr env rhs in
  (lhs_stmts @ rhs_stmts, Ir.Binary (op, lhs_expr, rhs_expr))

and lower_unary env op operand =
  let (ir_stmts, ir_expr) = lower_expr env operand in
  (ir_stmts, Ir.Unary (op, ir_expr))

and lower_app env callee args =
  let (callee_stmts, callee_expr) = lower_expr env callee in
  let (args_stmts, args_exprs) =
    List.fold_left
      (fun (stmts, exprs) arg ->
        let (arg_stmts, arg_expr) = lower_expr env arg in
        (stmts @ arg_stmts, exprs @ [arg_expr]) )
      ([], []) args
  in
  (callee_stmts @ args_stmts, Ir.App (callee_expr, args_exprs))

and lower_lambda env params body =
  let (env', params') = mangle_list env params in
  ([], Ir.Lambda (params', return env' body))

let lower env stmts =
  reset_id ();
  lower_stmts env stmts
