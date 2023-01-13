(* ----- Interpreter functions ------------------------------------------------------------------ *)

let rec collect_fns env = function
  | stmt :: rest ->
    let (env', stmts) = collect_fns env rest in
    let stmts' =
      match Ast.(stmt.value) with
      | Ast.Fn (name, params, body) ->
        env' := Environment.add name (Values.Closure (env', params, body)) !env';
        stmts
      | _ -> stmt :: stmts
    in
    (env', stmts')
  | [] -> (env, [])

let rec eval_stmts env stmts =
  let (fn_env, stmts') = collect_fns (ref env) stmts in
  let rec eval_stmts' env = function
    | [stmt] -> eval_stmt env stmt
    | stmt :: rest ->
      let (env', _) = eval_stmt env stmt in
      eval_stmts' env' rest
    | [] -> (env, Values.Unit)
  in
  eval_stmts' !fn_env stmts'

and eval_stmt env node =
  match Ast.(node.value) with
  | Ast.Let (name, value) -> (Environment.add name (eval_expr env value) env, Values.Unit)
  | Ast.Expr expr -> (env, eval_expr env Ast.{value = expr; span = node.span})
  | _ -> failwith "Unreachable case"

and eval_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> eval_binary env op lhs rhs
  | Ast.Unary (op, operand) -> eval_unary env op operand
  | Ast.Block stmts -> eval_block env stmts
  | Ast.If (cond, thn, els) -> eval_if env cond thn els
  | Ast.App (callee, args) -> eval_app env callee args
  | Ast.Lambda (params, body) -> Values.Closure (ref env, params, body)
  | Ast.Var x -> Environment.find x env
  | Ast.Number num -> Values.Number num
  | Ast.Boolean bool -> Values.Boolean bool
  | Ast.Unit -> Values.Unit
  | Ast.Invalid -> failwith "Unreachable case"

and eval_binary env op lhs rhs =
  let left_value = eval_expr env lhs in
  let right_value = eval_expr env rhs in
  match (op, left_value, right_value) with
  | (Ast.Or, Values.Boolean b0, Values.Boolean b1) -> Values.Boolean (b0 || b1)
  | (Ast.And, Values.Boolean b0, Values.Boolean b1) -> Values.Boolean (b0 && b1)
  | (Ast.Eq, _, _) -> Values.Boolean (left_value = right_value)
  | (Ast.Ne, _, _) -> Values.Boolean (left_value <> right_value)
  | (Ast.Le, Values.Number n0, Values.Number n1) -> Values.Boolean (n0 <= n1)
  | (Ast.Ge, Values.Number n0, Values.Number n1) -> Values.Boolean (n0 >= n1)
  | (Ast.Lt, Values.Number n0, Values.Number n1) -> Values.Boolean (n0 < n1)
  | (Ast.Gt, Values.Number n0, Values.Number n1) -> Values.Boolean (n0 > n1)
  | (Ast.Add, Values.Number n0, Values.Number n1) -> Values.Number (n0 +. n1)
  | (Ast.Sub, Values.Number n0, Values.Number n1) -> Values.Number (n0 -. n1)
  | (Ast.Mul, Values.Number n0, Values.Number n1) -> Values.Number (n0 *. n1)
  | (Ast.Div, Values.Number n0, Values.Number n1) -> Values.Number (n0 /. n1)
  | _ -> failwith "Unreachable case"

and eval_unary env op operand =
  let operand_value = eval_expr env operand in
  match (op, operand_value) with
  | (Ast.Not, Values.Boolean b) -> Values.Boolean (not b)
  | (Ast.Neg, Values.Number n) -> Values.Number (-.n)
  | _ -> failwith "Unreachable case"

and eval_block env stmts =
  let (_, value) = eval_stmts env stmts in
  value

and eval_if env cond thn els =
  let cond_value = eval_expr env cond in
  match cond_value with
  | Values.Boolean true -> eval_expr env thn
  | Values.Boolean false -> eval_expr env els
  | _ -> failwith "Unreachable case"

and eval_app env callee args =
  let callee_value = eval_expr env callee in
  match callee_value with
  | Values.Closure (closure_env, params, body) ->
    let call_env =
      List.fold_left2
        (fun call_env param arg -> Environment.add param (eval_expr env arg) call_env)
        !closure_env params args
    in
    eval_expr call_env body
  | _ -> failwith "Unreachable case"

let run env stmts = eval_stmts env stmts