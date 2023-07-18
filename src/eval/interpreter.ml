(* ----- Intitial runtime environment ----------------------------------------------------------- *)

let init_env =
  Environment.of_list
    [ ( "print",
        Values.NativeFun
          (function
          | Values.String string :: _ ->
            print_endline string;
            Values.Unit
          | _ -> failwith "Invalid arguments" ) );
      ( "num2str",
        NativeFun
          (function
          | Values.Number num :: _ -> Values.String (string_of_float num)
          | _ -> failwith "Invalid arguments" ) );
      ( "bool2str",
        NativeFun
          (function
          | Values.Boolean bool :: _ -> Values.String (string_of_bool bool)
          | _ -> failwith "Invalid arguments" ) ) ]

(* ----- Interpreter functions ------------------------------------------------------------------ *)

let rec eval_stmts env = function
  | [stmt] -> eval_stmt env stmt
  | stmt :: rest ->
    let (env', _) = eval_stmt env stmt in
    eval_stmts env' rest
  | [] -> (env, Values.Unit)

and eval_stmt env node =
  match Ast.(node.value) with
  | Ast.Fun funs -> eval_funs env funs
  | Ast.Let (name, value) -> (Environment.add name (eval_expr env value) env, Values.Unit)
  | Ast.Update (lhs, rhs) -> eval_update env lhs rhs
  | Ast.While (cond, body) -> eval_while env cond body
  | Ast.Expr expr -> (env, eval_expr env Ast.{value = expr; span = node.span})

and eval_funs env funs =
  let env' = ref env in
  List.iter
    (fun (name, params, body) ->
      env' := Environment.add name (Values.Closure (env', params, body)) !env' )
    funs;
  (!env', Values.Unit)

and eval_update env lhs rhs =
  match eval_expr env lhs with
  | Values.Ref value ->
    value := eval_expr env rhs;
    (env, Values.Unit)
  | _ -> failwith "Cannot assign to a non-reference value"

and eval_while env cond body =
  while eval_expr env cond = Values.Boolean true do
    ignore (eval_expr env body)
  done;
  (env, Values.Unit)

and eval_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> eval_binary env op lhs rhs
  | Ast.Unary (op, operand) -> eval_unary env op operand
  | Ast.Block stmts -> eval_block env stmts
  | Ast.If (cond, thn, els) -> eval_if env cond thn els
  | Ast.Match (expr, arms, default) -> eval_match env expr arms default
  | Ast.App (callee, args) -> eval_app env callee args
  | Ast.Record (fields, record) -> (
    let fields' = Environment.map (eval_expr env) fields in
    match eval_expr env record with
    | Values.Record record' -> Values.Record (Environment.merge fields' record')
    | _ -> failwith "expect a record" )
  | Ast.Select (expr, field) -> (
    match eval_expr env expr with
    | Values.Record record -> Environment.find field.value record
    | _ -> failwith "expect a record" )
  | Ast.Variant (case, value) -> Values.Variant (case, eval_expr env value)
  | Ast.Array elements -> Values.Array (Array.of_list (List.map (eval_expr env) elements))
  | Ast.Index (array, index) -> (
    match (eval_expr env array, eval_expr env index) with
    | (Values.Array elements, Values.Number n) ->
      if Float.is_integer n then
        elements.(Int.of_float n)
      else
        failwith "Invalid array index"
    | _ -> failwith "Invalid array index" )
  | Ast.Lambda (params, body) -> Values.Closure (ref env, params, body)
  | Ast.Open name -> Environment.find name env
  | Ast.Var x -> Environment.find x env
  | Ast.Number num -> Values.Number num
  | Ast.Boolean bool -> Values.Boolean bool
  | Ast.String string -> Values.String string
  | Ast.EmptyRecord -> Values.Record Environment.empty
  | Ast.Unit -> Values.Unit
  | Ast.Invalid -> failwith "Invalid expression"

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
  | (Ast.Concat, Values.String s0, Values.String s1) -> Values.String (s0 ^ s1)
  | _ -> failwith "Invalid operands"

and eval_unary env op operand =
  let operand_value = eval_expr env operand in
  match (op, operand_value) with
  | (Ast.Not, Values.Boolean b) -> Values.Boolean (not b)
  | (Ast.Neg, Values.Number n) -> Values.Number (-.n)
  | (Ast.Ref, value) -> Values.Ref (ref value)
  | (Ast.Deref, Values.Ref r) -> !r
  | _ -> failwith "Invalid operand"

and eval_block env stmts =
  let (_, value) = eval_stmts env stmts in
  value

and eval_if env cond thn els =
  let cond_value = eval_expr env cond in
  match cond_value with
  | Values.Boolean true -> eval_expr env thn
  | Values.Boolean false -> eval_expr env els
  | _ -> failwith "Invalid condition"

and eval_match env expr arms default =
  let (case, value) =
    match eval_expr env expr with
    | Values.Variant (case, value) -> (case, value)
    | _ -> failwith "Invalid match expression"
  in
  match List.find_opt (fun (arm_case, _, _) -> case = arm_case) arms with
  | Some (_, variable, body) -> eval_expr (Environment.add variable value env) body
  | None -> (
    match default with
    | Some (variable, body) ->
      eval_expr (Environment.add variable (Values.Variant (case, value)) env) body
    | None -> failwith "No case matching the input expression" )

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
  | Values.NativeFun f -> f (List.map (eval_expr env) args)
  | _ -> failwith "Invalid callee"

let run env stmts = eval_stmts env stmts
