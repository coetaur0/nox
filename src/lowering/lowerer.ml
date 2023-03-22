(* ----- Lowering environment ------------------------------------------------------------------- *)

type env = string Environment.t

let init_env =
  Environment.of_list [("print", "print"); ("num2str", "tostring"); ("bool2str", "tostring")]

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

let rec merge_fields fields record =
  match Ast.(record.value) with
  | Ast.Record (fields', record') ->
    let (fields'', record'') = merge_fields fields' record' in
    (Environment.merge fields fields'', record'')
  | _ -> (fields, record)

(* ----- Lowering functions --------------------------------------------------------------------- *)

let rec lower_stmts env = function
  | [Ast.{value = Expr expr; span}] -> return env Ast.{value = expr; span}
  | stmt :: rest ->
    let (env', ir_stmts) = lower_stmt env stmt in
    ir_stmts @ lower_stmts env' rest
  | [] -> [Ir.Return Ir.Unit]

and lower_stmt env node =
  match Ast.(node.value) with
  | Ast.Fun funs -> lower_funs env funs
  | Ast.Let (name, value) -> lower_let env name value
  | Ast.Update (lhs, rhs) -> lower_update env lhs rhs
  | Ast.Expr expr -> (env, assign (Ir.Var "_") env Ast.{value = expr; span = node.span})

and lower_funs env funs =
  let env' =
    List.fold_left (fun env' (name, _, _) -> Environment.add name (gensym name) env') env funs
  in
  let ir_funs = List.map (fun (name, params, body) -> lower_fun env' name params body) funs in
  (env', [Ir.Fun ir_funs])

and lower_fun env name params body =
  let (env', params') = mangle_list env params in
  (Environment.find name env, params', return env' body)

and return env node =
  match Ast.(node.value) with
  | Ast.Block stmts -> lower_stmts env stmts
  | Ast.If (cond, thn, els) -> lower_if env cond thn els return
  | Ast.Match (expr, arms, default) -> lower_match env expr arms default return
  | _ ->
    let (ir_stmts, ir_expr) = lower_expr env node in
    ir_stmts @ [Ir.Return ir_expr]

and lower_let env name value =
  let (env', name') = mangle env name in
  (env', [Ir.Decl name'] @ assign (Ir.Var name') env' value)

and assign lhs env rhs =
  match Ast.(rhs.value) with
  | Ast.Block stmts ->
    let (block_stmts, block_expr) = lower_block env stmts in
    block_stmts @ [Ir.Assign (lhs, block_expr)]
  | Ast.If (cond, thn, els) -> lower_if env cond thn els (assign lhs)
  | Ast.Match (expr, arms, default) -> lower_match env expr arms default (assign lhs)
  | _ ->
    let (rhs_stmts, rhs_expr) = lower_expr env rhs in
    rhs_stmts @ [Ir.Assign (lhs, rhs_expr)]

and lower_update env lhs rhs =
  let (lhs_stmts, lhs_expr) = lower_expr env lhs in
  let (rhs_stmts, rhs_expr) = lower_expr env rhs in
  (env, lhs_stmts @ rhs_stmts @ [Ir.Assign (Ir.Unary (Ast.Deref, lhs_expr), rhs_expr)])

and lower_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> lower_binary env op lhs rhs
  | Ast.Unary (op, operand) -> lower_unary env op operand
  | Ast.Block _ | Ast.If _ | Ast.Match _ ->
    let tmp = gensym "tmp" in
    let ir_stmts = assign (Ir.Var tmp) env node in
    ([Ir.Decl tmp] @ ir_stmts, Ir.Var tmp)
  | Ast.App (callee, args) -> lower_app env callee args
  | Ast.Record (fields, record) -> lower_record env fields record
  | Ast.Select (record, field) ->
    let (record_stmts, record_expr) = lower_expr env record in
    (record_stmts, Ir.Select (record_expr, field.value))
  | Ast.Variant (case, value) ->
    let case_field = Ast.{value = String case; span = node.span} in
    lower_record env
      (Environment.of_list [("case", case_field); ("value", value)])
      Ast.{value = EmptyRecord; span = node.span}
  | Ast.Lambda (params, body) -> lower_lambda env params body
  | Ast.Var x -> ([], Ir.Var (Environment.find x env))
  | Ast.Number num -> ([], Ir.Number num)
  | Ast.Boolean bool -> ([], Ir.Boolean bool)
  | Ast.String string -> ([], Ir.String string)
  | Ast.Unit -> ([], Ir.Unit)
  | Ast.EmptyRecord ->
    let tmp = gensym "tmp" in
    ([Ir.Decl tmp; Ir.Assign (Ir.Var tmp, Ir.EmptyRecord)], Ir.Var tmp)
  | Ast.Invalid -> failwith "Unreachable case"

and lower_binary env op lhs rhs =
  let (lhs_stmts, lhs_expr) = lower_expr env lhs in
  let (rhs_stmts, rhs_expr) = lower_expr env rhs in
  (lhs_stmts @ rhs_stmts, Ir.Binary (op, lhs_expr, rhs_expr))

and lower_unary env op operand =
  let (ir_stmts, ir_expr) = lower_expr env operand in
  (ir_stmts, Ir.Unary (op, ir_expr))

and lower_block env = function
  | [Ast.{value = Expr expr; span}] -> lower_expr env Ast.{value = expr; span}
  | stmt :: rest ->
    let (env', ir_stmts) = lower_stmt env stmt in
    let (rest_stmts, rest_expr) = lower_block env' rest in
    (ir_stmts @ rest_stmts, rest_expr)
  | [] -> ([], Ir.Unit)

and lower_if env cond thn els fn =
  let (cond_stmts, cond_expr) = lower_expr env cond in
  cond_stmts @ [Ir.If (cond_expr, fn env thn, fn env els)]

and lower_match env expr arms default fn =
  let (expr_stmts, expr_expr) = lower_expr env expr in
  let case_expr = Ir.Select (expr_expr, "case") in
  let value_expr = Ir.Select (expr_expr, "value") in
  let default_stmts =
    match default with
    | Some (variable, body) ->
      let (env', variable') = mangle env variable in
      [Ir.Decl variable; Ir.Assign (Ir.Var variable', value_expr)] @ fn env' body
    | None -> []
  in
  let rec lower_arms = function
    | (case, variable, body) :: rest ->
      let (env', variable') = mangle env variable in
      [ Ir.If
          ( Ir.Binary (Ast.Eq, case_expr, Ir.String case),
            [Ir.Decl variable'; Ir.Assign (Ir.Var variable', value_expr)] @ fn env' body,
            lower_arms rest ) ]
    | [] -> default_stmts
  in
  expr_stmts @ lower_arms arms

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

and lower_record env fields record =
  let (fields', record') = merge_fields fields record in
  let (record_stmts, record_expr) = lower_expr env record' in
  let (copy_stmt, record_copy) =
    match Ast.(record'.value) with
    | Ast.Select _ | Ast.Var _ ->
      let copy_name = gensym "tmp" in
      ([Ir.CopyRecord (copy_name, record_expr)], Ir.Var copy_name)
    | _ -> ([], record_expr)
  in
  let fields_stmts =
    Environment.fold
      (fun name value stmts ->
        let (field_stmts, field_expr) = lower_expr env value in
        stmts @ field_stmts @ [Ir.Assign (Ir.Select (record_copy, name), field_expr)] )
      fields' []
  in
  (record_stmts @ copy_stmt @ fields_stmts, record_copy)

and lower_lambda env params body =
  let (env', params') = mangle_list env params in
  ([], Ir.Lambda (params', return env' body))

let lower env stmts =
  reset_id ();
  lower_stmts env stmts
