(* ----- Type error ----------------------------------------------------------------------------- *)

exception TypeError of Diagnostic.t

(* ----- Typing environment --------------------------------------------------------------------- *)

type env = Types.t Environment.t

let init_env =
  Environment.of_list
    [ ("print", Types.Fun ([Types.String], Types.Unit));
      ("num2str", Types.Fun ([Types.Number], Types.String));
      ("bool2str", Types.Fun ([Types.Boolean], Types.String)) ]

(* ----- Type variables and generalisation levels ----------------------------------------------- *)

let previous_id = ref 0

let current_id = ref 0

let current_level = ref 0

let reset_level () =
  previous_id := 0;
  current_id := 0;
  current_level := 0

let enter_level () =
  previous_id := !current_id;
  incr current_level

let exit_level () =
  current_id := !previous_id;
  decr current_level

let gensym () =
  let id = !current_id in
  incr current_id;
  Printf.sprintf "'%s%s"
    (String.make 1 (Char.chr (97 + (id mod 26))))
    ( if id >= 26 then
        string_of_int (id / 26)
      else
        "" )

let new_var level = Types.Var (ref (Types.Free (gensym (), level)))

(* ----- Generalisation and instantiation functions --------------------------------------------- *)

let rec generalise = function
  | Types.Fun (params, return) -> Types.Fun (List.map generalise params, generalise return)
  | Types.Var {contents = Bound ty} -> generalise ty
  | Types.Var {contents = Free (x, level)} when level > !current_level -> Types.Generic x
  | Types.Ref ty -> Types.Ref (generalise ty)
  | Types.Record row -> Types.Record (generalise row)
  | Types.Variant row -> Types.Variant (generalise row)
  | Types.Row (fields, row) -> Types.Row (Environment.map generalise fields, generalise row)
  | _ as ty -> ty

let instantiate ty =
  let generics = Hashtbl.create 10 in
  let rec instantiate' = function
    | Types.Fun (params, return) -> Types.Fun (List.map instantiate' params, instantiate' return)
    | Types.Generic x -> (
      try Hashtbl.find generics x
      with Not_found ->
        let var = new_var !current_level in
        Hashtbl.add generics x var;
        var )
    | Types.Var {contents = Bound ty} -> instantiate' ty
    | Types.Ref ty -> Types.Ref (instantiate' ty)
    | Types.Record row -> Types.Record (instantiate' row)
    | Types.Variant row -> Types.Variant (instantiate' row)
    | Types.Row (fields, row) -> Types.Row (Environment.map instantiate' fields, instantiate' row)
    | _ as ty -> ty
  in
  instantiate' ty

(* ----- Unification functions ------------------------------------------------------------------ *)

let rec merge_fields = function
  | Types.Var {contents = Bound ty} -> merge_fields ty
  | Types.Var _ as var -> (Environment.empty, var)
  | Types.Record row -> merge_fields row
  | Types.Variant row -> merge_fields row
  | Types.Row (fields, row) -> (
    match merge_fields row with
    | (fields', row') when Environment.is_empty fields' -> (fields, row')
    | (fields', row') -> (Environment.merge fields fields', row') )
  | Types.EmptyRow -> (Environment.empty, Types.EmptyRow)
  | _ -> failwith "expect a row type"

let rec occurs typevar = function
  | Types.Fun (params, return) -> List.exists (occurs typevar) params || occurs typevar return
  | Types.Var {contents = Bound ty} -> occurs typevar ty
  | Types.Var typevar' when typevar == typevar' -> true
  | Types.Var ({contents = Free (x', level')} as typevar') ->
    let min_level =
      match !typevar with
      | Free (_, level) -> min level level'
      | _ -> level'
    in
    typevar' := Free (x', min_level);
    false
  | Types.Ref ty -> occurs typevar ty
  | Types.Record row -> occurs typevar row
  | Types.Variant row -> occurs typevar row
  | Types.Row (fields, row) ->
    Environment.exists (fun _ ty -> occurs typevar ty) fields || occurs typevar row
  | _ -> false

let rec unify span lhs rhs =
  if lhs == rhs then
    ()
  else (
    match (lhs, rhs) with
    | (Types.Fun (lhs_params, lhs_return), Types.Fun (rhs_params, rhs_return)) ->
      List.iter2 (unify span) lhs_params rhs_params;
      unify span lhs_return rhs_return
    | (Types.Var {contents = Bound lhs'}, rhs') | (lhs', Types.Var {contents = Bound rhs'}) ->
      unify span lhs' rhs'
    | (Types.Var ({contents = Free _} as typevar), ty)
     |(ty, Types.Var ({contents = Free _} as typevar)) ->
      if occurs typevar ty then
        raise (TypeError {message = "found recursive types"; span})
      else
        typevar := Bound ty
    | (Types.Ref lhs_ty, Types.Ref rhs_ty) -> unify span lhs_ty rhs_ty
    | (Types.Record lhs_row, Types.Record rhs_row) | (Types.Variant lhs_row, Types.Variant rhs_row)
      ->
      unify span lhs_row rhs_row
    | ((Types.Row _ as lhs_row), (Types.Row _ as rhs_row)) -> unify_rows span lhs_row rhs_row
    | (Types.Row (fields, _), Types.EmptyRow) | (Types.EmptyRow, Types.Row (fields, _)) ->
      let (label, _) = Environment.choose fields in
      raise (TypeError {message = Printf.sprintf "type doesn't contain label '%s'" label; span})
    | (Types.Number, Types.Number)
     |(Types.Boolean, Types.Boolean)
     |(Types.String, Types.String)
     |(Types.EmptyRow, Types.EmptyRow)
     |(Types.Unit, Types.Unit) ->
      ()
    | _ ->
      raise
        (TypeError
           { message =
               Printf.sprintf "expect a value of type %s, but found a %s value"
                 (Printer.type_repr lhs) (Printer.type_repr rhs);
             span } )
  )

and unify_rows span lhs rhs =
  let rec unify_fields lhs_fields rhs_fields lhs_missing rhs_missing =
    match (lhs_fields, rhs_fields) with
    | ((lhs_name, lhs_type) :: lhs_rest, (rhs_name, rhs_type) :: rhs_rest) -> (
      match String.compare lhs_name rhs_name with
      | 0 ->
        unify span lhs_type rhs_type;
        unify_fields lhs_rest rhs_rest lhs_missing rhs_missing
      | x when x < 0 ->
        unify_fields lhs_rest rhs_fields lhs_missing (Environment.add lhs_name lhs_type rhs_missing)
      | _ ->
        unify_fields lhs_fields rhs_rest (Environment.add rhs_name rhs_type lhs_missing) rhs_missing
      )
    | ([], []) -> (lhs_missing, rhs_missing)
    | ([], _) -> (Environment.merge (Environment.of_list rhs_fields) lhs_missing, rhs_missing)
    | (_, []) -> (lhs_missing, Environment.merge (Environment.of_list lhs_fields) rhs_missing)
  in
  let (lhs_fields, lhs_rest) = merge_fields lhs in
  let (rhs_fields, rhs_rest) = merge_fields rhs in
  let (lhs_missing, rhs_missing) =
    unify_fields (Environment.bindings lhs_fields) (Environment.bindings rhs_fields)
      Environment.empty Environment.empty
  in
  match (Environment.is_empty lhs_missing, Environment.is_empty rhs_missing) with
  | (true, true) -> unify span lhs_rest rhs_rest
  | (true, false) -> unify span rhs_rest (Types.Row (rhs_missing, lhs_rest))
  | (false, true) -> unify span lhs_rest (Types.Row (lhs_missing, rhs_rest))
  | (false, false) -> (
    match lhs_rest with
    | Types.Var ({contents = Free _} as var) ->
      let record_var = new_var !current_level in
      unify span rhs_rest (Types.Row (rhs_missing, record_var));
      ( match !var with
      | Bound _ -> raise (TypeError {message = "recursive row types"; span})
      | _ -> () );
      unify span lhs_rest (Types.Row (lhs_missing, record_var))
    | Types.EmptyRow -> unify span lhs_rest (Types.Row (lhs_missing, new_var 0))
    | _ -> assert false )

(* ----- Type inference functions --------------------------------------------------------------- *)

let rec infer_stmts env = function
  | [stmt] -> infer_stmt env stmt
  | stmt :: rest ->
    let (env', _) = infer_stmt env stmt in
    infer_stmts env' rest
  | [] -> (env, Types.Unit)

and infer_stmt env stmt =
  match Ast.(stmt.value) with
  | Ast.Fun funs -> infer_funs env funs
  | Ast.Let (name, value) -> infer_let env name value
  | Ast.Update (lhs, rhs) -> infer_update env lhs rhs
  | Ast.Expr expr -> (env, infer_expr env Ast.{value = expr; span = stmt.span})

and infer_funs env funs =
  enter_level ();
  let env' =
    List.fold_left
      (fun env' (name, params, _) ->
        let param_types = List.map (fun _ -> new_var !current_level) params in
        let ty = Types.Fun (param_types, new_var !current_level) in
        Environment.add name ty env' )
      env funs
  in
  let types =
    List.fold_right
      (fun (name, params, body) types -> infer_fun env' name params body :: types)
      funs []
  in
  exit_level ();
  let types' = List.map generalise types in
  let env'' =
    List.fold_left2 (fun env (name, _, _) ty -> Environment.add name ty env) env funs types'
  in
  (env'', Types.Unit)

and infer_fun env name params body =
  match Environment.find name env with
  | Types.Fun (param_types, return_type) ->
    let env' =
      List.fold_left2
        (fun env param param_type -> Environment.add param param_type env)
        env params param_types
    in
    unify body.span (infer_expr env' body) return_type;
    Types.Fun (param_types, return_type)
  | _ -> raise (TypeError {message = "expect a function"; span = body.span})

and infer_let env name value =
  enter_level ();
  let ty = infer_expr env value in
  exit_level ();
  match value.value with
  | Ast.Lambda _ -> (Environment.add name (generalise ty) env, Types.Unit)
  | _ -> (Environment.add name ty env, Types.Unit)

and infer_update env lhs rhs =
  let lhs_ty = infer_expr env lhs in
  let rhs_ty = infer_expr env rhs in
  unify lhs.span (Types.Ref (new_var !current_level)) lhs_ty;
  unify rhs.span lhs_ty (Types.Ref rhs_ty);
  (env, Types.Unit)

and infer_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> infer_binary env op lhs rhs
  | Ast.Unary (op, operand) -> infer_unary env op operand
  | Ast.Block stmts ->
    let (_, ty) = infer_stmts env stmts in
    ty
  | Ast.If (cond, thn, els) -> infer_if env cond thn els
  | Ast.Match (expr, arms, default) -> infer_match env expr arms default
  | Ast.App (callee, args) -> infer_app env callee args
  | Ast.Record (fields, record) -> infer_record env fields record
  | Ast.Select (path, field) -> infer_select env path field
  | Ast.Variant (case, value) -> infer_variant env case value
  | Ast.Lambda (params, body) -> infer_lambda env params body
  | Ast.Open name -> (
    try Environment.find name env
    with Not_found ->
      raise (TypeError {message = Printf.sprintf "unknown module %s" name; span = node.span}) )
  | Ast.Var x -> (
    try instantiate (Environment.find x env)
    with Not_found ->
      raise (TypeError {message = Printf.sprintf "unknown variable '%s'" x; span = node.span}) )
  | Ast.Number _ -> Types.Number
  | Ast.Boolean _ -> Types.Boolean
  | Ast.String _ -> Types.String
  | Ast.EmptyRecord -> Types.Record Types.EmptyRow
  | Ast.Unit -> Types.Unit
  | Ast.Invalid ->
    raise (TypeError {message = "cannot type an invalid expression"; span = node.span})

and infer_binary env op lhs rhs =
  let lhs_type = infer_expr env lhs in
  let rhs_type = infer_expr env rhs in
  let (operand_type, expr_type) =
    match op with
    | Ast.Or | Ast.And -> (Types.Boolean, Types.Boolean)
    | Ast.Eq | Ast.Ne -> (lhs_type, Types.Boolean)
    | Ast.Le | Ast.Ge | Ast.Lt | Ast.Gt -> (Types.Number, Types.Boolean)
    | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> (Types.Number, Types.Number)
    | Ast.Concat -> (Types.String, Types.String)
  in
  unify Ast.(lhs.span) operand_type lhs_type;
  unify Ast.(rhs.span) operand_type rhs_type;
  expr_type

and infer_unary env op operand =
  let operand_type = infer_expr env operand in
  let span = Ast.(operand.span) in
  match op with
  | Ast.Not ->
    unify span Types.Boolean operand_type;
    Types.Boolean
  | Ast.Neg ->
    unify span Types.Number operand_type;
    Types.Number
  | Ast.Ref -> Types.Ref operand_type
  | Ast.Deref ->
    let ty = new_var !current_level in
    unify span (Types.Ref ty) operand_type;
    ty

and infer_if env cond thn els =
  let cond_type = infer_expr env cond in
  let thn_type = infer_expr env thn in
  let els_type = infer_expr env els in
  unify Ast.(cond.span) Types.Boolean cond_type;
  unify Ast.(thn.span) els_type thn_type;
  thn_type

and infer_match env expr arms default =
  let rec infer_arms env match_type row_type arms =
    match arms with
    | (case, variable, body) :: rest ->
      let case_type = new_var !current_level in
      unify Ast.(body.span) match_type (infer_expr (Environment.add variable case_type env) body);
      let rest_types = infer_arms env match_type row_type rest in
      Types.Row (Environment.singleton case case_type, rest_types)
    | [] -> row_type
  in
  let (match_type, default_type) =
    match default with
    | Some (variable, body) ->
      let default_type = new_var !current_level in
      let match_type =
        infer_expr (Environment.add variable (Types.Variant default_type) env) body
      in
      (match_type, default_type)
    | None -> (new_var !current_level, Types.EmptyRow)
  in
  let expr_type = infer_expr env expr in
  let row_type = infer_arms env match_type default_type arms in
  let span =
    match expr.value with
    | Ast.Variant (_, value) -> value.span
    | _ -> expr.span
  in
  unify span (Types.Variant row_type) expr_type;
  match_type

and infer_app env callee args =
  let callee_type = infer_expr env callee in
  let n_args = List.length args in
  let rec match_args = function
    | Types.Fun (params, return) ->
      let n_params = List.length params in
      if n_args <> n_params then
        raise
          (TypeError
             { message = Printf.sprintf "expect %d arguments, but %d were found" n_params n_args;
               span = Ast.(callee.span) } )
      else
        (params, return)
    | Types.Var {contents = Bound ty} -> match_args ty
    | Types.Var ({contents = Free (_, level)} as typevar) ->
      let params = List.map (fun _ -> new_var level) (List.init n_args (fun _ -> ())) in
      let return = new_var level in
      typevar := Types.Bound (Types.Fun (params, return));
      (params, return)
    | _ -> raise (TypeError {message = "expect a function"; span = callee.span})
  in
  let (param_types, return_type) = match_args callee_type in
  List.iter2
    (fun param_type arg -> unify Ast.(arg.span) param_type (infer_expr env arg))
    param_types args;
  return_type

and infer_record env fields record =
  let row_type = new_var !current_level in
  unify record.span (Types.Record row_type) (infer_expr env record);
  let fields_types = Environment.map (infer_expr env) fields in
  let (fields_types', row_type') = merge_fields (Types.Row (fields_types, row_type)) in
  Types.Record (Types.Row (fields_types', row_type'))

and infer_select env path field =
  let row_type = new_var !current_level in
  let field_type = new_var !current_level in
  unify field.span
    (Types.Record (Types.Row (Environment.singleton field.value field_type, row_type)))
    (infer_expr env path);
  field_type

and infer_variant env case value =
  let row_type = new_var !current_level in
  let case_type = new_var !current_level in
  let variant_type = Types.Variant (Types.Row (Environment.singleton case case_type, row_type)) in
  unify value.span case_type (infer_expr env value);
  variant_type

and infer_lambda env params body =
  let param_types = List.map (fun _ -> new_var !current_level) params in
  let env' =
    List.fold_left2
      (fun env param param_type -> Environment.add param param_type env)
      env params param_types
  in
  let return_type = infer_expr env' body in
  Types.Fun (param_types, return_type)

let infer env stmts =
  reset_level ();
  infer_stmts env stmts
