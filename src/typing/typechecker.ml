(* ----- Type error ----------------------------------------------------------------------------- *)

exception TypeError of Diagnostic.t

(* ----- Typing environment --------------------------------------------------------------------- *)

module Env = Map.Make (String)

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
  "'"
  ^ String.make 1 (Char.chr (97 + (id mod 26)))
  ^
  if id >= 26 then
    string_of_int (id / 26)
  else
    ""

let new_var level = Types.Var (ref (Types.Free (gensym (), level)))

(* ----- Generalisation and instantiation functions --------------------------------------------- *)

let rec generalise = function
  | Types.Fn (params, return) -> Types.Fn (List.map generalise params, generalise return)
  | Types.Var {contents = Bound ty} -> generalise ty
  | Types.Var {contents = Free (x, level)} when level > !current_level -> Types.Generic x
  | _ as ty -> ty

let instantiate ty =
  let generics = Hashtbl.create 10 in
  let rec instantiate' = function
    | Types.Fn (params, return) -> Types.Fn (List.map instantiate' params, instantiate' return)
    | Types.Generic x -> (
      try Hashtbl.find generics x
      with Not_found ->
        let var = new_var !current_level in
        Hashtbl.add generics x var;
        var )
    | Types.Var {contents = Bound ty} -> instantiate' ty
    | _ as ty -> ty
  in
  instantiate' ty

(* ----- Unification functions ------------------------------------------------------------------ *)

let rec occurs typevar = function
  | Types.Fn (params, return) -> List.exists (occurs typevar) params || occurs typevar return
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
  | _ -> false

let rec unify span lhs rhs =
  if lhs == rhs then
    ()
  else (
    match (lhs, rhs) with
    | (Types.Fn (lhs_params, lhs_return), Types.Fn (rhs_params, rhs_return)) ->
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
    | (Types.Number, Types.Number) | (Types.Boolean, Types.Boolean) | (Types.Unit, Types.Unit) -> ()
    | _ ->
      raise
        (TypeError
           { message =
               "expect a value of type " ^ Printer.type_repr lhs ^ ", but found a "
               ^ Printer.type_repr rhs ^ " value";
             span } )
  )

(* ----- Type inference functions --------------------------------------------------------------- *)

let rec collect_fn_types env = function
  | stmt :: rest -> (
    let env' = collect_fn_types env rest in
    match Ast.(stmt.value) with
    | Ast.Fn (name, params, _) ->
      let param_types = List.map (fun _ -> new_var !current_level) params in
      let ty = Types.Fn (param_types, new_var !current_level) in
      Env.add name ty env'
    | _ -> env' )
  | [] -> env

let rec infer_stmts env stmts =
  enter_level ();
  let env' = collect_fn_types env stmts in
  exit_level ();
  let rec infer_stmts' env = function
    | [stmt] ->
      let (ty, _) = infer_stmt env stmt in
      ty
    | stmt :: rest ->
      let (_, env') = infer_stmt env stmt in
      infer_stmts' env' rest
    | [] -> Types.Unit
  in
  infer_stmts' env' stmts

and infer_stmt env stmt =
  match Ast.(stmt.value) with
  | Ast.Fn (name, params, body) -> infer_fn env name params body
  | Ast.Let (name, value) -> infer_let env name value
  | Ast.Expr expr -> (infer_expr env Ast.{value = expr; span = stmt.span}, env)

and infer_fn env name params body =
  enter_level ();
  let ty =
    match Env.find name env with
    | Types.Fn (param_types, _) ->
      let env' =
        List.fold_left2
          (fun env param param_type -> Env.add param param_type env)
          env params param_types
      in
      let return_type = infer_expr env' body in
      Types.Fn (param_types, return_type)
    | _ -> raise (TypeError {message = "expect a function"; span = body.span})
  in
  exit_level ();
  let ty' = generalise ty in
  (Types.Unit, Env.add name ty' env)

and infer_let env name value =
  enter_level ();
  let ty = infer_expr env value in
  exit_level ();
  (Types.Unit, Env.add name (generalise ty) env)

and infer_expr env node =
  match Ast.(node.value) with
  | Ast.Binary (op, lhs, rhs) -> infer_binary env op lhs rhs
  | Ast.Unary (op, operand) -> infer_unary env op operand
  | Ast.Block stmts -> infer_stmts env stmts
  | Ast.If (cond, thn, els) -> infer_if env cond thn els
  | Ast.App (callee, args) -> infer_app env callee args
  | Ast.Lambda (params, body) -> infer_lambda env params body
  | Ast.Var x -> (
    try instantiate (Env.find x env)
    with Not_found ->
      raise (TypeError {message = "unknown variable '" ^ x ^ "'"; span = node.span}) )
  | Ast.Number _ -> Types.Number
  | Ast.Boolean _ -> Types.Boolean
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
  in
  unify Ast.(lhs.span) operand_type lhs_type;
  unify Ast.(rhs.span) operand_type rhs_type;
  expr_type

and infer_unary env op operand =
  let operand_type = infer_expr env operand in
  let expr_type =
    match op with
    | Ast.Not -> Types.Boolean
    | Ast.Neg -> Types.Number
  in
  unify Ast.(operand.span) expr_type operand_type;
  expr_type

and infer_if env cond thn els =
  let cond_type = infer_expr env cond in
  let thn_type = infer_expr env thn in
  let els_type = infer_expr env els in
  unify Ast.(cond.span) Types.Boolean cond_type;
  unify Ast.(thn.span) els_type thn_type;
  thn_type

and infer_app env callee args =
  let callee_type = infer_expr env callee in
  let n_args = List.length args in
  let rec match_args = function
    | Types.Fn (params, return) ->
      let n_params = List.length params in
      if n_args <> n_params then
        raise
          (TypeError
             { message =
                 "expect " ^ string_of_int n_params ^ " arguments, but " ^ string_of_int n_args
                 ^ " were found";
               span = Ast.(callee.span) } )
      else
        (params, return)
    | Types.Var {contents = Bound ty} -> match_args ty
    | Types.Var ({contents = Free (_, level)} as typevar) ->
      let params = List.map (fun _ -> new_var level) (List.init n_args (fun _ -> ())) in
      let return = new_var level in
      typevar := Types.Bound (Types.Fn (params, return));
      (params, return)
    | _ -> raise (TypeError {message = "expect a function"; span = callee.span})
  in
  let (param_types, return_type) = match_args callee_type in
  List.iter2
    (fun param_type arg -> unify Ast.(arg.span) param_type (infer_expr env arg))
    param_types args;
  return_type

and infer_lambda env params body =
  let param_types = List.map (fun _ -> new_var !current_level) params in
  let env' =
    List.fold_left2
      (fun env param param_type -> Env.add param param_type env)
      env params param_types
  in
  let return_type = infer_expr env' body in
  Types.Fn (param_types, return_type)

let infer stmts =
  reset_level ();
  infer_stmts Env.empty stmts
