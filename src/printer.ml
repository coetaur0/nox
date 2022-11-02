(* ----- Utility functions ---------------------------------------------------------------------- *)

let rec string_of_list list string_fn separator =
  match list with
  | [value] -> string_fn value
  | value :: rest -> string_fn value ^ separator ^ " " ^ string_of_list rest string_fn separator
  | [] -> ""

(* ----- Span representation functions ---------------------------------------------------------- *)

let string_of_position position =
  let open Source in
  string_of_int position.line ^ ":" ^ string_of_int position.column 

let string_of_span span =
  let open Source in
  string_of_position span.left ^ ".." ^ string_of_position span.right

(* ----- Diagnostic representation functions ---------------------------------------------------- *)

let string_of_diagnostic diagnostic =
  let open Diagnostic in
  string_of_span diagnostic.span ^ ": " ^ diagnostic.message ^ "."

(* ----- Parse tree representation functions ---------------------------------------------------- *)

let string_of_binop op =
  let open Parsetree in
  match op with
  | Or -> "||"
  | And -> "&&"
  | Eq -> "=="
  | Ne -> "!="
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let string_of_unop op =
  let open Parsetree in
  match op with
  | Not -> "!"
  | Neg -> "-"

let rec string_of_stmts stmts =
  string_of_list stmts string_of_stmt ";"

and string_of_stmt stmt =
  let open Parsetree in
  match stmt with
  | Decl decl -> string_of_decl decl
  | Expr expr -> string_of_expr expr

and string_of_decl decl =
  let open Parsetree in
  match decl.kind with
  | Fn (name, params, body) ->
    "fn " ^ name ^ "(" ^ string_of_list params (fun x -> x) "," ^ ") " ^ string_of_expr body
  | Let (name, value) -> "let " ^ name ^ " = " ^ string_of_expr value

and string_of_expr expr =
  let open Parsetree in
  match expr.kind with
  | Binary (op, lhs, rhs) ->
    "(" ^ string_of_expr lhs ^ " " ^ string_of_binop op ^ " " ^ string_of_expr rhs ^ ")"
  | Unary (op, operand) -> string_of_unop op ^ string_of_expr operand
  | Block stmts -> "{" ^ string_of_list stmts string_of_stmt ";" ^ "}"
  | If (cond, thn, els) ->
    let els_string =
      match els with
      | Some expr -> " else " ^ string_of_expr expr
      | None -> ""
    in
    "if " ^ string_of_expr cond ^ " " ^ string_of_expr thn ^ els_string
  | App (callee, args) -> string_of_expr callee ^ "(" ^ string_of_list args string_of_expr "," ^ ")"
  | Lambda (params, body) ->
    "lam(" ^ string_of_list params (fun x -> x) "," ^ ") " ^ string_of_expr body
  | Var x -> x
  | Number num -> string_of_float num
  | Boolean bool -> string_of_bool bool
  | Unit -> "()"
  | Invalid -> "<invalid expression>"
