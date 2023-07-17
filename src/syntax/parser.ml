(* ----- Syntax error --------------------------------------------------------------------------- *)

exception SyntaxError of Diagnostic.t list

(* ----- Parser --------------------------------------------------------------------------------- *)

type t =
  { source : Source.t;
    lexer : Lexer.t;
    mutable token : Token.t;
    mutable next_tokens : Token.t * Token.t;
    mutable diagnostics : Diagnostic.t list;
    mutable panic : bool }

(* ----- Utility functions ---------------------------------------------------------------------- *)

let advance parser =
  let token = parser.token in
  parser.token <- fst parser.next_tokens;
  parser.next_tokens <- (snd parser.next_tokens, Lexer.next parser.lexer);
  token

let emit_diagnostic parser message span =
  if parser.panic then
    ()
  else (
    parser.panic <- true;
    parser.diagnostics <- Diagnostic.{message; span} :: parser.diagnostics
  )

let consume parser lexeme message =
  if parser.token.kind = lexeme then
    Some (advance parser)
  else (
    emit_diagnostic parser message parser.token.span;
    None
  )

let synchronize parser lexemes =
  let rec synchronize' () =
    match parser.token.kind with
    | l when List.exists (fun lexeme -> l = lexeme) lexemes -> parser.panic <- false
    | Token.Eof -> parser.panic <- false
    | _ ->
      ignore (advance parser);
      synchronize' ()
  in
  if parser.panic then synchronize' ()

(* ----- Parsing functions ---------------------------------------------------------------------- *)

let parse_list parser parse_func separator delimiters =
  let sync_list = separator :: delimiters in
  let rec parse_list' result =
    if List.exists (fun elt -> elt = parser.token.kind) delimiters then
      result
    else (
      let result' = result @ [parse_func parser] in
      synchronize parser sync_list;
      if parser.token.kind = separator then (
        ignore (advance parser);
        parse_list' result'
      ) else
        result'
    )
  in
  parse_list' []

let parse_name message parser =
  match consume parser Token.Name message with
  | Some token -> Source.read parser.source token.span
  | None -> ""

let rec parse_stmts parser = parse_list parser parse_stmt Token.Semicolon [Token.Eof]

and parse_stmt parser =
  match parser.token.kind with
  | Token.Fun -> parse_fun parser
  | Token.Let -> parse_let parser
  | Token.While -> parse_while parser
  | _ ->
    let lhs = parse_unary parser in
    if parser.token.kind = Token.Update then (
      ignore (advance parser);
      let rhs = parse_expr parser in
      Ast.{value = Update (lhs, rhs); span = Source.merge lhs.span rhs.span}
    ) else (
      let node = parse_binary parser 0 lhs in
      Ast.{value = Expr node.value; span = node.span}
    )

and parse_fun parser =
  let start = parser.token.span in
  let (funs, span_end) = parse_funs parser in
  Ast.{value = Fun funs; span = Source.merge start span_end}

and parse_funs parser =
  ignore (advance parser);
  let name = parse_name "expect a function name" parser in
  ignore (consume parser Token.LParen "expect a '('");
  let params =
    parse_list parser (parse_name "expect a parameter name") Token.Comma [Token.RParen]
  in
  synchronize parser [Token.RParen; Token.LBrace];
  ignore (consume parser Token.RParen "expect a ')'");
  synchronize parser [Token.LBrace];
  let body = parse_block parser in
  if parser.token.kind = Token.Fun then (
    let (rest, span_end) = parse_funs parser in
    ((name, params, body) :: rest, span_end)
  ) else
    ([(name, params, body)], body.span)

and parse_let parser =
  let start = (advance parser).span in
  let name = parse_name "expect a variable name" parser in
  ignore (consume parser Token.Assign "expect a '='");
  let body = parse_expr parser in
  Ast.{value = Let (name, body); span = Source.merge start body.span}

and parse_while parser =
  let start = (advance parser).span in
  let cond = parse_expr parser in
  let body = parse_block parser in
  Ast.{value = While (cond, body); span = Source.merge start body.span}

and parse_expr parser = parse_binary parser 0 (parse_unary parser)

and parse_binary parser precedence lhs =
  let next_op () =
    match parser.token.kind with
    | Token.Or -> (Ast.Or, 1)
    | Token.And -> (Ast.And, 2)
    | Token.Eq -> (Ast.Eq, 3)
    | Token.Ne -> (Ast.Ne, 3)
    | Token.Le -> (Ast.Le, 4)
    | Token.Ge -> (Ast.Ge, 4)
    | Token.Lt -> (Ast.Lt, 4)
    | Token.Gt -> (Ast.Gt, 4)
    | Token.Add -> (Ast.Add, 5)
    | Token.Sub -> (Ast.Sub, 5)
    | Token.Mul -> (Ast.Mul, 6)
    | Token.Div -> (Ast.Div, 6)
    | Token.Concat -> (Ast.Concat, 7)
    | _ -> (Ast.Or, 0)
  in
  let (op, next_precedence) = next_op () in
  if precedence < next_precedence then (
    ignore (advance parser);
    let rhs = parse_binary parser next_precedence (parse_unary parser) in
    let expr = Ast.{value = Binary (op, lhs, rhs); span = Source.merge lhs.span rhs.span} in
    parse_binary parser precedence expr
  ) else
    lhs

and parse_unary parser =
  let unop =
    match parser.token.kind with
    | Token.Not -> Some Ast.Not
    | Token.Sub -> Some Ast.Neg
    | Token.Ref -> Some Ast.Ref
    | Token.Deref -> Some Ast.Deref
    | _ -> None
  in
  match unop with
  | Some op ->
    let start = (advance parser).span in
    let operand = parse_unary parser in
    Ast.{value = Unary (op, operand); span = Source.merge start operand.span}
  | None -> parse_path parser

and parse_path parser =
  let rec parse_path' path =
    match parser.token.kind with
    | Token.LParen ->
      ignore (advance parser);
      let args = parse_list parser parse_expr Token.Comma [Token.RParen] in
      let span_end =
        match consume parser Token.RParen "expect a ')'" with
        | Some token -> token.span
        | None -> Ast.(path.span)
      in
      parse_path' Ast.{value = App (path, args); span = Source.merge path.span span_end}
    | Token.Dot -> (
      ignore (advance parser);
      match consume parser Token.Name "expect a field name" with
      | Some token ->
        let field = Ast.{value = Source.read parser.source token.span; span = token.span} in
        parse_path' Ast.{value = Select (path, field); span = Source.merge path.span field.span}
      | None -> path )
    | _ -> path
  in
  let path = parse_primary parser in
  parse_path' path

and parse_primary parser =
  match parser.token.kind with
  | Token.LBrace -> parse_brace parser
  | Token.If -> parse_if parser
  | Token.Match -> parse_match parser
  | Token.Lt -> parse_lambda parser
  | Token.Case -> parse_variant parser
  | Token.Open -> parse_open parser
  | Token.Name -> parse_var parser
  | Token.Number -> parse_number parser
  | Token.Boolean -> parse_boolean parser
  | Token.String -> parse_string parser
  | Token.LParen -> parse_paren parser
  | _ ->
    emit_diagnostic parser "expect an expression" parser.token.span;
    Ast.{value = Invalid; span = parser.token.span}

and parse_brace parser =
  match ((fst parser.next_tokens).kind, (snd parser.next_tokens).kind) with
  | (Token.Name, Token.Assign) | (Token.Pipe, _) | (Token.RBrace, _) -> parse_record parser
  | _ -> parse_block parser

and parse_block parser =
  let left = parser.token.span.left in
  ignore (consume parser Token.LBrace "expect a '{'");
  let stmts = parse_list parser parse_stmt Token.Semicolon [Token.RBrace] in
  let right =
    match consume parser Token.RBrace "expect a '}'" with
    | Some token -> token.span.right
    | None -> parser.token.span.left
  in
  Ast.{value = Block stmts; span = Source.{left; right}}

and parse_record parser =
  let parse_field parser =
    match consume parser Token.Name "expect a field name" with
    | Some name ->
      ignore (consume parser Token.Assign "expect a '='");
      let value = parse_expr parser in
      (Source.read parser.source name.span, value)
    | None -> ("", Ast.{value = Invalid; span = parser.token.span})
  in
  let left = (advance parser).span.left in
  let fields =
    Environment.of_list (parse_list parser parse_field Token.Comma [Token.Pipe; Token.RBrace])
  in
  let record =
    if parser.token.kind = Token.Pipe then (
      ignore (advance parser);
      parse_expr parser
    ) else
      Ast.{value = EmptyRecord; span = parser.token.span}
  in
  let right =
    match consume parser Token.RBrace "expect a '}'" with
    | Some token -> token.span.right
    | None -> parser.token.span.left
  in
  if Environment.is_empty fields then
    record
  else
    Ast.{value = Record (fields, record); span = Source.{left; right}}

and parse_if parser =
  let start = (advance parser).span in
  let cond = parse_expr parser in
  synchronize parser [Token.LBrace];
  let thn = parse_block parser in
  let els =
    if parser.token.kind = Token.Else then (
      ignore (advance parser);
      if parser.token.kind = Token.If then
        parse_if parser
      else
        parse_block parser
    ) else
      Ast.{value = Block []; span = Source.{left = thn.span.right; right = thn.span.right}}
  in
  Ast.{value = If (cond, thn, els); span = Source.merge start els.span}

and parse_match parser =
  let parse_arm parser =
    let case =
      match consume parser Token.Case "expect a variant case name" with
      | Some case -> Source.read parser.source case.span
      | None -> ""
    in
    let variable =
      match consume parser Token.Name "expect a variable name" with
      | Some name -> Source.read parser.source name.span
      | None -> ""
    in
    ignore (consume parser Token.Arrow "expect a '=>'");
    let body = parse_expr parser in
    (case, variable, body)
  in
  let start = (advance parser).span in
  let expr = parse_expr parser in
  ignore (consume parser Token.LBrace "expect a '{'");
  let arms = parse_list parser parse_arm Token.Comma [Token.Name; Token.RBrace] in
  let default =
    match parser.token.kind with
    | Token.Name ->
      let variable = Source.read parser.source (advance parser).span in
      ignore (consume parser Token.Arrow "expect a '=>'");
      let body = parse_expr parser in
      Some (variable, body)
    | _ -> None
  in
  let span_end =
    match consume parser Token.RBrace "expect a '}'" with
    | Some token -> token.span
    | None -> parser.token.span
  in
  Ast.{value = Match (expr, arms, default); span = Source.merge start span_end}

and parse_lambda parser =
  let start = (advance parser).span in
  let params = parse_list parser (parse_name "expect a parameter name") Token.Comma [Token.Gt] in
  synchronize parser [Token.Gt; Token.LBrace];
  ignore (consume parser Token.Gt "expect a '>'");
  let body = parse_block parser in
  Ast.{value = Lambda (params, body); span = Source.merge start body.span}

and parse_variant parser =
  let start = (advance parser).span in
  let case = Source.read parser.source start in
  let value = parse_expr parser in
  Ast.{value = Variant (case, value); span = Source.merge start value.span}

and parse_open parser =
  ignore (advance parser);
  let (name, span) =
    match consume parser Token.String "expect a module name between \"" with
    | Some token -> (Source.read parser.source token.span, token.span)
    | None -> ("", parser.token.span)
  in
  Ast.{value = Open name; span}

and parse_var parser =
  let span = (advance parser).span in
  let x = Source.read parser.source span in
  Ast.{value = Var x; span}

and parse_number parser =
  let span = (advance parser).span in
  let num = float_of_string (Source.read parser.source span) in
  Ast.{value = Number num; span}

and parse_boolean parser =
  let span = (advance parser).span in
  let bool = bool_of_string (Source.read parser.source span) in
  Ast.{value = Boolean bool; span}

and parse_string parser =
  let span = (advance parser).span in
  let string = Source.read parser.source span in
  let value = String.sub string 1 (String.length string - 2) in
  Ast.{value = String value; span}

and parse_paren parser =
  let start = (advance parser).span in
  if parser.token.kind = Token.RParen then
    Ast.{value = Unit; span = Source.merge start (advance parser).span}
  else (
    let expr = parse_expr parser in
    synchronize parser [Token.RParen];
    ignore (consume parser Token.RParen "expect a ')'");
    expr
  )

let parse source =
  let lexer = Lexer.make source in
  let token = Lexer.next lexer in
  let next_token = Lexer.next lexer in
  let parser =
    { source;
      lexer;
      token;
      next_tokens = (next_token, Lexer.next lexer);
      diagnostics = [];
      panic = false }
  in
  let stmts = parse_stmts parser in
  ignore (consume parser Token.Eof "expect the end of file");
  if parser.diagnostics = [] then
    stmts
  else
    raise (SyntaxError (List.rev parser.diagnostics))
