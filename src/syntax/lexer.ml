(* ----- Lexer ---------------------------------------------------------------------------------- *)

type t =
  { source : Source.t;
    mutable start : Source.position;
    mutable current : Source.position }

let make source =
  let start = Source.{line = 1; column = 1; offset = 0} in
  {source; start; current = start}

(* ----- Utility functions ---------------------------------------------------------------------- *)

let peek lexer offset = Source.at lexer.source (lexer.current.offset + offset)

let rec advance lexer n =
  if n <= 0 || lexer.current.offset >= Source.length lexer.source then
    ()
  else (
    let offset = lexer.current.offset + 1 in
    let (line, column) =
      if peek lexer 0 = Some '\n' then
        (lexer.current.line + 1, 1)
      else
        (lexer.current.line, lexer.current.column + 1)
    in
    lexer.current <- Source.{line; column; offset};
    advance lexer (n - 1)
  )

let consume lexer predicate =
  let rec consume' _ =
    match peek lexer 0 with
    | Some c when predicate c ->
      advance lexer 1;
      consume' ()
    | _ -> ()
  in
  let left = lexer.current in
  consume' ();
  Source.read lexer.source Source.{left; right = lexer.current}

let make_token lexer kind = Token.{kind; span = Source.{left = lexer.start; right = lexer.current}}

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alnum c = is_alpha c || is_digit c

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_comment lexer =
  advance lexer 2;
  ignore (consume lexer (fun c -> c <> '\n'));
  advance lexer 1

(* ----- Lexing functions ----------------------------------------------------------------------- *)

let lex_identifier lexer =
  let identifier = consume lexer is_alnum in
  let kind =
    match identifier with
    | "fn" -> Token.Fn
    | "let" -> Token.Let
    | "if" -> Token.If
    | "else" -> Token.Else
    | "true" | "false" -> Token.Boolean
    | _ -> Token.Name
  in
  make_token lexer kind

let lex_number lexer =
  ignore (consume lexer is_digit);
  if peek lexer 0 = Some '.' then (
    advance lexer 1;
    ignore (consume lexer is_digit)
  );
  make_token lexer Token.Number

let lex_string lexer =
  advance lexer 1;
  ignore (consume lexer (fun c -> c <> '"'));
  if peek lexer 0 <> Some '"' then
    make_token lexer Token.BadString
  else (
    advance lexer 1;
    make_token lexer Token.String
  )

let lex_symbol lexer =
  let (kind, length) =
    match (peek lexer 0, peek lexer 1) with
    | (Some '<', Some '-') -> (Token.Update, 2)
    | (Some '|', Some '|') -> (Token.Or, 2)
    | (Some '&', Some '&') -> (Token.And, 2)
    | (Some '=', Some '=') -> (Token.Eq, 2)
    | (Some '!', Some '=') -> (Token.Ne, 2)
    | (Some '<', Some '=') -> (Token.Le, 2)
    | (Some '>', Some '=') -> (Token.Ge, 2)
    | (Some '.', Some '.') -> (Token.Concat, 2)
    | (Some '=', _) -> (Token.Assign, 1)
    | (Some '<', _) -> (Token.Lt, 1)
    | (Some '>', _) -> (Token.Gt, 1)
    | (Some '+', _) -> (Token.Add, 1)
    | (Some '-', _) -> (Token.Sub, 1)
    | (Some '*', _) -> (Token.Mul, 1)
    | (Some '/', _) -> (Token.Div, 1)
    | (Some '!', _) -> (Token.Not, 1)
    | (Some '&', _) -> (Token.Ref, 1)
    | (Some '@', _) -> (Token.Deref, 1)
    | (Some '(', _) -> (Token.LParen, 1)
    | (Some ')', _) -> (Token.RParen, 1)
    | (Some '{', _) -> (Token.LBrace, 1)
    | (Some '}', _) -> (Token.RBrace, 1)
    | (Some ',', _) -> (Token.Comma, 1)
    | (Some ';', _) -> (Token.Semicolon, 1)
    | _ -> (Token.Unknown, 1)
  in
  advance lexer length;
  make_token lexer kind

let rec next lexer =
  ignore (consume lexer is_whitespace);
  lexer.start <- lexer.current;
  match peek lexer 0 with
  | Some '/' ->
    if peek lexer 1 = Some '/' then (
      skip_comment lexer;
      next lexer
    ) else
      lex_symbol lexer
  | Some '"' -> lex_string lexer
  | Some c when is_alpha c -> lex_identifier lexer
  | Some c when is_digit c -> lex_number lexer
  | Some _ -> lex_symbol lexer
  | None -> make_token lexer Token.Eof
