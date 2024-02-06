(** lexer adapted from spec/lex *)
open! Fun

let sp = Printf.sprintf

exception Lex_error of string

type token =
  | Fn
  | Colon
  | Semicolon
  | Comma
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Decl
  | Ident of string

type stream = char Seq.t

let rec skip_whitespace (s : stream) =
  match s () with
  | Cons (hd, tl) when hd = ' ' || hd = '\t' || hd = '\r' || hd = '\n' ->
    skip_whitespace tl
  | _ -> s


let in_range lo hi ch = lo <= ch && ch <= hi
let one_of sel ch = List.exists (( = ) ch) sel
let is_ident_char ch = in_range 'a' 'z' ch || in_range 'A' 'Z' ch || one_of [ '_' ] ch

let ident (first : char) (s : stream) =
  let token = Seq.take_while is_ident_char s in
  let token = Seq.cons first token |> String.of_seq in
  let remainder = Seq.drop_while is_ident_char s in
  token, remainder


let special_tokens = [ "fn", Fn ]

let rec next_token (s : stream) =
  let s = skip_whitespace s in
  match s () with
  | Nil -> None
  | Cons ('(', tl) -> Some (OpenParen, tl)
  | Cons (')', tl) -> Some (CloseParen, tl)
  | Cons ('{', tl) -> Some (OpenBrace, tl)
  | Cons ('}', tl) -> Some (CloseBrace, tl)
  | Cons (';', tl) -> Some (Semicolon, tl)
  | Cons (':', tl) -> begin
    match tl () with
    | Cons ('=', tl) -> Some (Decl, tl)
    | _ -> Some (Colon, tl)
  end
  | Cons (',', tl) -> Some (Comma, tl)
  | Cons ('#', tl) ->
    let tl = Seq.drop_while (negate @@ one_of [ '\n'; '\r' ]) tl in
    next_token tl
  | Cons (hd, tl) ->
    if is_ident_char hd
    then (
      let tok, rem = ident hd tl in
      Some
        (List.assoc_opt tok special_tokens |> Option.fold ~some:id ~none:(Ident tok), rem))
    else raise (Lex_error (sp "Unexpected character '%c'" hd))


let lex (s : string) : token list =
  let s = String.to_seq s in
  let rec loop s =
    match next_token s with
    | Some (x, s) -> x :: loop s
    | None -> []
  in
  loop s


let string_of_token = function
  | Colon -> "Colon"
  | Semicolon -> "Semicolon"
  | Ident id -> sp "Ident (%s)" id
  | Fn -> "Fn"
  | Comma -> "Comma"
  | OpenParen -> "OpenParen"
  | CloseParen -> "CloseParen"
  | OpenBrace -> "OpenBrace"
  | Decl -> "Decl"
  | CloseBrace -> "CloseBrace"
