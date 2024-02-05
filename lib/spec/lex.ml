(** hand written lexer for the spec language bc why not *)

open Util

exception Lex_error of string

type token =
  | Equals
  | Colon
  | Pipe
  | Target
  | Prelude
  | Literal of string
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


let literal (s : stream) =
  let buf = Buffer.create 16 in
  let rec scan depth (s : stream) =
    if depth = 0
    then s
    else begin
      match s () with
      | Seq.Nil -> raise (Lex_error "Unexpected EOF in Literal")
      | Seq.Cons (hd, tl) -> begin
        let depth =
          match hd with
          | '{' -> depth + 1
          | '}' -> depth - 1
          | _ -> depth
        in
        if depth <> 0 then Buffer.add_char buf hd;
        scan depth tl
      end
    end
  in
  let rem = scan 1 s in
  Literal (Buffer.contents buf), rem


let special_tokens = [ "TARGET", Target; "PRELUDE", Prelude ]

let next_token (s : stream) =
  let s = skip_whitespace s in
  match s () with
  | Nil -> None
  | Cons ('=', tl) -> Some (Equals, tl)
  | Cons (':', tl) -> Some (Colon, tl)
  | Cons ('|', tl) -> Some (Pipe, tl)
  | Cons ('{', tl) -> Some (literal tl)
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
  | Equals -> "="
  | Colon -> ":"
  | Pipe -> "|"
  | Target -> "TARGET"
  | Prelude -> "PRELUDE"
  | Literal s -> sp "Literal (%s)" (String.escaped s)
  | Ident id -> sp "Ident (%s)" id
