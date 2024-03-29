open! Printf
open Datastructures
open Util
open Generator

let ocaml_of_table
  ~(prelude : string)
  ~(token_id : string -> string)
  ~(non_term_types : string -> string)
  ~(token_type : string)
  ~(token_associated_type : string -> string option)
  ~(toplevel_rule : string)
  (grammar : string grammar)
  (table : table)
  : string
  =
  let is_eof = ( = ) "$" in
  let rec indent n = if n <= 0 then "" else "  " ^ indent (n - 1) in
  let table = UidM.to_list table in
  let all_terminals =
    grammar
    |> List.map (fun (_, x, _) -> x)
    |> List.flatten
    |> List.filter_map Token.term
    |> List.sort_uniq String.compare
  in
  let all_non_terminals =
    grammar |> List.map (fun (x, _, _) -> x) |> List.sort_uniq String.compare
  in
  let all_states = List.map fst table in
  let state_id = sp "State_%s" in
  let stack_value_nonterm = sp "StackValue_%s" in
  let stack_value_term x = sp "StackValue_%s" (token_id x) in
  let token_nonterm = sp "Token_%s" in
  let token_term tok = if is_eof tok then "TermEof" else "Term " ^ token_id tok in
  let gotos =
    let fold (state, (_, gotos)) collection =
      UidM.fold
        (fun sym goto col -> UidM.update_or [] (fun x -> (state, goto) :: x) sym col)
        gotos
        collection
    in
    List.fold_right fold table UidM.empty
    |> UidM.to_list
    |> sl
         (fun (state, x) ->
           sp
             {|and goto_%s (state: states) =
              match state with
                | %s
                | _ -> raise_msg ("couldn't match in goto %s")|}
             state
             (sl
                (fun (st, goto) -> sp "%s -> %s, %s" (state_id st) goto (state_id goto))
                "\n                | "
                x)
             state)
         ("\n\n" ^ indent 2)
  in
  let reduce expansion =
    let len = List.length expansion in
    expansion
    |> List.rev
    |> List.mapi (fun i x ->
      let i = len - i - 1 in
      let typ =
        Token.fold
          ~non_term:(stack_value_nonterm >>> Option.some)
          ~term:(fun x ->
            token_associated_type x >>= const (Option.some @@ stack_value_term x))
          x
      in
      sp
        {|let (_, typ, tmp), _stack = pop_stack _stack in
                let v%d = (match typ, tmp with
                | %s%s, %s -> %s
                | _ -> raise_msg ("expected token %s")) in
                ignore v%d;|}
        i
        (Token.fold
           ~non_term:(fun nt -> "NonTerm " ^ token_nonterm nt)
           ~term:token_term
           x)
        (Token.fold
           ~non_term:(const "")
           ~term:(token_associated_type >>> Option.fold ~none:"" ~some:(const " _"))
           x)
        (Option.fold ~none:"StackValue_None" ~some:(fun typ -> typ ^ " v") typ)
        (Option.fold ~none:"()" ~some:(const "v") typ)
        (string_of_token x)
        i)
    |> sl id ("\n" ^ indent 8)
  in
  let case (token : string) (action : action) =
    let body =
      match action with
      | Reduce (rule, expansion) ->
        let _, _, code = List.find (fun (r, e, _) -> rule = r && expansion = e) grammar in
        let goto =
          if rule = toplevel_rule
          then "_value"
          else
            sp
              {|
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_%s before in
                let _stack = (goto_id, NonTerm %s, %s _value) :: _stack in
                goto _stack input|}
              rule
              (token_nonterm rule)
              (stack_value_nonterm rule)
        in
        sp
          {|%s
                let _value = (%s) in
                %s|}
          (reduce expansion)
          code
          goto
      | Shift goto ->
        let shifted_token = if is_eof token then "TermEof" else "Term _head" in
        sp
          {|let _stack = (%s, %s, %s) :: _stack in
               %s _stack _input'|}
          (state_id goto)
          shifted_token
          (match token_associated_type token with
           | None -> "StackValue_None"
           | Some _ -> sp "%s (_value)" (stack_value_term token))
          goto
    in
    let match_case =
      if is_eof token
      then "[ (* EOF *) ] as _input'"
      else
        sp
          "(%s%s as _head) :: _input'"
          (token_id token)
          (token |> token_associated_type |> Option.fold ~none:"" ~some:(const " _value"))
    in
    sp {|| %s -> begin
                %s
             end
  |} match_case body
  in
  let map_action ((state, (actions, _)) : string * (action UidM.t * string UidM.t)) =
    let error_message =
      sp
        {|[%s], "%s", input|}
        (actions
         |> UidM.to_list
         |> List.filter (fst >>> ( <> ) "$")
         |> sl (fun (tok, _) -> token_id tok |> sp "\"%s\"") "; ")
        state
    in
    let body =
      actions
      |> UidM.to_list
      |> sl (fun (token, action) -> case token action) "\n             "
    in
    sp
      {|and %s (_stack: stack) (input: token list) =%s
      |}
      state
      (if state = ""
       then failwith {|match _stack with _::x::_ -> x | _ -> raise_msg ("empty stack")|}
       else
         sp (* could probably be List.hd, since EOF symbol does this checking *)
           {|
           match input with
             %s
             | _ -> raise (Parse_error (ErrUnexpectedToken (%s)))|}
           body
           error_message)
  in
  sp
    {|
    (* Autogenerated file *)
%s
  type error_data =
    | ErrMsg of string
    | ErrUnexpectedToken of string list * string * token list
  exception Parse_error of error_data

  type states =
  | %s
  type stack_value =
  | %s
    %s
  | StackValue_None

  type nonterm = %s
  type token_type = Term of %s | NonTerm of nonterm | TermEof
  type stack = (states * token_type * stack_value) list

  let parse input =
    let raise_msg m = raise (Parse_error (ErrMsg (m))) in
    let pop msg = function
      | [] -> raise_msg (msg)
      | hd::tl -> hd, tl in
    let pop_stack a = pop "stack" a in

    let rec _hello = ()

    %s

    %s

    in

    node1 [State_node1, NonTerm %s, StackValue_None] input

    |}
    prelude
    (sl_grouped 5 state_id " | " "\n  | " all_states)
    (sl
       (fun nonterm ->
         sp "%s of (%s)" (stack_value_nonterm nonterm) (non_term_types nonterm))
       "\n  | "
       all_non_terminals)
    (all_terminals
     |> List.filter_map (fun x -> token_associated_type x $> fun y -> x, y)
     |> sl (fun (token, value) -> sp "| %s of (%s)" (stack_value_term token) value) "\n  "
    )
    (sl token_nonterm " | " all_non_terminals)
    token_type
    gotos
    (sl map_action "\n    " table)
    (all_non_terminals |> List.hd |> token_nonterm)
