open Printf
(*
   Grammar: { string * string * (string * string * (string list * string) list) list  }
  | TargetSpec PreludeSpec Rules { (v0, v1, v2) }

TargetSpec: { string }
  | Target Equals Ident { v2 }

PreludeSpec : { string }
  | Prelude Equals Literal { v2 }


Rules: { (string * string * (string list * string) list) list }
  | Rule Rules { v1 :: v0 }
  | { [] }

Rule: { string * string * (string list * string) list }
  | Ident Colon Literal CaseList { (v0, v2, v3) }

CaseList: { (string list * string) list }
  | Case CaseList { v0 :: v1 }
  | { [] }

Case: { string list * string }
  | Pipe Idents Literal { v1, v2 }

Idents: { string list }
  | Ident Idents { v0 :: v1 }
  | { [] }
*)

let own_grammar =
  let open Generator in
  let nt x = NonTerm x in
  let t x = Term x in
  mk_grammar
    ( "Grammar"
    , [ "Grammar", [ nt "TargetSpec"; nt "PreludeSpec"; nt "Rules" ], "(v0, v1, v2)"
      ; "TargetSpec", [ t "Target"; t "Equals"; t "Ident" ], "v2"
      ; "PreludeSpec", [ t "Prelude"; t "Equals"; t "Literal" ], "v2"
      ; "Rules", [ nt "Rule"; nt "Rules" ], " v0 :: v1 "
      ; "Rules", [], "[]"
      ; "Rule", [ t "Ident"; t "Colon"; t "Literal"; nt "CaseList" ], " (v0, v2, v3) "
      ; "CaseList", [ nt "Case"; nt "CaseList" ], " v0 :: v1 "
      ; "CaseList", [], " [] "
      ; "Case", [ t "Pipe"; nt "Idents"; t "Literal" ], " v1, v2 "
      ; "Idents", [ t "Ident"; nt "Idents" ], " v0 :: v1 "
      ; "Idents", [], " [] "
      ] )


(** returns target, prelude, types, grammar *)
let parse_spec (s : string)
  : string * string * (string -> string) * string Generator.grammar
  =
  let (target, prelude, rules)
    : string * string * (string * string * (string list * string) list) list
    =
    s |> Lex.lex |> Parser.parse
  in
  let open Datastructures in
  printf "%s\n" prelude;
  printf "%s\n" target;
  let non_terminals_types = List.map (fun (rule, typ, _) -> rule, typ) rules in
  let non_terminals = List.map fst non_terminals_types in
  let types x = List.assoc x non_terminals_types in
  let grammar =
    rules
    |> List.map (fun (rule, _, expansion) ->
      List.map
        (fun (expansion, code) ->
          ( rule
          , List.map
              (fun x ->
                if List.mem x non_terminals then Generator.NonTerm x else Generator.Term x)
              expansion
          , code ))
        expansion)
    |> List.concat
  in
  target, prelude, types, grammar