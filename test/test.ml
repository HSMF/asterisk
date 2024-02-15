open OUnit2
open Lib.Generator
open! Lib.Util

let ug = List.map (fun (a, b) -> a, b, "()")
let t x = Term x
let nt x = NonTerm x

(* let () = *)
(*   let grammar = ug [ "A", [ t "value"; nt "A" ]; "A", [] ] in *)
(*   print_endline @@ TokenS.to_string @@ first_set grammar (nt "A"); *)
(*   failwith "hey" *)

let spec =
  let f = open_in "./grammar-lex.ast" in
  let s = In_channel.input_all f in
  close_in f;
  s


let tests =
  let terms = List.map t >>> TokenS.of_list in
  let only_terms =
    TokenS.to_list >>> List.filter_map Token.term >>> List.map t >>> TokenS.of_list
  in
  let printer = TokenS.to_string in
  let assert_equal_tokens = assert_equal ~printer in
  let assert_equal_lex =
    assert_equal ~printer:(sl Lib.Spec.Lex.string_of_token ",\n " >>> sp "{%s}")
  in
  "generator tests"
  >::: [ (let grammar =
            ug [ "A", [ t "a"; t "b" ]; "A", [ t "c"; t "d" ]; "A", [ t "e"; t "f" ] ]
          in
          "first_set trivial"
          >:: fun _ ->
          assert_equal_tokens
            (terms [ "a"; "c"; "e" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar =
            ug [ "A", [ nt "B"; t "_" ]; "B", [ t "c"; t "d" ]; "A", [ t "e"; t "f" ] ]
          in
          "first_set follow"
          >:: fun _ ->
          assert_equal_tokens
            (terms [ "c"; "e" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar = ug [ "A", [] ] in
          "first_set empty"
          >:: fun _ ->
          assert_equal_tokens
            (terms [ "<empty>" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar = ug [ "A", [ t "value"; nt "A" ]; "A", [] ] in
          "first_set empty"
          >:: fun _ ->
          assert_equal_tokens
            (terms [ "<empty>"; "value" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; ("tokenize grammar"
          >:: fun _ ->
          assert_equal_lex
            Lib.Spec.Lex.(
              let a = Ident "A" in
              let b = Ident "B" in
              [ Ident "TARGET"
              ; Equals
              ; Ident "ocaml"
              ; Ident "PRELUDE"
              ; Equals
              ; Literal "\n    open Lex\n"
              ; a
              ; Colon
              ; Literal " Ast.elt "
              ; Pipe
              ; a
              ; Ident "And"
              ; b
              ; Literal " Ast.And(v0, v2) "
              ; Pipe
              ; b
              ; Literal " v0 "
              ])
            (Lib.Spec.Lex.lex spec))
       ; ("tokenize literal"
          >:: fun _ ->
          assert_equal_lex
            Lib.Spec.Lex.[ Literal "{he{llo}world}" ]
            (Lib.Spec.Lex.lex "{{he{llo}world}}"))
       ; Lib.Spec.Lex.(
           "oop"
           >:: fun _ ->
           assert_raises (Lex_error "Unexpected EOF in Literal") (fun () -> lex "{{{}}"))
       ]


let _ = run_test_tt_main tests
