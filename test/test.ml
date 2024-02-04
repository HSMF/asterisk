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


let tests =
  let terms = List.map t >>> TokenS.of_list in
  let only_terms =
    TokenS.to_list >>> List.filter_map Token.term >>> List.map t >>> TokenS.of_list
  in
  let printer = TokenS.to_string in
  let assert_equal = assert_equal ~printer in
  "generator tests"
  >::: [ (let grammar =
            ug [ "A", [ t "a"; t "b" ]; "A", [ t "c"; t "d" ]; "A", [ t "e"; t "f" ] ]
          in
          "first_set trivial"
          >:: fun _ ->
          assert_equal
            (terms [ "a"; "c"; "e" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar =
            ug [ "A", [ nt "B"; t "_" ]; "B", [ t "c"; t "d" ]; "A", [ t "e"; t "f" ] ]
          in
          "first_set follow"
          >:: fun _ ->
          assert_equal (terms [ "c"; "e" ]) (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar = ug [ "A", [] ] in
          "first_set empty"
          >:: fun _ ->
          assert_equal
            (terms [ "<empty>" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ; (let grammar = ug [ "A", [ t "value"; nt "A" ]; "A", [] ] in
          "first_set empty"
          >:: fun _ ->
          assert_equal
            (terms [ "<empty>"; "value" ])
            (only_terms @@ first_set grammar (NonTerm "A")))
       ]


let _ = run_test_tt_main tests
