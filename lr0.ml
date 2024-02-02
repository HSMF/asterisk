(** LR(0) parse graph generator. The LR(1) parser generator builds on this so it
 is here for historic reasons *)

open Printf
open Datastructures
open! Fun

let sp = sprintf
let sl map sep lst = lst |> List.map map |> String.concat sep

module Set = struct
  type 'a t = 'a list

  let rec union xs = function
    | [] -> xs
    | hd :: tl ->
      if List.find_opt (fun x -> x = hd) xs |> Option.is_some
      then union xs tl
      else union (hd :: xs) tl


  let from_list lst =
    let rec helper acc = function
      | [] -> acc
      | hd :: tl -> helper (union acc [ hd ]) tl
    in
    helper [] lst
end

type token =
  | Term of string
  | NonTerm of string

and production = string * token list

and state =
  { rule : string
  ; before : token list
  ; after : token list (* ; lookahead : token *)
  }

type graph = (state list * token UidM.t) Datastructures.UidM.t

let string_of_token = function
  | Term x -> sp "`%s`" x
  | NonTerm x -> x


let non_term = function
  | NonTerm x -> Some x
  | _ -> None


let rec fix f init =
  let fixed = f init in
  if fixed = init then init else fix f fixed


let string_of_state { rule; before; after } =
  sp "%s -> %s . %s" rule (sl string_of_token " " before) (sl string_of_token " " after)


let string_of_states = sl string_of_state "\n"
let print_states st = st |> string_of_states |> print_endline

let graph_to_string (g : graph) : string =
  let g = UidM.to_list g in
  let node_to_string ((l, (states, neighbors)) : string * (state list * token UidM.t)) =
    let text = sl string_of_state "\\l" states in
    let neighbor_edges =
      neighbors
      |> UidM.to_list
      |> List.map (fun (n, tok) ->
        sp "%s -> %s [label=\"%s\"];" l n (string_of_token tok))
      |> String.concat " "
    in
    sp
      {|
    %s [shape=box,labeljust=l, label="%s\l%s"];
    %s
  |}
      l
      l
      text
      neighbor_edges
  in
  let texts = List.map node_to_string g in
  sp {|
  digraph G {
    %s
  }
    |} (String.concat "\n" texts)


(* let rec node_to_string (n : node) = *)
(*   let id = uid () in *)
(*   let text = string_of_states n.st in *)
(*   let childs = List.map (fun (_, x) -> node_to_string x) n.children in *)
(*   ( id *)
(*   , sp *)
(*       {| *)
  (*     %s [label="%s"] *)
  (*     %s *)
  (*     %s *)
  (*     |} *)
(*       id *)
(*       text *)
(*       (childs |> List.map snd |> String.concat "\n") *)
(*       (childs *)
(*        |> List.map fst *)
(*        |> List.map (fun t -> sp "%s -> %s" id t) *)
(*        |> String.concat "\n") ) *)
(* in *)
(* let _, node = node_to_string n in *)
(* sp {| *)
  (*   digraph G { *)
  (*     %s *)
  (*   } *)
  (*   |} node *)

let grammar =
  [ "A'", [ NonTerm "A"; Term "$" ]
  ; "A", [ NonTerm "A"; Term "and"; NonTerm "B" ]
  ; "A", [ NonTerm "B" ]
  ; "B", [ NonTerm "B"; Term "or"; NonTerm "C" ]
  ; "B", [ NonTerm "C" ]
  ; "C", [ Term "("; NonTerm "A"; Term ")" ]
  ; "C", [ Term "v"; Term "="; Term "v" ]
  ]


(* let grammar = *)
(*   [ "S'", [ NonTerm "S"; Term "$" ] *)
(*   ; "S", [ Term "("; NonTerm "L"; Term ")" ] *)
(*   ; "S", [ Term "id" ] *)
(*   ; "L", [ NonTerm "S" ] *)
(*   ; "L", [ NonTerm "L"; Term ","; NonTerm "S" ] *)
(*   ] *)

let productions rule_name =
  List.filter_map (fun (k, v) -> if k = rule_name then Some v else None) grammar


let initial rule_name =
  rule_name
  |> productions
  |> List.map (fun prod -> { rule = rule_name; before = []; after = prod })


let closure_pass (states : state Set.t) =
  let rec closure_pass (acc : state Set.t) (states : state list) =
    let ( >>= ) = Option.bind in
    let ( $> ) op f = Option.map f op in
    match states with
    | [] -> acc
    | hd :: tl ->
      let x =
        List.nth_opt hd.after 0
        >>= fun follows -> non_term follows $> fun follows -> initial follows
      in
      let added = Option.fold ~none:[] ~some:Fun.id x in
      closure_pass (Set.union acc added) tl
  in
  closure_pass states states


let closure (states : state list) = fix closure_pass states

let out_edges (states : state list) =
  List.filter_map (fun xs -> List.nth_opt xs.after 0) states |> Set.from_list


let advance (edge : token) (states : state list) =
  let filt = function
    | { after = []; _ } -> None
    | { after = hd :: _; _ } when hd <> edge -> None
    | { rule; before; after = _ :: tl } ->
      Some { rule; before = before @ [ edge ]; after = tl }
  in
  List.filter_map filt states


let make_graph (states : state list) =
  let uid =
    let i = ref 0 in
    fun () ->
      i := !i + 1;
      sp "node%d" !i
  in
  let rec make_graph (g : graph) (states : state list) : string * graph =
    let st = closure states in
    match
      UidM.to_list g
      |> List.map (fun (k, (x, _)) -> k, x)
      |> List.find_opt (fun (_, x) -> x = st)
    with
    | Some (id, _) -> id, g
    | None ->
      let id = uid () in
      let g = UidM.add id (st, UidM.empty) g in
      let edges = out_edges st in
      let edge (g : graph) (e : token) : graph =
        let advanced = advance e st in
        let n, g = make_graph g advanced in
        UidM.update (fun (x, neighbors) -> x, UidM.add n e neighbors) id g
      in
      id, List.fold_left edge g edges
  in
  snd @@ make_graph UidM.empty states


type conflict =
  | ShiftReduce
  | ReduceReduce

let string_of_conflict = function
  | ShiftReduce -> "shift/reduce"
  | ReduceReduce -> "reduce/reduce"


let conflicts (g : graph) =
  let g = UidM.to_list g in
  let conflict (s : state list) =
    let num_states = List.length s in
    let reduces = List.filter (fun x -> List.is_empty x.after) s in
    if List.length reduces = 1 && num_states <> 1
    then Some ShiftReduce
    else if List.length reduces > 1
    then Some ReduceReduce
    else None
  in
  g |> List.filter_map (fun (k, (s, _)) -> Option.map (fun x -> k, x) @@ conflict s)


let s0 = "A'"
let init_state = List.hd (initial s0)
let () = print_states @@ closure (initial s0)
let () = print_endline "===="
let () = closure (initial s0) |> out_edges |> sl string_of_token ", " |> print_endline
let () = print_endline "===="
let () = print_states @@ (closure (initial s0) |> advance (NonTerm "A") |> closure)

let () =
  let graph = make_graph (initial s0) in
  let conflicts = conflicts graph in
  printf
    "conflicts %s\n"
    (sl
       (fun (state, conf) -> sp "%s in state %s" (string_of_conflict conf) state)
       ", "
       conflicts);
  let s = graph_to_string graph in
  let f = open_out "hey.dot" in
  fprintf f "%s" s;
  close_out f;
  ()
