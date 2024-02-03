open Util
open Datastructures
open! Fun

type token =
  | Term of string
  | NonTerm of string

type 'a grammar = (string * token list * 'a) list

module Token = struct
  type t = token

  let compare (a : t) (b : t) =
    match a, b with
    | NonTerm a, NonTerm b | Term a, Term b -> String.compare a b
    | NonTerm _, Term _ -> -1
    | Term _, NonTerm _ -> 1


  let to_string = function
    | Term x -> sp "`%s`" x
    | NonTerm x -> x


  let non_term = function
    | NonTerm x -> Some x
    | _ -> None


  let term = function
    | Term x -> Some x
    | _ -> None


  let fold ~non_term ~term (tok : t) =
    match tok with
    | Term x -> term x
    | NonTerm x -> non_term x
end

module TokenS = MakeSet (Token)
module TokenM = MakeMap (Token)

type production = string * token list

(* lookahead cannot be NonTerm, should be just string *)
and state =
  { rule : string
  ; before : token list
  ; after : token list
  ; lookahead : TokenS.t
  }

type graph = (state list * token UidM.t) Datastructures.UidM.t

let string_of_token = Token.to_string

let rec fix f init =
  let fixed = f init in
  if fixed = init then init else fix f fixed


let string_of_state { rule; before; after; lookahead } =
  sp
    "%s -> %s . %s %s"
    rule
    (sl string_of_token " " before)
    (sl string_of_token " " after)
    (TokenS.to_string lookahead)


let is_reduction = function
  | { after = []; _ } -> true
  | _ -> false


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


let productions grammar rule_name =
  List.filter_map (fun (k, v, _) -> if k = rule_name then Some v else None) grammar


let first_set grammar (rule : token) : TokenS.t =
  let pass (firsts : TokenS.t) =
    firsts
    |> TokenS.to_list
    |> List.filter_map Token.non_term
    |> List.map (fun x -> productions grammar x |> List.map List.hd |> TokenS.of_list)
    |> List.fold_left (fun ac x -> TokenS.union x ac) firsts
  in
  fix pass (TokenS.of_list [ rule ])
  |> TokenS.to_list
  |> List.filter (fun x -> Option.is_some @@ Token.term x)
  |> TokenS.of_list


let initial grammar rule_name =
  rule_name
  |> productions grammar
  |> List.map (fun prod ->
    { rule = rule_name; before = []; after = prod; lookahead = TokenS.empty })


let closure_pass grammar (states : state Set.t) =
  let rec closure_pass (acc : state Set.t) (states : state list) =
    match states with
    | [] -> acc
    | hd :: tl ->
      let x =
        List.nth_opt hd.after 0
        >>= fun follows ->
        Token.non_term follows $> fun follows -> initial grammar follows
      in
      let added = Option.fold ~none:[] ~some:Fun.id x in
      closure_pass (Set.union acc added) tl
  in
  closure_pass states states


let closure grammar (states : state list) =
  let ( >>= ) = Option.bind in
  let ( $> ) op f = Option.map f op in
  let pass (states : state Set.t) =
    let rec pass (acc : state Set.t) (states : state Set.t) =
      match states with
      | [] -> acc
      | hd :: tl ->
        let added =
          begin
            let x =
              begin
                match hd.after with
                | [] -> None
                | [ c ] -> Some (c, TokenS.of_list [ Term "" ])
                | c :: delta :: _ -> Some (c, first_set grammar delta)
              end
            in
            x
            (* TODO: check if delta can derive "" *)
            >>= fun (c, lookahead) ->
            Token.non_term c
            $> fun c ->
            let lookahead =
              if TokenS.mem (Term "") lookahead
              then TokenS.union (TokenS.remove (Term "") lookahead) hd.lookahead
              else lookahead
            in
            List.map (fun state -> { state with lookahead }) (initial grammar c)
          end
          |> Option.fold ~none:[] ~some:id
        in
        added |> fun x -> pass (Set.union acc x) tl
    in
    let o = pass states states in
    o
  in
  fix pass states


let out_edges (states : state list) =
  List.filter_map (fun xs -> List.nth_opt xs.after 0) states |> Set.from_list


let advance (edge : token) (states : state list) =
  let filt = function
    | { after = []; _ } -> None
    | { after = hd :: _; _ } when hd <> edge -> None
    | { rule; before; lookahead; after = _ :: tl } ->
      Some { rule; before = before @ [ edge ]; after = tl; lookahead }
  in
  List.filter_map filt states


let make_graph (grammar : 'a grammar) (states : state list) =
  let uid =
    let i = ref 0 in
    fun () ->
      i := !i + 1;
      sp "node%d" !i
  in
  let rec make_graph (g : graph) (states : state list) : string * graph =
    let st = closure grammar states in
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


type action =
  | Reduce of (string * token list)
  | Shift of string

type table = (action UidM.t * string UidM.t) UidM.t

let string_of_action = function
  | Shift x -> sp "s%s" x
  | Reduce (r, expansion) -> sp "%s -> %s" r (sl string_of_token " " expansion)


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


let table_of_graph (g : graph) : table =
  let g = UidM.to_list g in
  let map_state (states : state list) (neighbors : token UidM.t)
    : action UidM.t * string UidM.t
    =
    let shifts =
      UidM.to_list neighbors
      |> List.filter_map (fun (a, b) -> Token.term b $> fun b -> b, Shift a)
      |> UidM.of_list
    in
    let gotos =
      UidM.to_list neighbors
      |> List.filter_map (fun (a, b) -> Token.non_term b $> fun b -> b, a)
      |> UidM.of_list
    in
    let reductions =
      states
      |> List.filter is_reduction
      |> List.map (fun x ->
        ( (match TokenS.to_list x.lookahead with
           | [] -> [ Term "$" ]
           | x -> x)
        , Reduce (x.rule, x.before) ))
      |> List.map (fun (lookahead, reduce) ->
        lookahead |> List.filter_map Token.term |> List.map (fun x -> x, reduce))
      |> List.concat
      |> UidM.of_list
    in
    (* let keys map = UidM.to_list map |> List.map fst |> UidS.of_list in *)
    (* if UidS.inter (keys shifts) (keys reductions) |> UidS.is_empty |> not *)
    (* then failwith "unresolved shift/reduce conflict"; *)
    ( UidM.union
        (fun key l r ->
          failwith
            (sp
               "shift/reduce conflict in %s: can %s but also %s"
               key
               (string_of_action r)
               (string_of_action l)))
        shifts
        reductions
    , gotos )
  in
  let x =
    List.map
      (fun (state_id, (states, neighbors)) -> state_id, map_state states neighbors)
      g
  in
  UidM.of_list x


let accepting_state (t : table) : string =
  UidM.to_list t
  |> List.find_map (fun (_, (actions, _)) ->
    UidM.find_opt "$" actions
    >>= function
    | Reduce _ -> None
    | Shift t -> Some t)
  |> Option.fold ~none:"" ~some:id


let string_of_table
  ?(action_cell : string -> string = id)
  ?(goto_cell : string -> string = id)
  ?(row : string -> string = id)
  ?(header_action : string -> string = id)
  ?(header_goto : string -> string = id)
  ?(header_node : string -> string = id)
  (t : table)
  : string
  =
  let terminals =
    UidM.fold
      (fun _ (actions, _) terminals ->
        UidM.to_list actions |> List.map fst |> UidS.of_list |> UidS.union terminals)
      t
      UidS.empty
  in
  let non_terminals =
    UidM.fold
      (fun _ (_, gotos) non_terminals ->
        UidM.to_list gotos |> List.map fst |> UidS.of_list |> UidS.union non_terminals)
      t
      UidS.empty
  in
  let terminals = UidS.to_list terminals in
  let non_terminals = UidS.to_list non_terminals in
  let transitions =
    UidM.to_list t
    |> List.map (fun (st, (actions, gotos)) ->
      let actions =
        terminals
        |> List.map (fun x -> UidM.find_opt x actions)
        |> List.map (fun x -> x >>= fun x -> Some x)
        |> List.map (Option.fold ~none:"" ~some:string_of_action)
        |> List.map action_cell
        |> sl id "\t"
      in
      let gotos =
        non_terminals
        |> List.map (fun x -> UidM.find_opt x gotos)
        |> List.map (Option.fold ~none:"" ~some:(sp "g%s"))
        |> List.map goto_cell
        |> sl id "\t"
      in
      row (sp "%s\t%s\t\t%s" (header_node st) actions gotos))
  in
  let header =
    row
    @@ sp
         "%s\t%s\t\t%s"
         (header_node "")
         (sl header_action "\t" terminals)
         (sl header_goto "\t" non_terminals)
  in
  sp "%s\n%s" header (String.concat "\n" transitions)
