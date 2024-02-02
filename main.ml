open Printf
open Datastructures
open! Fun

let sp = sprintf
let sl map sep lst = lst |> List.map map |> String.concat sep
let ( >>= ) = Option.bind
let ( $> ) op f = Option.map f op
let ( >>> ) f g x = x |> f |> g

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

module List = struct
  include List

  let group (n : int) (lst : 'a list) =
    let rec aux acc m lst =
      match lst, acc with
      | [], [] -> []
      | [], x -> [ rev x ]
      | hd :: tl, x ->
        if m = 0 then rev x :: aux [] n (hd :: tl) else aux (hd :: x) (m - 1) tl
    in
    aux [] n lst
end

type token =
  | Term of string
  | NonTerm of string

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

(* let grammar = *)
(*   [ "S0", [ NonTerm "A"; Term "$" ], "$[0]" *)
(*   ; "A", [ NonTerm "A"; Term "and"; NonTerm "B" ], "({op: 'and', l: $[0], r: $[2]})" *)
(*   ; "A", [ NonTerm "B" ], "$[0]" *)
(*   ; "B", [ NonTerm "B"; Term "or"; NonTerm "C" ], "({op: 'or', l: $[0], r: $[2]})" *)
(*   ; "B", [ NonTerm "C" ], "$[0]" *)
(*   ; "C", [ Term "("; NonTerm "A"; Term ")" ], "$[1]" *)
(*   ; "C", [ Term "v"; Term "="; Term "v" ], "({op: 'cmp', l: $[0], r: $[0]})" *)
(*   ] *)

let grammar =
  [ "S0", [ NonTerm "A"; Term "$" ], "buf[0]"
  ; ( "A"
    , [ NonTerm "A"; Term "and"; NonTerm "B" ]
    , "new And((Operand)buf[0], (Operand) buf[2])" )
  ; "A", [ NonTerm "B" ], "buf[0]"
  ; ( "B"
    , [ NonTerm "B"; Term "or"; NonTerm "C" ]
    , "new Or((Operand)buf[0], (Operand)buf[2])" )
  ; "B", [ NonTerm "C" ], "buf[0]"
  ; "C", [ Term "("; NonTerm "A"; Term ")" ], "buf[1]"
  ; "C", [ Term "v"; Term "="; Term "v" ], "new Eq((String)buf[0], (String)buf[2])"
  ]


(* let grammar = *)
(*   [ "S0", [ NonTerm "S"; Term "$" ], "$[0]" *)
(*   ; "S", [ Term "("; NonTerm "L"; Term ")" ], "({group: $[1]})" *)
(*   ; "S", [ Term "id" ], "({id: `${$[0]}`})" *)
(*   ; "L", [ NonTerm "S" ], "({l:$[0]})" *)
(*   ; "L", [ NonTerm "L"; Term ","; NonTerm "S" ], "({l:$[0], r:$[2]})" *)
(*   ] *)

(* let grammar = *)
(*   [ "S0", [ NonTerm "S"; Term "$" ] *)
(*   ; "S", [ NonTerm "E"; Term "+"; NonTerm "S" ] *)
(*   ; "S", [ NonTerm "E" ] *)
(*   ; "E", [ Term "number" ] *)
(*   ; "E", [ Term "("; NonTerm "S"; Term ")" ] *)
(*   ] *)

let productions rule_name =
  List.filter_map (fun (k, v, _) -> if k = rule_name then Some v else None) grammar


let first_set (rule : token) : TokenS.t =
  let pass (firsts : TokenS.t) =
    firsts
    |> TokenS.to_list
    |> List.filter_map Token.non_term
    |> List.map (fun x -> productions x |> List.map List.hd |> TokenS.of_list)
    |> List.fold_left (fun ac x -> TokenS.union x ac) firsts
  in
  fix pass (TokenS.of_list [ rule ])
  |> TokenS.to_list
  |> List.filter (fun x -> Option.is_some @@ Token.term x)
  |> TokenS.of_list


let initial rule_name =
  rule_name
  |> productions
  |> List.map (fun prod ->
    { rule = rule_name; before = []; after = prod; lookahead = TokenS.empty })


let closure_pass (states : state Set.t) =
  let rec closure_pass (acc : state Set.t) (states : state list) =
    match states with
    | [] -> acc
    | hd :: tl ->
      let x =
        List.nth_opt hd.after 0
        >>= fun follows -> Token.non_term follows $> fun follows -> initial follows
      in
      let added = Option.fold ~none:[] ~some:Fun.id x in
      closure_pass (Set.union acc added) tl
  in
  closure_pass states states


let closure (states : state list) =
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
                | c :: delta :: _ -> Some (c, first_set delta)
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
            List.map (fun state -> { state with lookahead }) (initial c)
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
      |> List.map (fun x -> TokenS.to_list x.lookahead, Reduce (x.rule, x.before))
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


let js_of_table (table : table) =
  let accept = accepting_state table in
  printf "accept: %s\n" accept;
  let table = UidM.to_list table in
  let prelude =
    {|
    const assert = (cond, msg) => {if (!cond) throw ('parse error: ' + msg)};
  |}
  in
  let reduce expansion =
    expansion |> List.rev |> sl (const "buf.push(stack.pop());") "\n  "
  in
  let case (token : string) (action : action) =
    let body =
      match action with
      | Reduce (rule, expansion) ->
        let _, _, code = List.find (fun (r, e, _) -> rule = r && expansion = e) grammar in
        sp
          {|const buf = [];
        %s


        buf.reverse();
        const value = (($) => %s)(buf.map(x => x.value))
        const before = stack[stack.length - 1];
        const goto = goto_%s[before.state];
        assert(goto !== undefined);
        stack.push({state: goto[0], type: "%s", value})
        return goto[1](input, stack);|}
          (reduce expansion)
          code
          rule
          rule
      | Shift goto ->
        sp
          {|const elem = {...input.shift(), state: "%s"};
        stack.push(elem);
        return %s(input, stack);|}
          goto
          goto
    in
    sp {|if (head.type === "%s") {
      %s
    }
  |} token body
  in
  let map_action ((state, (actions, _)) : string * (action UidM.t * string UidM.t)) =
    let body =
      actions |> UidM.to_list |> sl (fun (token, action) -> case token action) "\n  "
    in
    sp
      "function %s (input, stack) {\n  const head = input[0]; %s %s\n}"
      state
      body
      (if state = accept
       then "return stack[1].value"
       else sp "assert (false, 'reached end in state %s')" state)
  in
  let goto_tables =
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
             "const goto_%s = {\n  %s\n}"
             state
             (sl (fun (st, goto) -> sp "%s: ['%s', %s]" st goto goto) ",\n  " x))
         "\n"
  in
  sp {|
    %s

    %s

    %s
    |} prelude goto_tables (sl map_action "\n" table)


let java_of_table
  ~(prelude : string)
  ~(return_type : string)
  ~(token_id : string -> string)
  (table : table)
  : string
  =
  let accept = accepting_state table in
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
  let table = UidM.to_list table in
  let reduce expansion =
    expansion
    |> List.rev
    |> sl
         (fun x ->
           sp
             {|checkedPush(b, stack, TokenTypeStack.%s, "%s");|}
             (match x with
              | NonTerm x -> x
              | Term x -> token_id x)
             (string_of_token x))
         "\n        "
  in
  let case (token : string) (action : action) =
    let body =
      match action with
      | Reduce (rule, expansion) ->
        let _, _, code = List.find (fun (r, e, _) -> rule = r && expansion = e) grammar in
        sp
          {|  ArrayList<Object> b = new ArrayList<>();
        %s
        rev(b);
        Object[] buf = b.toArray();

        Object value = %s;

        StackItem before = stack.get(stack.size() - 1);
        State go = goto_%s(before.state);
        ensure(go != null);
        stack.add(new StackItem(go.id(), TokenTypeStack.%s, value));
        return go.parse();|}
          (reduce expansion)
          code
          rule
          rule
      | Shift goto ->
        sp
          {|
          Token tmp = input.remove(input.size() - 1);
          System.err.println("removing " + tmp);
          stack.add(new StackItem(StateId.%s, cast(head.id()), head.value()));
          return new %s().parse();|}
          goto
          goto
    in
    sp {|if (head.id() == TokenId.%s) {
      %s
    }
  |} (token_id token) body
  in
  let map_action ((state, (actions, _)) : string * (action UidM.t * string UidM.t)) =
    let error_message =
      sp
        {|"expected one of {%s} in state %s.\nDebug info: " + stack + " -- " + input|}
        (actions
         |> UidM.to_list
         |> sl
              (fun (tok, _) -> sp "%s (%s)" (string_of_token (Term tok)) (token_id tok))
              ", ")
        state
    in
    let body =
      actions |> UidM.to_list |> sl (fun (token, action) -> case token action) "\n  "
    in
    sp
      {|
      private class %s implements State {
        @Override
        public StateId id() {
          return StateId.%s;
        }

        @Override
        public Object parse() throws ParseError {
          %s
        }
      }
      |}
      state
      state
      (if state = accept
       then "return stack.get(1).value;"
       else
         sp
           {|
           Token head = input.get(input.size() - 1);
           %s
           throw new ParseError(%s);|}
           body
           error_message)
  in
  let goto_tables =
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
           sp (* "const goto_%s = {\n  %s\n}" *)
             {|private State goto_%s(StateId state) throws ParseError {
           switch (state) {
                %s
                default: throw new ParseError();
           }
         }|}
             state
             (sl
                (fun (st, goto) -> sp "case %s: return new %s();" st goto)
                "\n                "
                x))
         "\n\n\n    "
  in
  let state_ids = List.map fst table in
  let casts =
    sl
      (fun x ->
        let x = token_id x in
        sp "case %s: return TokenTypeStack.%s;" x x)
      "\n          "
      all_terminals
  in
  sp
    {|// this file is autogenerated. modify at your own risk

    %s

import java.util.ArrayList;

  public class Parser {
    public static class ParseError extends Exception {
      private String message;

      public ParseError() {
        message = null;
      }

      public ParseError(String msg) {
        message = msg;
      }

      @Override
      public String toString() {
        String msg = "Parse Error";
        if (message != null) {
          return msg + ": " + message;
        }
        return msg;
      }
    }

    private static void ensure(boolean condition) throws ParseError {
      if (!condition) {
        throw new ParseError();
      }
    }

    private static void rev(ArrayList<Object> a) {
        for (int i = 0; i < a.size() / 2; i++) {
            Object tmp = a.get(i);
            a.set(i, a.get(a.size() - i - 1));
            a.set(a.size() - i - 1, tmp);
        }
    }

    private static void checkedPush(
      ArrayList<Object> buf,
      ArrayList<StackItem> stack,
      TokenTypeStack expectedType,
      String expectedLabel) throws ParseError {
      StackItem top = stack.remove(stack.size() - 1);
      ensure(top.type == expectedType, "expected token of type " + expectedLabel + " but got token of type " + top.type);
      buf.add(top.value);
    }


    private static void ensure(boolean condition, String msg) throws ParseError {
      if (!condition) {
        throw new ParseError(msg);
      }
    }

    private static enum TokenTypeStack {
        %s,
        %s
    }

    private static TokenTypeStack cast(TokenId id) throws ParseError {
        switch (id) {
          %s
          default: throw new ParseError("token " + id + " isn't in grammar.");
        }
    }


    private static class StackItem {
        Object value;
        StateId state;
        TokenTypeStack type;

        public StackItem(StateId state, TokenTypeStack tokenTypeStack, Object value) {
            this.state = state;
            this.value = value;
            this.type = tokenTypeStack;
        }

        @Override
        public String toString() { return "[" + state + " " + type + " " + value + "]";}
    }

    ArrayList<Token> input;
    ArrayList<StackItem> stack;

    public Parser(Token[] tokens) {
        input = new ArrayList<>();
        input.ensureCapacity(tokens.length);
        for (int i = tokens.length - 1; i >= 0; i--) {
            input.add(tokens[i]);
        }

        stack = new ArrayList<>();
        stack.add(new StackItem(StateId.node1, TokenTypeStack.Eof, null));
    }

    private static enum StateId {
      %s
    }

    private static interface State {
        StateId id();

        Object parse() throws ParseError;
    }

    public %s parse() throws ParseError {
      return (%s)(new node1().parse());
    }




  %s

  %s
  }
    |}
    prelude
    (sl token_id ", " all_terminals)
    (sl id ", " all_non_terminals)
    casts
    (sl id ",\n      " (List.group 5 state_ids |> List.map (sl id ", ")))
    return_type
    return_type
    goto_tables
    (sl map_action "\n" table)


let s0 = "S0"
let init_state = List.hd (initial s0)

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
  let table = table_of_graph graph in
  printf "generated a table with %d states\n" (UidM.cardinal table);
  printf "%s\n\n" (string_of_table ~header_action:(fun x -> sp "`%s`" x) table);
  let th = sp {|<th class="px-2 py-1 border bg-slate-300 border-slate-500">%s</th>|} in
  let td ?(bg_color = "bg-slate-200") =
    sp {|<td class="px-2 py-1 border border-slate-500 %s">%s</td>|} bg_color
  in
  let tr = sp {|<tr class="px-2 py-1 border bg-slate-200 border-slate-500">%s</tr>|} in
  let f = open_out "table.html" in
  fprintf
    f
    {|
    <html>
    <head>
      <script src="https://cdn.tailwindcss.com"></script>
      <title>table for grammar</title>
    <script>
    %s

    /*
    const input = [
      {type: '('},
      {type: 'id', value: 'x'},
      {type: ','},
      {type: '('},
      {type: 'id', value: 'y'},
      {type: ','},
      {type: 'id', value: 'z'},
      {type: ')'},
      {type: ','},
      {type: 'id', value: 'w'},
      {type: ')'},
      {type: '$'},
    ]
    */

    // a = b or a = c
    const input = [
      {type: 'v', value: 'a' },
      {type: '='},
      {type: 'v', value: 'b' },
      {type: 'or'},
      {type: 'v', value: 'a' },
      {type: '='},
      {type: 'v', value: 'c' },
      {type: 'and'},
      {type: 'v', value: 'b' },
      {type: '='},
      {type: 'v', value: 'c' },
      {type: '$'},
    ]

    function foo() {
      const i = [...input]
    console.log(node1(i, [{type: "", state: "node1"}]));
    }
    </script>
    </head>
    <body>
      <div class="flex flex-wrap gap-2 p-2">
        <div>
          <ul>%s</ul>
          <button class="p-2 bg-blue-300 text-white" onclick="foo()">clikc me</button>
          <table>
          %s
          </table>
        </div>

        <div>
          <a href="hey.dot.svg" target="_blank">
          <img src="hey.dot.svg"></img>
          </a>
        </div>
      </div>
      </body>
    </html>
    |}
    (js_of_table table)
    (sl
       (fun (rule, prod, _) ->
         sp "<li>%s &rarr; %s</li>" rule (sl string_of_token " " prod))
       "\n"
       grammar)
    (string_of_table
       ~header_goto:th
       ~header_action:th
       ~header_node:th
       ~action_cell:td
       ~row:tr
       ~goto_cell:(td ~bg_color:"bg-blue-200")
       table);
  close_out f;
  let f = open_out "parser.js" in
  output_string f (js_of_table table);
  close_out f;
  let f = open_out "parser.java" in
  output_string
    f
    (java_of_table
       ~prelude:
         {|
package com.hyde.app;

import com.hyde.app.ast.And;
import com.hyde.app.ast.Or;
import com.hyde.app.ast.Eq;
import com.hyde.app.ast.Operand;
import com.hyde.app.tokens.Token;
import com.hyde.app.tokens.TokenId;
      |}
       ~return_type:"Operand"
       ~token_id:(fun x ->
         List.assoc
           x
           [ "(", "OpenParen"
           ; ")", "CloseParen"
           ; "and", "And"
           ; "or", "Or"
           ; "=", "Equals"
           ; "col", "Col"
           ; "v", "Val"
           ; "$", "Eof"
           ])
       table);
  close_out f;
  ()
