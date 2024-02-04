open Generator
open! Printf
open Datastructures
open Util

(** generates javascript of a table *)
let js_of_table grammar (table : table) =
  let accept = accepting_state table in
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


(** generates html of table formatted using tailwindcss. for debugging purposes mainly *)
let html_of_table grammar (input : string list) (table : table) : string =
  let th = sp {|<th class="px-2 py-1 border bg-slate-300 border-slate-500">%s</th>|} in
  let td ?(bg_color = "bg-slate-200") =
    sp {|<td class="px-2 py-1 border border-slate-500 %s">%s</td>|} bg_color
  in
  let tr = sp {|<tr class="px-2 py-1 border bg-slate-200 border-slate-500">%s</tr>|} in
  sp
    {|
    <html>
    <head>
      <script src="https://cdn.tailwindcss.com"></script>
      <title>table for grammar</title>
    <script>
    %s

    const input = [
      %s
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
          <a href="dfa.dot.svg" target="_blank">
          <img src="dfa.dot.svg"></img>
          </a>
        </div>
      </div>
      </body>
    </html>
    |}
    (js_of_table grammar table)
    (sl id ", " input)
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
       table)
