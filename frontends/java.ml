open Util
open Datastructures
open Generator

let java_of_table
  grammar
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