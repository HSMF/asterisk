
    (* Autogenerated file *)
open Lex
  exception Parse_error of string

  type states =
  | State_node1 | State_node10 | State_node11 | State_node12 | State_node13
  | State_node14 | State_node15 | State_node16 | State_node17 | State_node18
  | State_node19 | State_node2 | State_node20 | State_node21 | State_node22
  | State_node23 | State_node24 | State_node25 | State_node3 | State_node4
  | State_node5 | State_node6 | State_node7 | State_node8 | State_node9
  type stack_value =
  | StackValue_Case of ((string list * string))
  | StackValue_CaseList of ( (string list * string) list )
  | StackValue_Grammar of ( (string * string * (string * string * (string list * string) list) list)  )
  | StackValue_Idents of ((string list))
  | StackValue_PreludeSpec of (string)
  | StackValue_Rule of ( (string * string * (string list * string) list) )
  | StackValue_Rules of ( (string * string * (string list * string) list) list )
  | StackValue_S0 of ( (string * string * (string * string * (string list * string) list) list)  )
  | StackValue_TargetSpec of (string)
    | StackValue_Ident of (string)
  | StackValue_Literal of (string)
  | StackValue_None

  type nonterm = Token_Case | Token_CaseList | Token_Grammar | Token_Idents | Token_PreludeSpec | Token_Rule | Token_Rules | Token_S0 | Token_TargetSpec
  type token_type = Term of token | NonTerm of nonterm | TermEof
  type stack = (states * token_type * stack_value) list

  let parse input =
    let pop msg = function
      | [] -> raise (Parse_error msg)
      | hd::tl -> hd, tl in
    let pop_stack a = pop "stack" a in

    let rec _hello = ()

    and goto_Case (state: states) =
              match state with
                | State_node11 -> node13, State_node13
                | State_node13 -> node13, State_node13
                | _ -> raise (Parse_error "couldn't match in goto Case")

    and goto_CaseList (state: states) =
              match state with
                | State_node11 -> node12, State_node12
                | State_node13 -> node14, State_node14
                | _ -> raise (Parse_error "couldn't match in goto CaseList")

    and goto_Grammar (state: states) =
              match state with
                | State_node1 -> node2, State_node2
                | _ -> raise (Parse_error "couldn't match in goto Grammar")

    and goto_Idents (state: states) =
              match state with
                | State_node15 -> node16, State_node16
                | State_node18 -> node19, State_node19
                | _ -> raise (Parse_error "couldn't match in goto Idents")

    and goto_PreludeSpec (state: states) =
              match state with
                | State_node4 -> node5, State_node5
                | _ -> raise (Parse_error "couldn't match in goto PreludeSpec")

    and goto_Rule (state: states) =
              match state with
                | State_node5 -> node7, State_node7
                | State_node7 -> node7, State_node7
                | _ -> raise (Parse_error "couldn't match in goto Rule")

    and goto_Rules (state: states) =
              match state with
                | State_node5 -> node6, State_node6
                | State_node7 -> node8, State_node8
                | _ -> raise (Parse_error "couldn't match in goto Rules")

    and goto_TargetSpec (state: states) =
              match state with
                | State_node1 -> node4, State_node4
                | _ -> raise (Parse_error "couldn't match in goto TargetSpec")

    and node1 (_stack: stack) (input: token list) =
           match input with
             | (Target as _head) :: _input' -> begin
                let _stack = (State_node23, Term _head, StackValue_None) :: _stack in
               node23 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Target` (Target)} in state node1."))
      
    and node10 (_stack: stack) (input: token list) =
           match input with
             | (Literal _value as _head) :: _input' -> begin
                let _stack = (State_node11, Term _head, StackValue_Literal (_value)) :: _stack in
               node11 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Literal` (Literal)} in state node10."))
      
    and node11 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | (Pipe as _head) :: _input' -> begin
                let _stack = (State_node15, Term _head, StackValue_None) :: _stack in
               node15 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident), `Pipe` (Pipe)} in state node11."))
      
    and node12 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v3 = (match typ, tmp with
                | NonTerm Token_CaseList, StackValue_CaseList v -> v
                | _ -> raise (Parse_error "expected token CaseList")) in
                ignore v3;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | Term Colon, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Colon`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Ident _, StackValue_Ident v -> v
                | _ -> raise (Parse_error "expected token `Ident`")) in
                ignore v0;
                let _value = ( (v0, v2, v3) ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Rule before in
                let _stack = (goto_id, NonTerm Token_Rule, StackValue_Rule _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v3 = (match typ, tmp with
                | NonTerm Token_CaseList, StackValue_CaseList v -> v
                | _ -> raise (Parse_error "expected token CaseList")) in
                ignore v3;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | Term Colon, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Colon`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Ident _, StackValue_Ident v -> v
                | _ -> raise (Parse_error "expected token `Ident`")) in
                ignore v0;
                let _value = ( (v0, v2, v3) ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Rule before in
                let _stack = (goto_id, NonTerm Token_Rule, StackValue_Rule _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident)} in state node12."))
      
    and node13 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | (Pipe as _head) :: _input' -> begin
                let _stack = (State_node15, Term _head, StackValue_None) :: _stack in
               node15 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident), `Pipe` (Pipe)} in state node13."))
      
    and node14 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_CaseList, StackValue_CaseList v -> v
                | _ -> raise (Parse_error "expected token CaseList")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | NonTerm Token_Case, StackValue_Case v -> v
                | _ -> raise (Parse_error "expected token Case")) in
                ignore v0;
                let _value = ( v0 :: v1 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_CaseList, StackValue_CaseList v -> v
                | _ -> raise (Parse_error "expected token CaseList")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | NonTerm Token_Case, StackValue_Case v -> v
                | _ -> raise (Parse_error "expected token Case")) in
                ignore v0;
                let _value = ( v0 :: v1 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_CaseList before in
                let _stack = (goto_id, NonTerm Token_CaseList, StackValue_CaseList _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident)} in state node14."))
      
    and node15 (_stack: stack) (input: token list) =
           match input with
             | (Ident _value as _head) :: _input' -> begin
                let _stack = (State_node18, Term _head, StackValue_Ident (_value)) :: _stack in
               node18 _stack _input'
             end
  
             | (Literal _value as _head) :: _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Idents before in
                let _stack = (goto_id, NonTerm Token_Idents, StackValue_Idents _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`Ident` (Ident), `Literal` (Literal)} in state node15."))
      
    and node16 (_stack: stack) (input: token list) =
           match input with
             | (Literal _value as _head) :: _input' -> begin
                let _stack = (State_node17, Term _head, StackValue_Literal (_value)) :: _stack in
               node17 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Literal` (Literal)} in state node16."))
      
    and node17 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_Idents, StackValue_Idents v -> v
                | _ -> raise (Parse_error "expected token Idents")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Pipe, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Pipe`")) in
                ignore v0;
                let _value = ( v1, v2 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Case before in
                let _stack = (goto_id, NonTerm Token_Case, StackValue_Case _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_Idents, StackValue_Idents v -> v
                | _ -> raise (Parse_error "expected token Idents")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Pipe, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Pipe`")) in
                ignore v0;
                let _value = ( v1, v2 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Case before in
                let _stack = (goto_id, NonTerm Token_Case, StackValue_Case _value) :: _stack in
                goto _stack input
             end
  
             | (Pipe as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_Idents, StackValue_Idents v -> v
                | _ -> raise (Parse_error "expected token Idents")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Pipe, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Pipe`")) in
                ignore v0;
                let _value = ( v1, v2 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Case before in
                let _stack = (goto_id, NonTerm Token_Case, StackValue_Case _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident), `Pipe` (Pipe)} in state node17."))
      
    and node18 (_stack: stack) (input: token list) =
           match input with
             | (Ident _value as _head) :: _input' -> begin
                let _stack = (State_node18, Term _head, StackValue_Ident (_value)) :: _stack in
               node18 _stack _input'
             end
  
             | (Literal _value as _head) :: _input' -> begin
                
                let _value = ( [] ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Idents before in
                let _stack = (goto_id, NonTerm Token_Idents, StackValue_Idents _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`Ident` (Ident), `Literal` (Literal)} in state node18."))
      
    and node19 (_stack: stack) (input: token list) =
           match input with
             | (Literal _value as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_Idents, StackValue_Idents v -> v
                | _ -> raise (Parse_error "expected token Idents")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Ident _, StackValue_Ident v -> v
                | _ -> raise (Parse_error "expected token `Ident`")) in
                ignore v0;
                let _value = ( v0 :: v1 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Idents before in
                let _stack = (goto_id, NonTerm Token_Idents, StackValue_Idents _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`Literal` (Literal)} in state node19."))
      
    and node2 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let _stack = (State_node3, TermEof, StackValue_None) :: _stack in
               node3 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($)} in state node2."))
      
    and node20 (_stack: stack) (input: token list) =
           match input with
             | (Equals as _head) :: _input' -> begin
                let _stack = (State_node21, Term _head, StackValue_None) :: _stack in
               node21 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Equals` (Equals)} in state node20."))
      
    and node21 (_stack: stack) (input: token list) =
           match input with
             | (Literal _value as _head) :: _input' -> begin
                let _stack = (State_node22, Term _head, StackValue_Literal (_value)) :: _stack in
               node22 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Literal` (Literal)} in state node21."))
      
    and node22 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | Term Equals, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Equals`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Prelude, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Prelude`")) in
                ignore v0;
                let _value = (v2) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_PreludeSpec before in
                let _stack = (goto_id, NonTerm Token_PreludeSpec, StackValue_PreludeSpec _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Literal _, StackValue_Literal v -> v
                | _ -> raise (Parse_error "expected token `Literal`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | Term Equals, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Equals`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Prelude, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Prelude`")) in
                ignore v0;
                let _value = (v2) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_PreludeSpec before in
                let _stack = (goto_id, NonTerm Token_PreludeSpec, StackValue_PreludeSpec _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident)} in state node22."))
      
    and node23 (_stack: stack) (input: token list) =
           match input with
             | (Equals as _head) :: _input' -> begin
                let _stack = (State_node24, Term _head, StackValue_None) :: _stack in
               node24 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Equals` (Equals)} in state node23."))
      
    and node24 (_stack: stack) (input: token list) =
           match input with
             | (Ident _value as _head) :: _input' -> begin
                let _stack = (State_node25, Term _head, StackValue_Ident (_value)) :: _stack in
               node25 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Ident` (Ident)} in state node24."))
      
    and node25 (_stack: stack) (input: token list) =
           match input with
             | (Prelude as _head) :: _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | Term Ident _, StackValue_Ident v -> v
                | _ -> raise (Parse_error "expected token `Ident`")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | Term Equals, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Equals`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | Term Target, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `Target`")) in
                ignore v0;
                let _value = (v2) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_TargetSpec before in
                let _stack = (goto_id, NonTerm Token_TargetSpec, StackValue_TargetSpec _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`Prelude` (Prelude)} in state node25."))
      
    and node3 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | TermEof, StackValue_None -> ()
                | _ -> raise (Parse_error "expected token `$`")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | NonTerm Token_Grammar, StackValue_Grammar v -> v
                | _ -> raise (Parse_error "expected token Grammar")) in
                ignore v0;
                let _value = (v0) in
                _value
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($)} in state node3."))
      
    and node4 (_stack: stack) (input: token list) =
           match input with
             | (Prelude as _head) :: _input' -> begin
                let _stack = (State_node20, Term _head, StackValue_None) :: _stack in
               node20 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Prelude` (Prelude)} in state node4."))
      
    and node5 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                
                let _value = ([]) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Rules before in
                let _stack = (goto_id, NonTerm Token_Rules, StackValue_Rules _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let _stack = (State_node9, Term _head, StackValue_Ident (_value)) :: _stack in
               node9 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident)} in state node5."))
      
    and node6 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v2 = (match typ, tmp with
                | NonTerm Token_Rules, StackValue_Rules v -> v
                | _ -> raise (Parse_error "expected token Rules")) in
                ignore v2;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_PreludeSpec, StackValue_PreludeSpec v -> v
                | _ -> raise (Parse_error "expected token PreludeSpec")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | NonTerm Token_TargetSpec, StackValue_TargetSpec v -> v
                | _ -> raise (Parse_error "expected token TargetSpec")) in
                ignore v0;
                let _value = ((v0, v1, v2)) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Grammar before in
                let _stack = (goto_id, NonTerm Token_Grammar, StackValue_Grammar _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($)} in state node6."))
      
    and node7 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                
                let _value = ([]) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Rules before in
                let _stack = (goto_id, NonTerm Token_Rules, StackValue_Rules _value) :: _stack in
                goto _stack input
             end
  
             | (Ident _value as _head) :: _input' -> begin
                let _stack = (State_node9, Term _head, StackValue_Ident (_value)) :: _stack in
               node9 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($), `Ident` (Ident)} in state node7."))
      
    and node8 (_stack: stack) (input: token list) =
           match input with
             | [ (* EOF *) ] as _input' -> begin
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v1 = (match typ, tmp with
                | NonTerm Token_Rules, StackValue_Rules v -> v
                | _ -> raise (Parse_error "expected token Rules")) in
                ignore v1;
                let (_, typ, tmp), _stack = pop_stack _stack in
                let v0 = (match typ, tmp with
                | NonTerm Token_Rule, StackValue_Rule v -> v
                | _ -> raise (Parse_error "expected token Rule")) in
                ignore v0;
                let _value = ( v0 :: v1 ) in
                
                let (before, _, _) = List.hd _stack in
                let goto, goto_id = goto_Rules before in
                let _stack = (goto_id, NonTerm Token_Rules, StackValue_Rules _value) :: _stack in
                goto _stack input
             end
  
             | _ -> raise (Parse_error ("expected one of {`$` ($)} in state node8."))
      
    and node9 (_stack: stack) (input: token list) =
           match input with
             | (Colon as _head) :: _input' -> begin
                let _stack = (State_node10, Term _head, StackValue_None) :: _stack in
               node10 _stack _input'
             end
  
             | _ -> raise (Parse_error ("expected one of {`Colon` (Colon)} in state node9."))
      

    in

    node1 [State_node1, NonTerm Token_Case, StackValue_None] input

    