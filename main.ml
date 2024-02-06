open Printf
open Lib.Datastructures
open! Lib
open! Fun
open! Lib.Util
open Lib.Generator

(*
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
  ] *)

(*
   let grammar =
   [ "S0", [ NonTerm "A"; Term "$" ], "v0"
  ; "A", [ NonTerm "A"; Term "and"; NonTerm "B" ], "Ast.And(v0, v2)"
  ; "A", [ NonTerm "B" ], "v0"
  ; "B", [ NonTerm "B"; Term "or"; NonTerm "C" ], "Ast.Or(v0, v2)"
  ; "B", [ NonTerm "C" ], "v0"
  ; "C", [ Term "("; NonTerm "A"; Term ")" ], "v1"
  ; "C", [ Term "v"; Term "="; Term "v" ], "Ast.Eq(v0, v2)"
  ]
*)

let grammar =
  mk_grammar ("A", [ "A", [ Term "v"; NonTerm "A" ], "(v0 :: v1)"; "A", [], "[]" ])


let s0 = "S0"

let write_file filename s =
  let f = open_out filename in
  output_string f s;
  close_out f


let make_artifact_dir =
  let did = ref false in
  fun name ->
    if not !did
    then begin
      if Sys.file_exists name && Sys.is_directory name
      then ()
      else begin
        Sys.mkdir name 0o755;
        did := true
      end
    end


let emit_html = ref false
let emit_dot = ref false
let output_file = ref None

let gen_from_file outfile filename : unit =
  let f = open_in filename in
  let contents = In_channel.input_all f in
  close_in f;
  let lang, prelude, non_term_types, token_associated_type, grammar =
    Spec.Spec_parser.parse_spec contents
  in
  (* print_endline *)
  (* @@ "parsed grammar as\n" *)
  (* ^ sl *)
  (*     (fun (rule, exp, code) -> sp "%s -> %s {%s}" rule (sl string_of_token " " exp) code) *)
  (*     "\n" *)
  (*     grammar; *)
  let graph = make_graph grammar (initial grammar s0) in
  if !emit_dot
  then begin
    write_file (outfile "dfa.dot") @@ graph_to_string graph
  end;
  let table = table_of_graph graph in
  if !emit_html
  then begin
    write_file (outfile "table.html")
    @@ Frontends.Javascript.html_of_table grammar [] table
  end;
  let actual_output ext =
    match !output_file with
    | Some x -> x
    | None -> outfile ("parser." ^ ext)
  in
  (match lang with
   | "js" | "javascript" -> failwith "todo"
   | "java" -> failwith "todo"
   | "ocaml" -> begin
     write_file (actual_output "ml")
     @@ Frontends.Ocaml.ocaml_of_table
          ~prelude
          ~token_id:id
          ~non_term_types
          ~token_type:"token"
          ~token_associated_type
          ~toplevel_rule:"S0"
          grammar
          table
   end
   | x -> failwith ("unsupported language " ^ x));
  ()


let () =
  let artifact_dir = ref "output" in
  let emit_js = ref false in
  let emit_java = ref false in
  let emit_ocaml = ref false in
  let print_table = ref false in
  let grammar_file = ref None in
  let speclist =
    [ ( "-O"
      , Arg.Set_string artifact_dir
      , "sets artifact directory (where the files are produced to)" )
    ; ( "-o"
      , Arg.String (fun x -> output_file := Some x)
      , "sets artifact directory (where the files are produced to)" )
    ; ( "-emit-js"
      , Arg.Set emit_js
      , "emit javascript code, put into <artifact_dir>/parser.js" )
    ; ( "-emit-java"
      , Arg.Set emit_java
      , "emit java code, put into <artifact_dir>/Parser.java" )
    ; ( "-emit-ocaml"
      , Arg.Set emit_ocaml
      , "emit ocaml code, put into <artifact_dir>/parser.ml" )
    ; ( "-emit-html"
      , Arg.Set emit_html
      , "emit java code, put into <artifact_dir>/table.html" )
    ; "-print-table", Arg.Set print_table, "print the table to stdout"
    ; "-emit-dot", Arg.Set emit_dot, "emit java code, put into <artifact_dir>/dfa.dot"
    ]
  in
  let grammar = Spec.Spec_parser.own_grammar in
  Arg.parse speclist (fun x -> grammar_file := Some x) "asterisk [OPTIONS]\n";
  let outfile name =
    make_artifact_dir !artifact_dir;
    sp "%s/%s" !artifact_dir name
  in
  begin
    match !grammar_file with
    | Some f ->
      gen_from_file outfile f;
      exit 0
    | None -> ()
  end;
  let graph = make_graph grammar (initial grammar s0) in
  let conflicts = conflicts graph in
  printf
    "conflicts %s\n"
    (sl
       (fun (state, conf) -> sp "%s in state %s" (string_of_conflict conf) state)
       ", "
       conflicts);
  if !emit_dot
  then begin
    write_file (outfile "dfa.dot") @@ graph_to_string graph
  end;
  let table = table_of_graph graph in
  if !print_table
  then begin
    printf "generated a table with %d states\n" (UidM.cardinal table);
    printf "%s\n\n" (string_of_table ~header_action:(fun x -> sp "`%s`" x) table)
  end;
  if !emit_html
  then begin
    write_file (outfile "table.html")
    @@ Frontends.Javascript.html_of_table
         grammar
         [ "{type: 'v', value: 'hello'}"
         ; "{type: '='}"
         ; "{type: 'v', value: 'world'}"
         ; "{type: '$'}"
         ]
         table
  end;
  if !emit_js
  then begin
    write_file (outfile "parser.js") @@ Frontends.Javascript.js_of_table grammar table
  end;
  let token_id x =
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
      ]
  in
  if !emit_java
  then begin
    write_file (outfile "Parser.java")
    @@ Frontends.Java.java_of_table
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
         ~token_id
         grammar
         table
  end;
  if !emit_ocaml
  then begin
    print_endline
    @@ "parsed grammar as\n"
    ^ sl
        (fun (rule, exp, code) ->
          sp "%s -> %s {%s}" rule (sl string_of_token " " exp) code)
        "\n"
        grammar;
    write_file (outfile ".parser.ml")
    @@ Frontends.Ocaml.ocaml_of_table
         ~prelude:"open Lex"
         ~token_id:id
         ~non_term_types:(fun x ->
           try
             List.assoc
               x
               [ ( "Grammar"
                 , " (string * string * (string * string * (string list * string) list) \
                    list)  " )
               ; ( "S0"
                 , " (string * string * (string * string * (string list * string) list) \
                    list)  " )
               ; "TargetSpec", "string"
               ; "PreludeSpec", "string"
               ; "Rules", " (string * string * (string list * string) list) list "
               ; "Rule", " (string * string * (string list * string) list) "
               ; "CaseList", " (string list * string) list "
               ; "Case", "(string list * string)"
               ; "Idents", "(string list)"
               ]
           with
           | Not_found -> failwith x)
         ~token_type:"token"
         ~token_associated_type:(fun x ->
           List.assoc_opt x [ "Literal", "string"; "Ident", "string" ])
         ~toplevel_rule:"S0"
         grammar
         table
  end;
  ()
