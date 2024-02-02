open Printf
open Datastructures
open! Fun
open! Util
open Generator

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

let s0 = "S0"

let write_file filename s =
  let f = open_out filename in
  output_string f s;
  close_out f


let make_artifact_dir name =
  if Sys.file_exists name && Sys.is_directory name then () else Sys.mkdir name 0o755


let () =
  let artifact_dir = ref "output" in
  let emit_html = ref false in
  let emit_dot = ref false in
  let emit_js = ref false in
  let emit_java = ref false in
  let print_table = ref false in
  let grammar_file = ref None in
  let speclist =
    [ ( "-O"
      , Arg.Set_string artifact_dir
      , "sets artifact directory (where the files are produced to)" )
    ; ( "-emit-js"
      , Arg.Set emit_js
      , "emit javascript code, put into <artifact_dir>/parser.js" )
    ; ( "-emit-java"
      , Arg.Set emit_java
      , "emit java code, put into <artifact_dir>/Parser.java" )
    ; ( "-emit-html"
      , Arg.Set emit_html
      , "emit java code, put into <artifact_dir>/table.html" )
    ; "-print-table", Arg.Set print_table, "print the table to stdout"
    ; "-emit-dot", Arg.Set emit_dot, "emit java code, put into <artifact_dir>/dfa.dot"
    ]
  in
  Arg.parse speclist (fun x -> grammar_file := Some x) "asterisk [OPTIONS]\n";
  make_artifact_dir !artifact_dir;
  let outfile = sp "%s/%s" !artifact_dir in
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
         grammar
         table
  end;
  ()
