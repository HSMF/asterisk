open Printf

let inspect printer lst =
  List.iter printer lst;
  lst


let () =
  let prog =
    try Sys.argv.(1) with
    | Invalid_argument _ ->
      eprintf "please supply a filename\n";
      exit ~-1
  in
  let f = open_in prog in
  let cont = In_channel.input_all f in
  close_in f;
  try
    cont
    |> Lex.lex
    |> inspect (fun tok ->
      printf "%s\n" (Lex.string_of_token tok);
      flush stdout)
    |> Grammar.parse
    |> List.map Ast.fdecl_to_string
    |> String.concat "\n\n"
    |> print_endline
  with
  | Grammar.Parse_error (Grammar.ErrUnexpectedToken (expected, state, input)) -> begin
    eprintf
      "expected one of: %s in state %s.\nRemaining input is: %s\n"
      (Ast.sl Fun.id ", " expected)
      state
      (Ast.sl Lex.string_of_token "," input);
    exit ~-1
  end
