type typ =
  | TVar of string
  | TVoid

and stmt =
  | Decl of string * exp
  | Ret of exp option

and exp =
  | EVar of string
  | EBop of bop * exp * exp

and bop =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv

and fdecl = string * (string * typ) list * stmt list

let sp = Printf.sprintf
let sl map sep lst = lst |> List.map map |> String.concat sep

let rec typ_to_string = function
  | TVar s -> s
  | TVoid -> "void"


let rec indent depth s = if depth > 0 then "    " ^ indent (depth-1) s else s

and stmt_to_string ?(il = 0) stmt =
  (match stmt with
   | Decl (s, e) -> sp "%s := %s;" s (exp_to_string e)
   | Ret None -> "return;"
   | Ret (Some v) -> sp "return %s;" (exp_to_string v))
  |> indent il
  |> sp "%s\n"


and exp_to_string = function
  | EVar s -> s
  | EBop (b, l, r) ->
    sp "(%s %s %s)" (exp_to_string l) (bop_to_string b) (exp_to_string r)


and bop_to_string = function
  | OpPlus -> "+"
  | OpMinus -> "-"
  | OpTimes -> "*"
  | OpDiv -> "/"


and block_to_string ?(il = 0) stmts =
  "{\n" ^ sl (stmt_to_string ~il:(il + 1)) "\n" stmts ^ "}"


and arg_to_string (name, typ) = sp "%s: %s" name (typ_to_string typ)

and fdecl_to_string (name, args, body) =
  sp "fn %s(%s) %s" name (sl arg_to_string ", " args) (block_to_string body)
