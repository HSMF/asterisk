type spec =
  { spec_target : string
  ; spec_prelude : string
  ; spec_tokens : (string * string option) list
  ; spec_rules : rule list
  ; spec_configs : (string * string) list
  }

and rule = string * string * (string list * string) list
