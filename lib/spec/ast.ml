type spec =
  { spec_rules : rule list
  ; spec_configs : (string * string) list
  }

and rule = string * string * (string list * string) list
