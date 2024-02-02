# Asterisk - LR(1) Parser Generator

Not the most efficient parser generator but it's cool, I guess<sup>TM</sup>

## Development

To run the generator, execute this:

```sh
dune exec -- ./main.exe
```

This will generate several files, TODO: only generate on demand

- `table.html`
- `hey.dot`
- `parser.js`
- `parser.java`

To generate a graph of the DFA, `graphviz` is required to be installed:

```sh
dot -Tsvg -{G,E,N}fontname=monospace -O hey.dot
```



## TODO

- actually usable cli interface
- custom grammars without recompiling the generator :D
  - grammar file format
- support more languages
- support N languages by establishing some table IR and emitting that ?
- lexer generator
