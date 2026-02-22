# Erlisp

Lisp interpreter written in Erlang.

## Run

First, compile the source code to *BEAM* byte code: 
```bash
erlc erlisp.erl
```

Then run 
```bash
erl -noshell -s erlisp main -s init stop
```
to start the REPL.
Or, interpret the `.lisp` Erlisp source file 
```bash
erl -noshell -s erlisp main [-extra] [file.lisp] -s init stop
```
You can also run in the Erlang's REPL: 
```bash
erl
1> c(erlisp).
2> erlisp:main().
```

## Examples

[here](./examples) to see some examples

