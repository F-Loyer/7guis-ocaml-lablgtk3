# 7GUIS implemented in OCaml / LibGtk3

This project implements the 7 tasks of the 7GUIs
(https://7guis.github.io/7guis/) in OCaml with the library
LablGtk3 (an object oriented Gtk3 binding). The CalendarLib package
is used to for dates handling.

The 7th task (Cells) handles float arithmetics (+, -, *, /) including unary
operators, and the SUM function. Circular references are detected. It is 
composed of multiple files. `cells.ml` the main program which supports the
GUIs aspects, `expr.ml` an abstract spreadsheet. `parser.mly` the parser 
description (with menhir), `lexer.mll` the lexer description (with ocamllex).

`opam install lablgtk3 calendar menhir` is required to compile the programs. 

The different tasks are compiled and executed with:

```
dune exec ./counter.exe
dune exec ./temperature.exe
dune exec ./flightbooker.exe
dune exec ./timer.exe
dune exec ./crud.exe
dune exec ./circledrawer.exe
dune exec ./circledrawer2.exe
dune exec ./cells.exe
```

LablGtk3 is an object oriented binding to Gtk3. However, some functional
traits are involved. (Callback)

A circledrawer alternative is proposed which uses only a functional (immuable) structure (list) instead of
Hashtbl and makes undo/redo easier. This saves a few lines.
