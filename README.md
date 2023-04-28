# 7GUIS implemented in OCaml / LibGtk3

This project implements the 6 first tasks of the 7GUIs
(https://eugenkiss.github.io/7guis/tasks) in OCaml with the library
LablGtk3 (an object oriented Gtk3 binding). The CalendarLib package
is used to for dates handling.

The 7th task (Cells) is in an early stage. (Float arithmetic, circular
reference handling). It is composed of multiple files. `cells.ml` the main
program which supports the GUIs aspects, `expr.ml` an abstract
spreadsheet. `parser.mly` the parser description (with menhir), `lexer.mll`
the lexer description (with ocamllex).

The different task are compiled and executed with

```
dune exec ./counter.exe
dune exec ./temperature.exe
dune exec ./flightbooker.exe
dune exec ./timer.exe
dune exec ./crud.exe
dune exec ./circledrawer.exe
dune exec ./cells.exe
```

LablGtk3 is an object oriented binding to Gtk3. However, some functional
traits are involved. (Callback)

A circledrawer alternative is proposed which use only functional (immuable) structure (list) instead of
Hashtbl to make undo/redo easier. This saves a few lines.
