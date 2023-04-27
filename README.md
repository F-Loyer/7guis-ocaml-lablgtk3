# 7GUIS implemented in OCaml / LibGtk3

The 6 first challenges are implemented with in OCaml with the library
LablGtk3 (an object oriented Gtk3 binding). The CalendarLib package
is used to for dates handling.

The 7th challenge (Cells) won't be developped. Gtk doesn't provide a 
real table widget. The list\_view is closed to this widget, but doesn't
permit the selection of a unique cell.

The different challenges are executed with

```
dune exec ./counter.exe
dune exec ./temperature.exe
dune exec ./flightbooker.exe
dune exec ./timer.exe
dune exec ./crud.exe
dune exec ./circledrawer.exe
```
