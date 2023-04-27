# 7GUIS implemented in OCaml / LibGtk3

This project implements the 6 first tasks of the 7GUIs
(https://eugenkiss.github.io/7guis/tasks) in OCaml with the library
LablGtk3 (an object oriented Gtk3 binding). The CalendarLib package
is used to for dates handling.

The 7th task (Cells) won't be developped. Gtk doesn't provide a 
real table widget. The list\_view is closed to this widget, but doesn't
permit the selection of a unique cell.

The different task are compiled and executed with

```
dune exec ./counter.exe
dune exec ./temperature.exe
dune exec ./flightbooker.exe
dune exec ./timer.exe
dune exec ./crud.exe
dune exec ./circledrawer.exe
```
