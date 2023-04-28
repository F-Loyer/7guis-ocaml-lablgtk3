let (_locale:string) = GMain.init ()

let cols = new GTree.column_list

let colnames = [|"A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";
                  "N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"|]

let indice_col = cols#add Gobject.Data.string
let table_cols = Array.map (fun _-> cols#add Gobject.Data.string) colnames
let model = GTree.list_store cols

let () = for i = 0 to 99
do
  let row = model#append () in
    model#set ~row ~column:indice_col (Int.to_string i);
done

let w = GWindow.window ~border_width:10 ~title:"Cells" ()
(* let table = GPack.table ~row_spacings:4 ~col_spacings:4 ~rows:5 ~columns:4 ~homogeneous:false ~show:true ~packing:w#add () *)
let scrolled_window = GBin.scrolled_window ~width:400 ~height:300 ~packing:w#add ()
let listview = GTree.view ~model:model ~packing:scrolled_window#add ()
let () = listview#set_enable_grid_lines `BOTH
let indice_colview = GTree.view_column ~renderer:(GTree.cell_renderer_text [`WRAP_WIDTH 150;`WRAP_MODE `WORD_CHAR], ["text",indice_col]) ()
let (_indice_column_num:int) = listview#append_column indice_colview

let cell_renderers = Array.map (fun gtk_col ->
    GTree.cell_renderer_text [`WRAP_WIDTH 150;`WRAP_MODE `WORD_CHAR;`EDITABLE true], ["text",gtk_col]) table_cols

let colviews = Array.mapi (fun i renderer -> 
                             GTree.view_column ~title:colnames.(i) ~renderer ()) cell_renderers
let () = Array.iter (fun colview -> ignore @@ listview#append_column colview) colviews

let selection = listview#selection 
let () = selection#set_mode `NONE

let search_col s =
  Char.code (s.[0]) - (Char.code 'A')

let _ = listview#connect#row_activated ~callback:(fun path view_col ->
  if String.length view_col#title = 1 then
  begin
    let col = search_col view_col#title in
    let row = (GTree.Path.get_indices path).(0) in
    model#set ~row:(model#get_iter path) ~column:table_cols.(col) (Expr.get_formulae (col,row));
    listview#set_cursor ~edit:true path view_col
  end)

let change_cell (col,row) str =
  let path = GTree.Path.create [row] in
  let column = table_cols.(col) in
  model#set ~row:(model#get_iter path) ~column str
let () = Expr.callback := change_cell

let () = for col=0 to 25 do
  ignore @@ (fst cell_renderers.(col))#connect#edited ~callback:(fun path str -> 
    let row = (GTree.Path.get_indices path).(0) in
    try
      if (String.length str) >0 then
        if str.[0] = '=' ||  str.[0] = '+' || str.[0] = '-' ||   str.[0] >='0' && str.[0] <='9' then
          let expr = Parser.prog Lexer.read (Lexing.from_string str) in
          Expr.add_formulae (col,row) str expr
        else
          Expr.add_formulae (col,row) str (Expr.String str)
        else
          Expr.add_formulae (col,row) str Expr.Null
    with
    _ ->
      Expr.add_formulae (col,row) str (Expr.Invalid "Parse error");
       model#set ~row:(model#get_iter path) ~column:table_cols.(col) "Invalid")
  done
let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()
