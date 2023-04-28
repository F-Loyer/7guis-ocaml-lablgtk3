let (_locale:string) = GMain.init ()

let cols = new GTree.column_list
let name_col = cols#add Gobject.Data.string
let surname_col = cols#add Gobject.Data.string
let display_col = cols#add Gobject.Data.string
let model = GTree.list_store cols
let model_filtered = GTree.model_filter model

let display name surname =
  name ^ ", " ^ surname

(*
let () =List.iter (fun (name,surname) ->
  let row = model#append () in
    model#set ~row ~column:name_col name;
    model#set ~row ~column:surname_col surname;
    model#set ~row ~column:display_col (display name surname))
["A","A";"BA","BA";"BB","BB"]
*)

let w = GWindow.window ~border_width:10 ~title:"CRUD" ()
let table = GPack.table ~row_spacings:4 ~col_spacings:4 ~rows:5 ~columns:4 ~homogeneous:false ~show:true ~packing:w#add ()
let _label_prefix = GMisc.label ~justify:`LEFT ~text:"Filter prefix:" ~packing:(table#attach ~left:0 ~top:0) ()
let prefix_entry = GEdit.entry ~packing:(table#attach ~left:1 ~top:0) ()
let _label_name = GMisc.label ~xalign:0. ~justify:`LEFT ~text:"Name:" ~packing:(table#attach ~left:2 ~top:1  ) ()
let name_entry = GEdit.entry ~packing:(table#attach ~left:3 ~top:1) ()
let _label_surname = GMisc.label ~xalign:0. ~justify:`LEFT ~text:"Surname:" ~packing:(table#attach ~left:2 ~top:2) ()
let surname_entry = GEdit.entry ~packing:(table#attach ~left:3 ~top:2) ()
let _label_filling = GMisc.label ~justify:`LEFT ~text:"" ~packing:(table#attach ~left:2 ~top:3 ~expand:`BOTH) ()
let scrolled_window = GBin.scrolled_window ~width:200 ~height:150 ~packing:(table#attach ~left:0 ~right:2 ~top:1 ~bottom:4 ~expand:`BOTH) ()
let listview = GTree.view ~headers_visible:false ~model:model_filtered ~packing:scrolled_window#add ()
let colview = GTree.view_column ~renderer:(GTree.cell_renderer_text [`WRAP_WIDTH 150;`WRAP_MODE `WORD_CHAR], ["text",display_col]) ()
let _indice_colview = listview#append_column colview
let selection = listview#selection 

let hbox = GPack.hbox ~border_width:10 ~packing:(table#attach~top:4 ~left:0 ~right:4) ()
let create = GButton.button ~label:("Create") ~packing:(hbox#pack ~padding:4) ()
let update = GButton.button ~label:("Udpate") ~packing:(hbox#pack ~padding:4) ()
let () = update#set_sensitive false
let delete = GButton.button ~label:("Delete") ~packing:(hbox#pack ~padding:4) ()
let () = delete#set_sensitive false

let _ = selection#connect#changed ~callback:(fun () ->
    let ok = selection#get_selected_rows <> [] in
      update#set_sensitive ok;
      delete#set_sensitive ok;
    match selection#get_selected_rows with
    | path::_ ->
        let row = model_filtered#get_iter path in
        let row' = model_filtered#convert_iter_to_child_iter row in
          name_entry#set_text (model#get ~row:row' ~column:name_col);
          surname_entry#set_text (model#get ~row:row' ~column:surname_col)
     | _ -> ())
        
let () = model_filtered#set_visible_func (fun model row ->
  let prefix = prefix_entry#text in
  if prefix = "" then true
  else let surname = model#get ~row ~column:surname_col in String.starts_with ~prefix surname)

let _ = prefix_entry#connect#changed ~callback:(fun () -> model_filtered#refilter ())

let _ = create#connect#clicked ~callback:(fun () ->
  let name = name_entry#text in
  let surname = surname_entry#text in
  let row = model#append () in
    model#set ~row ~column:name_col name;
    model#set ~row ~column:surname_col surname;
    model#set ~row ~column:display_col (display name surname))

let _ = update#connect#clicked ~callback:(fun () ->
  let name = name_entry#text in
  let surname = surname_entry#text in
  List.iter (fun path ->
    let row = model_filtered#get_iter path in
    let row' = model_filtered#convert_iter_to_child_iter row in
      model#set ~row:row' ~column:name_col name;
      model#set ~row:row' ~column:surname_col surname;
      model#set ~row:row' ~column:display_col (display name surname)
    ) selection#get_selected_rows)
   
let _ = delete#connect#clicked ~callback:(fun () -> 
  List.iter (fun path ->
    let row = model_filtered#get_iter path in
    let row' = model_filtered#convert_iter_to_child_iter row in
      ignore @@ model#remove row') selection#get_selected_rows)

let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()
