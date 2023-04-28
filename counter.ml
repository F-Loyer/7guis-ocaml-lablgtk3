let (_locale:string) = GMain.init ()

let counter = ref 0

let w = GWindow.window ~title:"Counter" ()
let hbox = GPack.hbox ~border_width:10 ~packing:w#add ()
let label = GMisc.label ~packing:(hbox#pack ~padding:4) ~text:"0" ()
let button = GButton.button ~label:("Count") ~packing:(hbox#pack ~padding:4) ()

let _ = button#connect#clicked ~callback:(function () ->
  incr counter;
  label#set_text (Int.to_string !counter))

let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()