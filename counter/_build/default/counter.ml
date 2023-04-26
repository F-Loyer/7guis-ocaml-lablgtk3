let counter = ref 0

let _ = GMain.init ()

let w = GWindow.window ()
let table = GPack.table ~rows:1 ~columns:2 ~homogeneous:false ~show:true ~packing:w#add()
let label = GMisc.label ~justify:`LEFT ~xalign:0.95 ~packing:(table#attach ~top:0 ~left:0 ~expand:`BOTH) ~text:"0" ()

let button = GButton.button ~label:("Count") ~packing:(table#attach ~top:0 ~left:1 ~expand:`BOTH) ()

let _ = button#connect#clicked ~callback:(function _ ->
  incr counter;
  label#set_text (Int.to_string !counter))

let _ =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()
