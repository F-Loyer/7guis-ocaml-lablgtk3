let (_locale:string) = GMain.init ()

let w = GWindow.window ~title:"TempConv" ()
let hbox = GPack.hbox ~border_width:10 ~packing:w#add ()
let celsius_entry = GEdit.entry ~packing:(hbox#pack ~padding:4) ()
let _celsius_label = GMisc.label ~justify:`LEFT ~xalign:0.95 ~packing:(hbox#pack ~padding:4) ~text:"Celsius" ()
let farenheit_entry = GEdit.entry ~packing:(hbox#pack ~padding:4) ()
let _farenheit_label = GMisc.label ~justify:`LEFT ~xalign:0.95 ~packing:(hbox#pack ~padding:4) ~text:"Farenheit" ()

let change_allowed = ref true (* when false, this avoid that when changing °F because of a °C change, then °C changes back*)

let _ = celsius_entry#connect#changed ~callback:(function _ ->
  if !change_allowed then
  (try 
    change_allowed := false;
    let celsius = float_of_string (celsius_entry#text) in
    let farenheit = celsius *. 9. /. 5. +. 32. in
    farenheit_entry#set_text (string_of_float farenheit)
  with _ -> ());
  change_allowed := true)
let _ = farenheit_entry#connect#changed ~callback:(function _ ->
  if !change_allowed then
  (try 
    change_allowed := false;
    let farenheit = float_of_string (farenheit_entry#text) in
    let celsius = (farenheit -. 32.) *. 5. /. 9. in
    celsius_entry#set_text (string_of_float celsius)
  with _ -> ());
  change_allowed := true)
    
let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()