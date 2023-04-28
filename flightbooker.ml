let (_locale:string) = GMain.init ()

let w = GWindow.window ~title:"Book Flight" ()
let vbox = GPack.vbox ~border_width:10 ~packing:w#add ()
let (combo, _) = GEdit.combo_box_text ~strings:[ "one-way flight"; "return flight" ] ~active:0 ~packing:(vbox#pack ~padding:4) ()
let flight1 = GEdit.entry ~packing:(vbox#pack ~padding:4) ()
let flight2 = GEdit.entry ~packing:(vbox#pack ~padding:4) ()
let () = flight2#set_sensitive false
let button = GButton.button ~label:("Book") ~packing:(vbox#pack ~padding:4) ()

let parse_date s =
  try
    Scanf.sscanf s "%d.%d.%d" (fun d m y -> 
      let date = CalendarLib.Calendar.Date.lmake ~year:y ~month:m ~day:d () in
      let date_str = CalendarLib.Calendar.Date.( 
          Printf.sprintf "%02d.%02d.%04d" (day_of_month date) (int_of_month @@ month date) (year date)) in
      if date_str = s then
        Some date
      else
        None
      )
  with _ -> None

let verif () =
  let return_flight = combo#active = 1 in
  let a = parse_date (flight1#text) in
  let b = parse_date (flight2#text) in
  if a = None then flight1#misc#modify_bg [`NORMAL, `NAME "red"] else flight1#misc#modify_bg [`NORMAL, `WHITE];
  if b = None && return_flight then flight2#misc#modify_bg [`NORMAL, `NAME "red"] else flight2#misc#modify_bg [`NORMAL, `WHITE];
  flight2#set_sensitive return_flight;
  match return_flight, a, b with
  | false, Some _, _ -> button#set_sensitive true
  | true, Some d1, Some d2 -> button#set_sensitive ((CalendarLib.Calendar.Date.compare d1 d2) <> 1)
  | _, _, _ -> button#set_sensitive false

let () = verif ()
let _ = flight1#connect#changed ~callback:(fun () -> verif())
let _ = flight2#connect#changed ~callback:(fun () -> verif())
let _ = combo#connect#changed ~callback:(fun () -> verif())
let _ = button#connect#clicked ~callback:(fun () -> 
  let message = match combo#active with
   | 0 -> "You have booked a one-way flight on " ^ flight1#text
   | _ -> "You have booked a return flight on " ^ flight1#text ^ " and " ^ flight2#text
 in let dialog = GWindow.message_dialog
    ~message_type:`INFO
    ~buttons:GWindow.Buttons.ok
    ~message:message () in 
    let _answer = dialog#run () in
      dialog#destroy ())

let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()