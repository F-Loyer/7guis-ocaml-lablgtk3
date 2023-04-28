let (_locale:string) = GMain.init ()

let t = ref 0.
let timer = ref None

let w = GWindow.window ~title:"Timer" ()
let vbox = GPack.vbox ~border_width:10 ~packing:w#add ()
let hbox1 = GPack.hbox ~border_width:10 ~packing:vbox#add ()
let _label_elapsed = GMisc.label ~text:"Elapsed time:" ~packing:(hbox1#pack ~padding:4) ()
let progress_bar = GRange.progress_bar ~pulse_step:0.01 ~packing:(hbox1#pack ~padding:4) ()
let label_elapsed = GMisc.label ~text:"0.0s" ~packing:(vbox#pack ~padding:4) ()
let hbox2 = GPack.hbox ~border_width:10 ~packing:vbox#add ()
let _label_duration = GMisc.label ~text:"Duration:" ~packing:(hbox2#pack ~padding:4) ()
let slider_adj = GData.adjustment ~lower:0. ~value:60. ~upper:20. ()
let slider = GRange.scale `HORIZONTAL ~adjustment:slider_adj ~packing:(hbox2#pack ~padding:4 ~expand:true) () 
let button = GButton.button ~label:("Reset") ~packing:(vbox#pack ~padding:4) ()

let update () =
  let duration = slider#adjustment#value in
  if !t > duration then 
  begin
     t:=duration;
     match !timer with 
     | Some t -> GMain.Timeout.remove t; timer:=None
     | _ -> (); 
  end;
  label_elapsed#set_text (Printf.sprintf "%.1f" !t);
  progress_bar#set_fraction (!t/.duration)

let start_timer () = timer := 
  Some (GMain.Timeout.add ~ms:100 ~callback:(fun () -> 
    t := !t +. 0.1; update ();
    true))

let reset () = 
  t:=0.; 
  label_elapsed#set_text "0.0s"; 
  progress_bar#set_fraction 0.;
  update ();
  if !timer = None then start_timer ()

let _ = button#connect#clicked ~callback:(fun () -> reset ())
let _ = slider#connect#value_changed ~callback:(fun () -> update ())
let () = reset ()

let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()