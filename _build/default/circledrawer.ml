let _ = GMain.init ()

let list_of_circles = ref [50., 50., 20.;
                         75., 50., 20.]
let selected_circle = ref None
(*
type change = Create of float*float*float
            | Change of float*float*float*float
*)
let change_diameter x y radius =
  list_of_circles := List.map (function c ->
    match c with 
      | (x',y',_) when x=x' && y=y' -> (x,y,radius)
      | _ -> c) !list_of_circles;
  match !selected_circle with
  | Some (x',y',_,d') when x=x' && y=y' -> selected_circle := Some (x,y,radius,d')
  | _ -> ()

let w = GWindow.window ~title:"Circle Drawer" ()
let table = GPack.table ~row_spacings:4 ~col_spacings:4 ~rows:2 ~columns:2 ~homogeneous:false ~show:true ~packing:w#add ()

let button_undo = GButton.button ~label:("Undo") ~packing:(table#attach ~left:0 ~top:0) ()
let button_redo = GButton.button ~label:("Redo") ~packing:(table#attach ~left:1 ~top:0) ()
(* GMisc.drawing_area can't be set with a set of dimension. We just make the room for it*)
let _ = GMisc.label ~height:200 ~width:200 ~justify:`LEFT ~text:"" ~packing:(table#attach ~left:0 ~right:2 ~top:1 ~expand:`BOTH) ()
let drawing = GMisc.drawing_area ~packing:(table#attach ~left:0 ~right:2~top:1) ()

let draw widget cr =
  ignore widget;
  (match !selected_circle with
   | Some (x,y,r,_) ->
        Cairo.set_source_rgb cr 0.5 0.5 0.5;
        Cairo.arc cr x y ~r ~a1:0. ~a2:(2.*.Float.pi);
        Cairo.fill cr
   | None -> ());
  (* Cairo.paint cr; *)
    List.iter (function (x,y,r) ->
     Cairo.arc cr x y ~r ~a1:0. ~a2:(2.*.Float.pi);
     Cairo.set_source_rgb cr 0. 0. 0.;
     Cairo.stroke cr
  ) !list_of_circles;
  true
(* drawing area/GObj.widget_full..widget_signals/gtkobj_signals *)
(* entry..connect/editable_signal/GObj.widget_signals
   entry..event/event_ops/*)

let calc_selection mouse_x mouse_y =
  selected_circle :=  None;
  List.iter (function (x,y,r)->
      let mouse_r = Float.sqrt((x-.mouse_x)**2.+.(y-.mouse_y)**2.) in
      if mouse_r < r then
        let d = r-.mouse_r in
        match !selected_circle with
        | Some (_,_,_,d') -> if d<d' then selected_circle := Some (x,y,r,d)
        | None -> selected_circle := Some (x,y,r,d)
        ) !list_of_circles

let _ = drawing#misc#connect#draw ~callback:(draw drawing)
let _ = drawing#event#add [`BUTTON_PRESS]
let _ = drawing#event#connect#button_press ~callback:(fun ev ->
  let mouse_x = GdkEvent.Button.x ev in
  let mouse_y = GdkEvent.Button.y ev in
  let button = GdkEvent.Button.button ev in
  if button = 3 then
    match !selected_circle with 
    | Some (x,y,r,_) when Float.sqrt((x-.mouse_x)**2.+.(y-.mouse_y)**2.) < r
      ->
        let menu = GMenu.menu () in
        let menuitem = GMenu.menu_item ~label:"Adjust diameter..." ~use_mnemonic:true ~packing:menu#append () in
        ignore @@ menuitem#connect#activate ~callback:(fun _ -> match !selected_circle with 
                                                     | Some (x,y,_,_) -> change_diameter x y 50. ; drawing#misc#queue_draw ()
                                                     | None -> ());
        menu#popup ~button:0 ~time:0l
    | _ -> ()
  else
    (calc_selection mouse_x mouse_y;
    if !selected_circle = None then
      list_of_circles := (mouse_x,mouse_y,10.):: !list_of_circles;
    drawing#misc#queue_draw ());
    true)
let () = ignore (button_undo, button_redo)
let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()