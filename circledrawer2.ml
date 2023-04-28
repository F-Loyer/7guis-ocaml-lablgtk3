let (_locale:string) = GMain.init ()

type circle = { x:float; y: float; r: float}

let redo_list = ref []
let undo_list = ref []

let circles = ref []
let selected_circle = ref None

let w = GWindow.window ~title:"Circle Drawer" ()
let table = GPack.table ~row_spacings:4 ~col_spacings:4 ~rows:2 ~columns:2 ~homogeneous:false ~show:true ~packing:w#add ()

let button_undo = GButton.button ~label:("Undo") ~packing:(table#attach ~left:0 ~top:0) ()
let button_redo = GButton.button ~label:("Redo") ~packing:(table#attach ~left:1 ~top:0) ()
let drawing = GMisc.drawing_area ~packing:(table#attach ~left:0 ~right:2~top:1) ()
let () = drawing#misc#set_size_request ~width:200 ~height:200 ()

let draw widget cr =
  ignore widget;
  Option.iter (fun circle ->
     Cairo.set_source_rgb cr 0.5 0.5 0.5;
     Cairo.arc cr circle.x circle.y ~r:circle.r ~a1:0. ~a2:(2.*.Float.pi);
     Cairo.fill cr
  ) !selected_circle;
  List.iter (fun circle ->
     Cairo.arc cr circle.x circle.y ~r:circle.r ~a1:0. ~a2:(2.*.Float.pi);
     Cairo.set_source_rgb cr 0. 0. 0.;
     Cairo.stroke cr
  ) !circles;
  true

let _ = drawing#misc#connect#draw ~callback:(draw drawing)

let check_undo () =
  button_undo#set_sensitive (!undo_list <> []) ;
  button_redo#set_sensitive (!redo_list <> [])

let () = check_undo ()

let _ = button_undo#connect#clicked ~callback:(fun () ->
  begin
    match !undo_list with 
    | hd::undo_list' -> undo_list:=undo_list'; redo_list:=!circles::!redo_list; circles:=hd
    | [] -> ()
  end;
  selected_circle := None;
  check_undo();
  drawing#misc#queue_draw ())

let _ = button_redo#connect#clicked ~callback:(fun () ->
  begin
    match !redo_list with 
    | hd::redo_list' -> redo_list:=redo_list'; undo_list:=!circles::!undo_list; circles:=hd
    | [] -> ()
  end;
  selected_circle := None;
  check_undo ();
  drawing#misc#queue_draw ())

let calc_selection mouse_x mouse_y =
  selected_circle :=  None;
  let min_d = ref None in
  List.iter (fun circle ->
      let mouse_r = Float.sqrt((circle.x-.mouse_x)**2.+.(circle.y-.mouse_y)**2.) in
      if mouse_r < circle.r then
        let d = circle.r-.mouse_r in
        match !min_d with
        | Some d' -> if d<d' then 
            min_d := Some d; selected_circle := Some circle
        | None -> selected_circle := Some circle) !circles

let change_diameter x y r =
  circles := List.map (fun circle ->
    if x=circle.x && y=circle.y then {x;y;r} else circle) !circles;
  selected_circle := Option.map (fun circle ->
    if x=circle.x && y=circle.y then {x;y;r} else circle) !selected_circle;
  drawing#misc#queue_draw ()

let create_circle x y r =
  circles := {x;y;r} :: !circles;
  drawing#misc#queue_draw ()

let create_diameter_window () =
  undo_list := !circles :: !undo_list;
  redo_list := [];
  match !selected_circle with
  | Some c ->
    let window = GWindow.window ~title:"" ~modal:true ~width:250 ~height:100 () in
    let vbox = GPack.vbox ~border_width:10 ~packing:window#add () in
    let label = GMisc.label ~text:(Printf.sprintf "Adjust diameter of circle at (%.0f, %.0f)" c.x c.y) ~packing:vbox#add () in
    let slider_adj = GData.adjustment ~lower:0. ~value:c.r ~upper:200. () in
    let slider = GRange.scale `HORIZONTAL ~adjustment:slider_adj ~packing:(vbox#pack ~padding:4 ~expand:true) () in
    let _ = window#connect#destroy ~callback:(fun () ->
      check_undo ()) in
    let _ = slider#connect#value_changed ~callback:(fun () -> change_diameter c.x c.y slider_adj#value;
    drawing#misc#queue_draw()) in
    window#show ();
    ignore (label, slider)
  | None -> ()

let _ = drawing#event#add [`BUTTON_PRESS]
let _ = drawing#event#connect#button_press ~callback:(fun ev ->
  let mouse_x = GdkEvent.Button.x ev in
  let mouse_y = GdkEvent.Button.y ev in
  let button = GdkEvent.Button.button ev in
  if button = 3 then
    match !selected_circle with 
    | Some c when Float.sqrt((c.x-.mouse_x)**2.+.(c.y-.mouse_y)**2.) < c.r
      ->
        let menu = GMenu.menu () in
        let menuitem = GMenu.menu_item ~label:"Adjust diameter..." ~use_mnemonic:true ~packing:menu#append () in
        ignore @@ menuitem#connect#activate ~callback:(fun () -> create_diameter_window ());
        menu#popup ~button:0 ~time:0l
    | _ -> ()
  else
    begin
      calc_selection mouse_x mouse_y;
      if !selected_circle = None then
      begin
        undo_list := !circles :: !undo_list;
        redo_list := [];
        create_circle mouse_x mouse_y 20.;
        check_undo ()    
      end
      else
        drawing#misc#queue_draw()
    end;
    true)
          
let () = ignore (button_undo, button_redo)
let () =
  ignore @@ w#connect#destroy ~callback: GMain.quit;
  w#show ();
  GMain.main ()