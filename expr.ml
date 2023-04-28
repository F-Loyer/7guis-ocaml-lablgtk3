type t =
  | Add of t * t
  | Sub of t * t
  | Multiply of t * t
  | Divide of t * t 
  | UnaryPlus of t
  | UnaryMinus of t
  | Float of float
  | Cell of int * int
  | Range of int * int * int * int
  | Function of string * t list
  | Null
  | Invalid of string
  | String of string

type cell_status = OK | Lazy | Computed
type value = FloatVal of float | NullVal | InvalidVal of string | StringVal of string
let string_of_value v = match v with NullVal -> "" | FloatVal v -> string_of_float v | InvalidVal s -> s | StringVal s -> s
exception Expression_error of string

type cell_type = {
    col:int; row:int;
    mutable str:string;
    mutable expr: t;
    mutable status: cell_status;
    mutable value: value;
    (* List of cell which should be recomputed when this one change *)
    dependant: ((int*int),cell_type) Hashtbl.t
}
let spreadsheet = Hashtbl.create 10
let callback = ref (fun (_col,_row) _str -> ())

let cell_of_string s = Cell (
  (let col = Char.code(s.[0]) in
    if col < 96 then col - 65
    else col - 97),
  (let len = String.length s in
    int_of_string(String.sub s 1 (len-1))))

let range_of_cells cell1 cell2 = 
  match cell1, cell2 with 
  | Cell(col1,row1), Cell(col2,row2) -> Range ((min col1 col2),(min row1 row2),(max col1 col2),(max row1 row2))
  | _,_ -> Null

let rec iter_dependant_cells f expr =
  let iter_cell (col,row) =
    begin try 
      let cell = Hashtbl.find spreadsheet (col,row) in f cell
      with Not_found -> 
        let cell = {col;row;str="";expr=Null;status=OK; value=NullVal; dependant=Hashtbl.create 10} in
           Hashtbl.add spreadsheet (col,row) cell;
           f cell
    end
  in match expr with
  | Add (a,b) -> iter_dependant_cells f a; iter_dependant_cells f b 
  | Sub (a,b) -> iter_dependant_cells f a; iter_dependant_cells f b 
  | Multiply (a,b) -> iter_dependant_cells f a; iter_dependant_cells f b 
  | Divide (a,b) -> iter_dependant_cells f a; iter_dependant_cells f b 
  | UnaryMinus a -> iter_dependant_cells f a 
  | UnaryPlus a -> iter_dependant_cells f a
  | Function (_f,arguments) ->
      List.iter (fun e -> iter_dependant_cells f e) arguments
  | Cell (col,row) ->
      iter_cell (col,row)
  | Range (col1,row1,col2,row2) ->
    for col = col1 to col2 do
      for row = row1 to row2 do
        iter_cell (col,row)
      done
    done
  | Null | Float _ | Invalid _ | String _ -> ()

let float_of_value value = 
  match value with 
  | NullVal -> 0.
  | FloatVal f -> f
  | _ -> raise (Expression_error "Invalid argument")
  
let rec eval_expr expr =
  match expr with
  | Add (a,b) -> let a', b' = 
      (float_of_value (eval_expr a)), (float_of_value (eval_expr b)) in FloatVal (a' +. b')
  | Sub (a,b) -> let a', b' = 
      (float_of_value (eval_expr a)), (float_of_value (eval_expr b)) in FloatVal (a' -. b')
  | UnaryPlus a -> let a' =
      (float_of_value (eval_expr a)) in FloatVal a'
  | UnaryMinus a -> let a' =
      (float_of_value (eval_expr a)) in FloatVal (-. a')
  | Multiply (a,b) -> let a', b' = 
      (float_of_value (eval_expr a)), (float_of_value (eval_expr b)) in FloatVal (a' *. b')
  | Divide (a,b) -> let a', b' = 
      (float_of_value (eval_expr a)), (float_of_value (eval_expr b)) in
            if b' =0. then raise (Expression_error "#divide by 0")
            else
              FloatVal (a'/.b')
  | Null -> NullVal
  | Float f -> FloatVal f
  | String s -> StringVal s
  | Cell (col,row) -> cell_value (col,row)
  | Function (f,arguments) ->
    begin
      match f with
      | "SUM" -> let sum = List.fold_left (fun accu expr ->
                    match expr with
                    | Range (col1,row1,col2,row2) ->
                        let accu' = ref accu in
                          for col = col1 to col2 do
                            for row = row1 to row2 do 
                              let b = float_of_value (cell_value (col,row)) in
                                accu' := !accu' +. b
                            done
                          done;
                          !accu'
                     | expr -> let b = float_of_value (eval_expr expr) in accu +. b 
            ) 0. arguments in FloatVal sum
      | _ -> raise (Expression_error "#Unknown func")
    end
  | Invalid s -> raise (Expression_error s)
  | Range (_,_,_,_) -> raise (Expression_error "#Invalid argument")
and cell_value (col,row) =
  let cell = Hashtbl.find spreadsheet (col,row) in 
  eval_cell cell;
  match cell.value with
  | InvalidVal s -> raise (Expression_error s)
  | cell_value -> cell_value
and eval_cell cell =
     match cell.status with
     | OK -> ()
     | Lazy ->
        cell.status <- Computed; 
        begin
          try 
            cell.value <- eval_expr cell.expr
          with
          | Expression_error s ->
               cell.value <- InvalidVal s
        end;
        (!callback) (cell.col, cell.row) (string_of_value cell.value);
        cell.status <- OK;
     | Computed ->
        cell.value <- InvalidVal "#Circular ref";
        (!callback) (cell.col, cell.row) (string_of_value cell.value)

let rec invalid_cell_and_dependant cell =
  if cell.status <> Lazy then
  begin
    cell.status <- Lazy;
    Hashtbl.iter (fun _coord cell' -> invalid_cell_and_dependant cell') cell.dependant
  end

let rec recompute_cell_and_dependant cell =
  if cell.status = Lazy then
  begin
    eval_cell cell;
    Hashtbl.iter (fun _coord cell' -> recompute_cell_and_dependant cell') cell.dependant
  end

let add_formulae (col,row) str expr = 
  if Hashtbl.mem spreadsheet (col,row) then
    let cell = Hashtbl.find spreadsheet (col,row) in
      invalid_cell_and_dependant cell;
      iter_dependant_cells (fun cell' -> Hashtbl.remove cell'.dependant (col,row)) cell.expr; (* Unsubsribe *)
      cell.str <- str;
      cell.expr <- expr;
      iter_dependant_cells (fun cell' -> Hashtbl.add cell'.dependant (col,row) cell) cell.expr; (* Subscribe *)
      recompute_cell_and_dependant cell
  else
    let cell = { col;row; str;expr;status=Lazy; value=NullVal; dependant=Hashtbl.create 10} in
    Hashtbl.add spreadsheet (col,row) cell;
    iter_dependant_cells (fun cell' -> Hashtbl.add cell'.dependant (col,row) cell) cell.expr ; (* Subscribe *)
    recompute_cell_and_dependant cell

let get_formulae (col,row) =
  if Hashtbl.mem spreadsheet (col,row) then
    let cell = Hashtbl.find spreadsheet (col,row) in
       cell.str
  else
    ""    
