type t =
  | Add of t * t
  | Sub of t * t
  | Multiply of t * t
  | Divide of t * t 
  | UnaryPlus of t
  | UnaryMinus of t
  | Float of float
  | Cell of int * int
  | Range of t * t
  | Function of string * t
  | Null
  | Invalid of string
  | String of string

type cell_status = OK | Lazy | Computed
type value = FloatVal of float | NullVal | InvalidVal of string | StringVal of string
let string_of_value v = match v with NullVal -> "" | FloatVal v -> string_of_float v | InvalidVal s -> s | StringVal s -> s
  
type cell_type = {
    col:int; row:int;
    mutable str:string;
    mutable status: cell_status;
    mutable value: value;
    mutable formulae: t;
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

let rec iter_dependant_cells expr f =
  match expr with
  | Add (a,b) -> iter_dependant_cells a f; iter_dependant_cells b f 
  | Sub (a,b) -> iter_dependant_cells a f; iter_dependant_cells b f 
  | Multiply (a,b) -> iter_dependant_cells a f; iter_dependant_cells b f 
  | Divide (a,b) -> iter_dependant_cells a f; iter_dependant_cells b f 
  | UnaryMinus a -> iter_dependant_cells a f 
  | UnaryPlus a -> iter_dependant_cells a f 
  | Cell (col,row) -> begin
    try 
      let cell = Hashtbl.find spreadsheet (col,row) in f cell
      with Not_found -> 
        let cell = {col;row;status=OK; str=""; value=NullVal; formulae=Null; dependant=Hashtbl.create 10} in
           Hashtbl.add spreadsheet (col,row) cell;
           f cell
      end
  | _ -> ()

let rec eval_expr expr =
  match expr with
  | Add (a,b) -> begin match (eval_expr a), (eval_expr b) with
         | FloatVal a', FloatVal b' -> FloatVal (a'+.b')
         | _,_ -> InvalidVal "fail" end
  | Sub (a,b) -> begin match (eval_expr a), (eval_expr b) with
         | FloatVal a', FloatVal b' -> FloatVal (a'-.b')
         | _,_ -> InvalidVal "fail" end
  | UnaryPlus a -> begin match (eval_expr a) with
         | FloatVal a' -> FloatVal (a')
         | _ -> InvalidVal "fail" end
  | UnaryMinus a -> begin match (eval_expr a) with
         | FloatVal a' -> FloatVal (-.a')
         | _ -> InvalidVal "fail" end
  | Multiply (a,b) -> begin match (eval_expr a), (eval_expr b) with
         | FloatVal a', FloatVal b' -> FloatVal (a'*.b')
         | _,_ -> InvalidVal "fail" end
  | Divide (a,b) -> begin match (eval_expr a), (eval_expr b) with
         | FloatVal a', FloatVal b' ->
            if b' =0. then 
              InvalidVal "divide by 0" 
            else
              FloatVal (a'/.b')
         | _,_ -> InvalidVal "fail" end
  | Null -> NullVal
  | Float f -> FloatVal f
  | String s -> StringVal s
  | Cell (c,r) -> 
    let cell = Hashtbl.find spreadsheet (c,r) in 
      eval_cell cell;
      cell.value
  | _ -> InvalidVal "not implemented"
and eval_cell cell =
     match cell.status with
     | OK -> ()
     | Lazy ->
        cell.status <- Computed; 
        cell.value <- eval_expr cell.formulae; 
        (!callback) (cell.col, cell.row) (string_of_value cell.value);
        cell.status <- OK;
     | Computed ->
        cell.value <- InvalidVal "Circular ref";
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
      iter_dependant_cells cell.formulae (fun cell' -> Hashtbl.remove cell'.dependant (col,row)); (* Unsubsribe *)
      cell.str <- str;
      cell.formulae <- expr;
      iter_dependant_cells cell.formulae (fun cell' -> Hashtbl.add cell'.dependant (col,row) cell); (* Subscribe *)
      recompute_cell_and_dependant cell
  else
    let cell = { col;row; str;status=Lazy; value=InvalidVal "new"; formulae=expr; dependant=Hashtbl.create 10} in
    Hashtbl.add spreadsheet (col,row) cell;
    iter_dependant_cells cell.formulae (fun cell' -> Hashtbl.add cell'.dependant (col,row) cell); (* Subscribe *)
    recompute_cell_and_dependant cell

let get_formulae (col,row) =
  if Hashtbl.mem spreadsheet (col,row) then
    let cell = Hashtbl.find spreadsheet (col,row) in
       cell.str
  else
    ""    
