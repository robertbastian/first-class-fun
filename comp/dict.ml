(* lab3/dict.ml *)

(*
Environments are implemented using a library module that 
represents mappings by balanced binary trees.
*)

type ident = string

type codelab = int

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

type typ = 
    BoolType
  | NumType
  | FunType of typ list * typ
  | UnitType

(* |def| -- definitions in environment *)
type def = 
  { d_tag : ident;              (* Name *)
    d_kind : def_kind;          (* Definition *)
    d_type : typ;               (* Type *)
    d_level : int;              (* Nesting level *)
    d_lab : string;             (* Label if global *)
    d_off : int }               (* Offset if local *)

and def_kind =
    VarDef                      (* Variable *)
  | ProcDef                     (* Procedure *)

let find_def x ds =
  let rec search =
    function
        [] -> raise Not_found
      | d::ds -> 
          if x = d.d_tag then d else search ds in
  search ds

module IdMap = Map.Make(struct type t = ident  let compare = compare end)

type environment = Env of def list * def IdMap.t

let can f x = try f x; true with Not_found -> false

(* |define| -- add a definition *)
let define d (Env (b, m)) = 
  if can (find_def d.d_tag) b then raise Exit;
  Env (d::b, IdMap.add d.d_tag d m)

(* |lookup| -- find definition of an identifier *)
let lookup x (Env (b, m)) = IdMap.find x m

(* |empty| -- empty environment *)
let empty = Env ([], IdMap.empty)

(* |new_block| -- add new block *)
let new_block (Env (b, m)) = Env ([], m)

let lib_procs = [
  ("print", FunType([NumType], UnitType), "Lib.Print");
  ("print_b", FunType([BoolType], UnitType), "Lib.PrintB");
  ("newline", FunType([], UnitType), "Lib.Newline");
  ("print_f", FunType([NumType; NumType], UnitType), "Lib.PrintF");
  ("rand", FunType([NumType], NumType), "Lib.Rand")
]
