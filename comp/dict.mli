(* lab3/dict.mli *)

type ident = string

type codelab = int

val label : unit -> codelab

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

type environment

(* |define| -- add a definition, raise Exit if already declared *)
val define : def -> environment -> environment

(* |lookup| -- search an environment or raise Not_found *)
val lookup : ident -> environment -> def

(* |new_block| -- add new block to top of environment *)
val new_block : environment -> environment

(* |empty| -- initial empty environment *)
val empty : environment

val lib_procs : (ident * typ * string) list
