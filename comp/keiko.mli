(* common/keiko.mli *)

(* |codelab| -- type of code labels *)
type codelab = int

(* |label| -- allocate a code label *)
val label : unit -> codelab

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA

(* op_name -- map an operator to its name *)
val op_name : op -> string

(* |code| -- type of intermediate instructions *)
type code =
    CONST of int                (* Push constant (value) *)
  | GLOBAL of string            (* Push global address (name) *)
  | LOCAL of int                (* Push local adddress (offset) *)
  | LOADW                       (* Load word *)
  | STOREW                      (* Store word *)
  | LOADC                       (* Load character *)
  | STOREC                      (* Store character *)
  | LOADP                       (* Load packed closure *)
  | STOREP                      (* Store packed closure *)
  | LOADE                       (* Load environment pointer *)
  | LDGW of string              (* Load value (name) *)
  | STGW of string              (* Store (name) *)
  | MONOP of op                 (* Perform unary operation (op) *)
  | BINOP of op                 (* Perform binary operation (op) *)
  | LABEL of codelab            (* Set code label *)
  | JUMP of codelab             (* Unconditional branch (dest) *)
  | JUMPB of bool * codelab     (* Branch on boolean (val, dest) *)
  | JUMPC of op * codelab       (* Conditional branch (op, dest) *)
  | PCALL of int                (* Call procedure *)
  | PCALLW of int               (* Proc call with result (nargs) *)
  | RETURNW                     (* Return from procedure *)
  | BOUND of int                (* Bounds check *)
  | CASEJUMP of int             (* Case jump (num cases) *)
  | CASEARM of int * codelab    (* Case value and label *)
  | PACK                        (* Pack two values into one *)
  | UNPACK                      (* Unpack one value into two *)

  | LINE of int
  | SEQ of code list
  | NOP

(* |fInst| -- format an instruction for |printf| *)
val fInst : code -> Print.arg

(* |canon| -- flatten a code sequence *)
val canon : code -> code

(* |output| -- output a code sequence *)
val output : code -> unit
