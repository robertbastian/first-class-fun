(* lab3/tree.mli *)

open Dict

(* |name| -- type for applied occurrences with annotations *)
type name = 
  { x_name: ident;              (* Name of the reference *)
    x_line: int;                (* Line number *)
    mutable x_def: def option}  (* Definition in scope *)

type calltype = { mutable c_returns: typ option }

type expr = 
    Number of int
  | Bool of bool
  | Variable of name
  | Monop of Keiko.op * expr
  | Binop of Keiko.op * expr * expr
  | Call of expr * expr list * calltype
  | Partial of name * name list

type stmt =
    Skip
  | Seq of stmt list
  | Assign of name * expr
  | Side of expr
  | Return of expr
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt

type nameset

type block = Block of (ident * typ) list * proc list * stmt

and proc = 
  { mutable p_guts: proc_guts; 
    mutable p_capts: nameset;
    mutable p_lifted: bool;
    mutable p_amap: int;
    mutable p_lmap: int;
    mutable p_cmap: int}

and proc_guts = Proc of name * (ident * typ) list * block * typ

type program = Program of block


(* seq -- neatly join a list of statements into a sequence *)
val seq : stmt list -> stmt

val makeName : ident -> int -> name

val makeProc : proc_guts -> proc

(* |get_def| -- rerieve definition from name *)
val get_def : name -> def

(* |print_tree| -- pretty-print a tree *)
val print_tree : out_channel -> program -> unit

val fType : typ -> Print.arg

val fDecl : ident * typ -> Print.arg

val empty_set : nameset

val subset : name list -> nameset -> bool

val contains : nameset -> name -> bool

val union : nameset list -> nameset

val singleton : name -> nameset

val to_list : nameset -> name list

val from_lists : name list list -> nameset
