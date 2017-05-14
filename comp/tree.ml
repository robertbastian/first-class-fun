(* lab3/tree.ml *)

open Dict
open Print

(* |name| -- type for applied occurrences with annotations *)
type name = 
  { x_name: ident;              (* Name of the reference *)
    x_line: int;                (* Line number *)
    mutable x_def: def option } (* Definition in scope *)

module IdSet = Set.Make(struct type t = name  let compare n1 n2 = compare n1.x_name n2.x_name end)
type nameset = IdSet.t
let empty_set = IdSet.empty
let subset lst set = IdSet.subset (IdSet.of_list lst) set
let contains s e = IdSet.mem e s
let union = List.fold_left IdSet.union empty_set
let singleton = IdSet.singleton
let to_list = IdSet.elements
let from_lists ls = union (List.map IdSet.of_list ls)

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


let seq =
  function
      [] -> Skip
    | [s] -> s
    | ss -> Seq ss

let makeName x ln = 
  { x_name = x; x_line = ln; x_def = None }

let get_def x =
  match x.x_def with
      Some d -> d
    | None -> failwith (sprintf "missing def on $" [fStr x.x_name])

let makeProc guts = { p_guts = guts; p_capts = empty_set; p_amap = 0;
  p_lmap = 0; p_cmap = 0; p_lifted = true }

(* Pretty printer *)

open Print

let fList_ f =
  function
      [] -> fStr ""
    | [x] -> f x
    | x::xs -> fMeta "$;$" [f x; fList(f) xs]

let fList f x = fMeta "[$]" [fList_(f) x]

let fName x = fStr x.x_name

let rec fType =
  function
    NumType -> fStr "num"
  | BoolType -> fStr "bool"
  | FunType (atypes, rtype) -> fMeta "($) -> $" [fList_(fType) atypes; fType rtype]
  | UnitType -> fStr "unit"

let fDecl (id,t) = fMeta "$: $" [fStr id; fType t]

let rec fExpr =
  function
      Number n ->
        fMeta "Number_$" [fNum n]
    | Bool b ->
        fMeta "Bool_$" [fBool b]
    | Variable x -> 
        fMeta "Variable_$" [fName x]
    | Monop (w, e1) -> 
        fMeta "Monop_($, $)" [fStr (Keiko.op_name w); fExpr e1]
    | Binop (w, e1, e2) -> 
        fMeta "Binop_($, $, $)" [fStr (Keiko.op_name w); fExpr e1; fExpr e2]
    | Call (e, es, _) ->
        fMeta "Call_($, $)" [fExpr e; fList(fExpr) es]
    | Partial (x, xs) ->
        fMeta "Partial_($, $)" [fName x; fList(fName) xs]

let rec fStmt = 
  function
      Skip -> 
        fStr "Skip"
    | Seq ss -> 
        fMeta "Seq_$" [fList(fStmt) ss]
    | Assign (x, e) -> 
        fMeta "Assign_($, $)" [fName x; fExpr e]
    | Side expr ->
        fMeta "Side_($)" [fExpr expr]
    | Return e ->
        fMeta "Return_($)" [fExpr e]
    | IfStmt (e, s1, s2) ->
        fMeta "IfStmt_($, $, $)" [fExpr e; fStmt s1; fStmt s2]
    | WhileStmt (e, s) -> 
        fMeta "WhileStmt_($, $)" [fExpr e; fStmt s]

let rec fBlock (Block (vs, ps, body)) =
  fMeta "Block_($, $, $)" [fList(fDecl) vs; fList(fProc) ps; fStmt body]

and fProc p = match p.p_guts with (Proc (x, fps, body, r)) -> 
  fMeta "Proc_($, $, $, $, capt $)" [fName x; fList(fDecl) fps; fBlock body; fType r; fList(fName) (to_list p.p_capts)]

let print_tree fp (Program b) =
  fgrindf fp "" "Program_($)" [fBlock b]
