(* lab3/tree.ml *)

open Dict
open Print

(* |name| -- type for applied occurrences with annotations *)
type name = 
  { x_name: ident;              (* Name of the reference *)
    x_line: int;                (* Line number *)
    mutable x_def: def option } (* Definition in scope *)

type expr = 
    Number of int
  | Bool of bool
  | Variable of name
  | Monop of Keiko.op * expr
  | Binop of Keiko.op * expr * expr
  | Call of name * expr list

type stmt =
    Skip
  | Seq of stmt list
  | Assign of name * expr
  | Return of expr
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | Print of expr
  | Newline

type block = Block of (ident * typ) list * proc list * stmt

and proc = Proc of name * (ident * typ) list * block * typ

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
    | Call (x, es) ->
        fMeta "Call_($, $)" [fName x; fList(fExpr) es]

let rec fStmt = 
  function
      Skip -> 
        fStr "Skip"
    | Seq ss -> 
        fMeta "Seq_$" [fList(fStmt) ss]
    | Assign (x, e) -> 
        fMeta "Assign_($, $)" [fName x; fExpr e]
    | Return e ->
        fMeta "Return_($)" [fExpr e]
    | Print e -> 
        fMeta "Print_($)" [fExpr e]
    | Newline -> 
        fStr "Newline"
    | IfStmt (e, s1, s2) ->
        fMeta "IfStmt_($, $, $)" [fExpr e; fStmt s1; fStmt s2]
    | WhileStmt (e, s) -> 
        fMeta "WhileStmt_($, $)" [fExpr e; fStmt s]

let rec fBlock (Block (vs, ps, body)) =
  fMeta "Block_($, $, $)" [fList(fDecl) vs; fList(fProc) ps; fStmt body]

and fProc (Proc (x, fps, body, r)) =
  fMeta "Proc_($, $, $, $)" [fName x; fList(fDecl) fps; fBlock body; fType r]

let print_tree fp (Program b) =
  fgrindf fp "" "Program_($)" [fBlock b]
