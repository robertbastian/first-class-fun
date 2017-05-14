(* lab3/kgen.ml *)

open Tree 
open Dict 
open Keiko 
open Print 

let optflag = ref false

let level = ref 0

let slink = 12


(* |gen_addr| -- generate code to push address of a variable *)
let gen_addr d =
  if d.d_level = 0 then
    GLOBAL d.d_lab
  else
    SEQ [LOCAL 0; CONST d.d_off; BINOP PlusA]

let gen_ldst d isLoad = SEQ [gen_addr d; match d.d_type, isLoad with
      FunType(_,_), true -> LOADP
    | FunType(_,_), false -> STOREP
    | _, true -> LOADW
    | _, false -> STOREW]

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr =
  function
      Variable x ->
        let d = get_def x in
        begin
          match d.d_kind with
              VarDef ->
                SEQ [LINE x.x_line; gen_ldst d true]
            | ProcDef -> SEQ [LINE x.x_line; GLOBAL d.d_lab; PACK]
        end
    | Number x ->
        CONST x
    | Bool b -> CONST (if b then 1 else 0)
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]
    | Partial (x, args) ->
        let d = get_def x in
        begin
          match d.d_kind with
              ProcDef ->
                let vals = List.map (fun x -> gen_ldst (get_def x) true) args in
                SEQ [LINE x.x_line; SEQ (List.rev vals); GLOBAL d.d_lab; PACK]
            | _ -> failwith "cannot apply partially apply variable"
        end
    | Call (e, args, c) ->
        let call = begin match c.c_returns with 
            Some(UnitType) -> PCALL (List.length args)
          | Some(_)        -> PCALLW (List.length args)
          | _ -> failwith "call not annotated"
        end in let frame = SEQ [gen_expr e; UNPACK] in
        SEQ[SEQ (List.rev (List.map gen_expr args)); frame; call]

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond tlab flab e =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e with
      Number x ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2;
          JUMPC (w, tlab); JUMP flab]
    | Monop (Not, e1) ->
        gen_cond flab tlab e1
    | Binop (And, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond lab1 flab e1; LABEL lab1; gen_cond tlab flab e2]
    | Binop (Or, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond tlab lab1 e1; LABEL lab1; gen_cond tlab flab e2]
    | _ ->
        SEQ [gen_expr e; JUMPB (true, tlab); JUMP flab]

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt =
  function
      Skip -> NOP
    | Seq ss ->
        SEQ (List.map gen_stmt ss)
    | Assign (v, e) ->
        let d = get_def v in
        begin
          match d.d_kind with
              VarDef ->
                SEQ [gen_expr e; gen_ldst d false]
           | _ -> failwith "assign"
        end
    | Side (e) -> gen_expr e
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond lab1 lab2 test; 
          LABEL lab1; gen_stmt thenpt; JUMP lab3;
          LABEL lab2; gen_stmt elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [LABEL lab1; gen_cond lab2 lab3 test;
          LABEL lab2; gen_stmt body; JUMP lab1; LABEL lab3]
    | Return e ->
        SEQ [gen_expr e; RETURNW]

(* |gen_proc| -- generate code for a procedure *)
let rec gen_proc p_ = match p_.p_guts with
  Proc (p, formals, Block (vars, procs, body), rtype) ->
  let d = get_def p in
  level := d.d_level;
  let code = gen_stmt body in 
  printf "PROC $ $ $ $ $ $ $\n" [fStr d.d_lab; 
    fNum (List.length formals); fNum p_.p_amap; 
    fNum (List.length vars); fNum p_.p_lmap; 
    fNum (List.length (to_list (p_.p_capts))); fNum p_.p_cmap];
  Keiko.output (if !optflag then Peepopt.optimise code else code);
  printf "$" [fStr (match rtype with UnitType -> "RETURN\n" | _ -> "ERROR E_RETURN 0\n")];
  printf "END\n\n" [];
  List.iter gen_proc procs

(* |translate| -- generate code for the whole program *)
let translate (Program (Block (vars, procs, body))) =
  level := 0;
  printf "PROC MAIN 0 0 0 0 0 0\n" [];
  let m = gen_stmt body in
  Keiko.output (if !optflag then Peepopt.optimise m else m);
  List.iter (function (x,t) -> match t with 
      FunType(_,_) -> printf "GLOBAL _$ \nDECPREF\nPOP 1\n" [fStr x]
    | _ -> ()) vars;
  printf "RETURN\n" [];
  printf "END\n\n" [];
  List.iter gen_proc procs;
  List.iter (function (x,t) -> printf "GLOVAR _$ 4\n" [fStr x]) vars
