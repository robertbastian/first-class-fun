(* lab3/check.ml *)

open Tree 
open Dict 
open Print 
open Keiko

(* |err_line| -- line number for error messages *)
let err_line = ref 1

(* |Semantic_error| -- exception raised if error detected *)
exception Semantic_error of string * Print.arg list * int

(* |sem_error| -- issue error message by raising exception *)
let sem_error fmt args = 
  raise (Semantic_error (fmt, args, !err_line))

(* |lookup_def| -- find definition of a name, give error is none *)
let lookup_def env x =
  err_line := x.x_line;
  try let d = lookup x.x_name env in x.x_def <- Some d; d with 
    Not_found -> sem_error "$ is not declared" [fStr x.x_name]

(* |add_def| -- add definition to env, give error if already declared *)
let add_def d env =
  try define d env with 
    Exit -> sem_error "$ is already declared" [fStr d.d_tag]

(* |check_expr| -- check and annotate an expression, returns type *)
let rec check_expr env =
  function
      Number n -> NumType
    | Bool b -> BoolType
    | Variable x -> 
        let d = lookup_def env x in d.d_type
    | Monop (w, e1) ->
        let t = check_expr env e1 in begin
          match w with
            Uminus -> if t <> NumType then
                sem_error "Operation $ can only be applied to numeric expression" 
                  [fStr (op_name w)]
              else NumType
          | Not -> if t <> BoolType then
                sem_error "Operation $ can only be applied to boolean expressions" 
                  [fStr (op_name w)]
              else BoolType
          | _ -> sem_error "unknown monop" []
        end
    | Binop (w, e1, e2) -> 
        let t1 = check_expr env e1 in
        let t2 = check_expr env e2 in begin
        match w with
            Plus | Minus | Times | Div | Mod -> 
              if t1 <> NumType || t2 <> NumType then 
                sem_error "Operation $ can only be applied to numeric expressions" 
                  [fStr (op_name w)]
              else NumType

          | Eq | Lt | Gt | Leq | Geq | Neq ->
              if t1 <> NumType || t2 <> NumType then 
                sem_error "Operation $ can only be applied to numeric expressions" 
                  [fStr (op_name w)]
              else BoolType
          | And | Or ->
              if t1 <> BoolType || t2 <> BoolType then
                sem_error "Operation $ can only be applied to boolean expressions" 
                  [fStr (op_name w)]
              else BoolType
          | _ -> sem_error "unknown binop" [] (* plusa? *)
        end
    | Call (e, args) ->
        match check_expr env e with
            FunType (atypes, rtype) ->
              if List.length args <> List.length atypes then
                sem_error "procedure needs $ arguments" 
                  [fNum (List.length atypes)]
              else if List.exists2 (<>) atypes (List.map (check_expr env) args) then
                sem_error "argument type mismatch" []
              else rtype  
          | t -> sem_error "expression with type $ not callable" 
            [fType t]

(* |check_stmt| -- check and annotate a statement *)
let rec check_stmt rtype env =
  function
      Skip -> ()
    | Seq ss ->
        List.iter (check_stmt rtype env) ss
    | Assign (x, e) ->
        let d = lookup_def env x in
        let a = check_expr env e and t = d.d_type in
        if a <> t then
          sem_error "expected expression of type $, found type $" [fType t; fType a]
    | Return e -> begin match rtype with
          None -> 
            sem_error "return statement only allowed in procedure" []
        | Some r -> let t = check_expr env e in if t <> r then
            sem_error "expected expression of type $, found type $" [fType r; fType t]
        end
    | IfStmt (test, thenpt, elsept) ->
        if check_expr env test <> BoolType then 
          sem_error "guard needs to be boolean" [];
        check_stmt rtype env thenpt;
        check_stmt rtype env elsept; ()
    | WhileStmt (test, body) ->
        if check_expr env test <> BoolType then 
          sem_error "guard needs to be boolean" [];
        check_stmt rtype env body; ()
    | Print e -> begin match check_expr env e with
          FunType(_,_) -> sem_error "procedure not printable" []
        | _ -> ()
      end
    | Newline ->
        ()

(* |serialize| -- number a list, starting from 0 *)
let serialize xs = 
  let rec count i =
    function
        [] -> []
      | x :: xs -> (i, x) :: count (i+1) xs in
  count 0 xs

(*
Frame layout

        arg n
        ...
fp+16:  arg 1
fp+12:  static link
fp+8:   current cp
fp+4:   return addr
fp:     dynamic link
fp-4:   local 1
        ...
        local m
*)

let arg_base = 16
let loc_base = 0

(* |declare_arg| -- declare a formal parameter *)
let declare_arg lev env (i, (x,t)) =
  let d = { d_tag = x; d_kind = VarDef; d_type = t; d_level = lev;
                d_lab = ""; d_off = arg_base + 4*i } in
  add_def d env

(* |declare_arg| -- declare a local variable *)
let declare_local lev env (i, (x,t)) =
  let d = { d_tag = x; d_kind = VarDef; d_type = t; d_level = lev;
                d_lab = ""; d_off = loc_base - 4*(i+1) } in
  add_def d env

(* |declare_global| -- declare a global variable *)
let declare_global env (x, t) =
  let d = { d_tag = x; d_kind = VarDef; d_type = t; d_level = 0;
                d_lab = sprintf "_$" [fStr x]; d_off = 0 } in
  add_def d env

(* |declare_proc| -- declare a procedure *)
let declare_proc lev env (Proc (p, formals, body, rtype)) =
  let lab = sprintf "$_$" [fStr p.x_name; fNum (label ())] in
  let d = { d_tag = p.x_name; 
                d_kind = ProcDef;
                d_type = FunType((List.map snd formals), rtype);
                d_level = lev; d_lab = lab; d_off = 0 } in
  p.x_def <- Some d; add_def d env

(* |check_proc| -- check a procedure body *)
let rec check_proc lev env (Proc (p, formals, Block (vars, procs, body), rtype)) =
  err_line := p.x_line;
  let env' = 
    List.fold_left (declare_arg lev) (new_block env) (serialize formals) in
  let env'' = 
    List.fold_left (declare_local lev) env' (serialize vars) in
  let env''' = 
    List.fold_left (declare_proc (lev+1)) env'' procs in
  List.iter (check_proc (lev+1) env''') procs;
  check_stmt (Some rtype) env''' body

(* |annotate| -- check and annotate a program *)
let annotate (Program (Block (vars, procs, body))) =
  let env = List.fold_left declare_global empty vars in
  let env' = List.fold_left (declare_proc 1) env procs in
  List.iter (check_proc 1 env') procs;
  check_stmt None env' body
