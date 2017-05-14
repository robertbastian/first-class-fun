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
    | Variable x | Partial(x,_) -> 
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
    | Call (e, args, c) ->
        match check_expr env e with
            FunType (atypes, rtype) ->
              c.c_returns <- Some rtype;
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
        begin match d.d_kind with
          ProcDef ->
            sem_error "procedure definition $ not assignable" [fStr x.x_name]
        | VarDef when a <> t ->
            sem_error "expected expression of type $, found type $" [fType t; fType a]
        | _ -> ()
        end
    | Side(e) ->
        let t = check_expr env e in
        if t <> UnitType then
          sem_error "non-unit expression cannot stand alone" []
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

(* |serialize| -- number a list, starting from 0 *)
let serialize xs = 
  let rec count i =
    function
        [] -> []
      | x :: xs -> (i, x) :: count (i+1) xs in
  count 0 xs

let is_packed_closure =
  function
    (_, FunType(_,_)) -> true
  | _ -> false

let rec make_bitmap =
  function
    [] -> 0
  | b::bs -> (if b then 1 else 0) + 2*(make_bitmap bs)

let make_ref_map decls =
  make_bitmap (List.map is_packed_closure decls)

(*
Frame layout

        arg n
        ...
fp+12:  arg 1
fp+8:   current cp
fp+4:   return addr
fp:     dynamic link
fp-4:   local 1
        ...
        local m
*)

let arg_base = 12
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

let declare_libs env (x, t, l) =
  let d = {d_tag = x; d_kind = ProcDef; d_type = t; d_level = 0;
                d_lab = l; d_off = 0} in
  add_def d env

(* |declare_proc| -- declare a procedure *)
let declare_proc lev env p_ = match p_.p_guts with 
  Proc (p, formals, Block (vars, procs, body), rtype) ->
  let lab = sprintf "$_$" [fStr p.x_name; fNum (label ())] in
  let d = { d_tag = p.x_name; d_kind = ProcDef;
                d_type = FunType((List.map snd formals), rtype);
                d_level = lev; d_lab = lab; d_off = 0 } in
  p.x_def <- Some d; add_def d env

(* |check_proc| -- check a procedure body *)
let rec check_proc lev env p =
  match p.p_guts with Proc (x, formals, Block (vars, procs, body), rtype) ->
  err_line := x.x_line;
  let capts = List.map (fun x -> (x.x_name, (lookup_def env x).d_type)) 
    (to_list p.p_capts) in
  p.p_amap <- make_ref_map formals;
  p.p_lmap <- make_ref_map vars;
  p.p_cmap <- make_ref_map capts;
  fgrindf stderr "" "Proc $ capts $\n" [fStr x.x_name; fList(fDecl) capts];
  let env' = 
    List.fold_left (declare_arg lev) (new_block env) (serialize (capts@formals)) in
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
  let env'' = List.fold_left declare_libs env' lib_procs in
  List.iter (check_proc 1 env'') procs;
  check_stmt None env'' body

























let rec captured_vars_e visible =
  function
   | Variable x -> if contains visible x then empty_set else singleton x 
   | Monop (w, e1) -> captured_vars_e visible e1
   | Binop (w, e1, e2) -> union [captured_vars_e visible e1; captured_vars_e visible e2]
   | Call (e, args,_) -> union (List.map (captured_vars_e visible) (e::args))
   | _ -> empty_set

let rec captured_vars_s visible =
  function
  | Seq ss -> union (List.map (captured_vars_s visible) ss)
  | Assign (x, e) ->
      if not (contains visible x) then sem_error "assigning to variable $ in enclosing scope" [fStr x.x_name]
      else captured_vars_e visible e
  | Return e -> captured_vars_e visible e
  | IfStmt (test, thenpt, elsept) -> union
    [captured_vars_e visible test; captured_vars_s visible thenpt; captured_vars_s visible elsept]
  | WhileStmt (test, body) -> union
    [captured_vars_e visible test; captured_vars_s visible body]
  | Side e -> captured_vars_e visible e
  | _ -> empty_set

let visible glovs p = match p.p_guts with (Proc (n, formals, Block (vars, procs, body), _)) ->
  union [from_lists [[n]; glovs; List.map (fun (x,t) -> makeName x (-1)) (formals@vars);
      List.map (fun q -> match q.p_guts with Proc(m, _, _ ,_) -> m) procs;]; 
      p.p_capts];

let rec annotate_captured glovs p = 
  match p.p_guts with (Proc (n, formals, Block (vars, procs, body), _)) ->
  p.p_capts <- captured_vars_s (visible glovs p) body;
  p.p_lifted <- List.length (to_list p.p_capts) == 0;
  List.iter (annotate_captured glovs) procs;

let rec add_extra_args_e p extra x = 
  function 
      Variable(y) | Partial(y, _) when y.x_name == x.x_name -> 
      let need_more_capts = not (subset extra (visible [] p)) in
      if need_more_capts then (p.p_lifted <- false; p.p_capts <- union [p.p_capts; from_lists [extra]]);
      Partial (y, extra)
    | Monop(w, e) -> Monop (w, add_extra_args_e p extra x e)
    | Binop(w, e1, e2) -> Binop (w, add_extra_args_e p extra x e1, add_extra_args_e p extra x e2) 
    | Call(e, a, c) -> Call(add_extra_args_e p extra x e, List.map (add_extra_args_e p extra x) a, c)
    | e -> e
  
let rec add_extra_args_s p extra x = 
  function
      Seq ss -> Seq (List.map (add_extra_args_s p extra x) ss)
    | Assign (x, e) -> Assign(x, add_extra_args_e p extra x e)
    | Return e -> Return (add_extra_args_e p extra x e)
    | IfStmt (test, thenpt, elsept) -> IfStmt (
      add_extra_args_e p extra x test, add_extra_args_s p extra x thenpt, add_extra_args_s p extra x elsept)
    | WhileStmt (test, body) -> WhileStmt (
      add_extra_args_e p extra x test, add_extra_args_s p extra x body)
    | Side e -> Side (add_extra_args_e p extra x e)
    | s -> s

let rec add_extra_args_p extra x p = 
  let Proc (n, f, (Block (vars, procs, body)), r) = p.p_guts in (
  p.p_guts <- Proc (n, f, (Block (vars, procs, add_extra_args_s p extra x body)), r))

and lift siblings parent p = 
  if not p.p_lifted then (
    let Proc (n, formals, body, rtype) = p.p_guts in
    List.map (add_extra_args_p (to_list p.p_capts) n) (parent@siblings)
  )

and do_lifts procs parent = List.exists (fun x->x) (List.map (lift procs parent) procs) 

let lambda_lift (Program (Block (vars, procs, body))) = 
  List.iter (annotate_captured (List.map (fun (x,t) -> makeName x (-1)) vars)) procs;
  do_lifts procs []; ()
