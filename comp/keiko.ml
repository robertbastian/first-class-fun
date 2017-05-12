(* common/keiko.ml *)

open Tree 
open Print

(* |codelab| -- type of code labels *)
type codelab = int

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

(* |fLab| -- format a code label for printf *)
let fLab n = fMeta "$" [fNum n]

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA

(* |code| -- type of intermediate instructions *)
type code =
    CONST of int                (* Push constant (value) *)
  | GLOBAL of string            (* Push global address (name) *)
  | LOCAL of int                (* Push local adddress (offset) *)
  | LOADW                       (* Load word *)
  | STOREW                      (* Store word *)
  | LOADC                       (* Load character *)
  | STOREC                      (* Store character *)
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

(* op_name -- map an operator to its name *)
let op_name =
  function
      Plus -> "Plus" | Minus -> "Minus" | Times -> "Times"
    | Div -> "Div" | Mod -> "Mod" | Eq -> "Eq"
    | Uminus -> "Uminus" | Lt -> "Lt" | Gt -> "Gt" 
    | Leq -> "Leq" | Geq -> "Geq" | Neq -> "Neq" 
    | And -> "And" | Or -> "Or" | Not -> "Not"
    | PlusA -> "PlusA"

(* fOp -- format an operator as an instruction *)
let fOp w = fStr (String.uppercase (op_name w))

(* |fInst| -- format an instruction for |printf| *)
let fInst =
  function
      CONST x ->        fMeta "CONST $" [fNum x]
    | GLOBAL a ->       fMeta "GLOBAL $" [fStr a]
    | LOCAL n ->        fMeta "LOCAL $" [fNum n]
    | LOADW ->          fStr "LOADW"
    | STOREW ->         fStr "STOREW"
    | LOADC ->          fStr "LOADC"
    | STOREC ->         fStr "STOREC"
    | LDGW a ->         fMeta "LDGW $" [fStr a]
    | STGW a ->         fMeta "STGW $" [fStr a]
    | MONOP w ->        fMeta "$" [fOp w]
    | BINOP w ->        fMeta "$" [fOp w]
    | LABEL l ->        fMeta "LABEL $" [fLab l]
    | JUMP l ->         fMeta "JUMP $" [fLab l]
    | JUMPB (b, l) ->   fMeta "$ $" 
                          [fStr (if b then "JUMPT" else "JUMPF"); fLab l]
    | JUMPC (w, l) ->   fMeta "J$ $" [fOp w; fLab l]
    | PCALL n ->        fMeta "PCALL $" [fNum n]
    | PCALLW n ->       fMeta "PCALLW $" [fNum n]
    | RETURNW ->        fStr "RETURNW"
    | BOUND n ->        fMeta "BOUND $" [fNum n]
    | CASEJUMP n ->     fMeta "CASEJUMP $" [fNum n]
    | CASEARM (v, l) -> fMeta "CASEARM $ $" [fNum v; fLab l]
    | PACK ->           fStr "PACK"
    | UNPACK ->         fStr "UNPACK"

    | LINE n ->         fMeta "LINE $" [fNum n]
    | SEQ _ ->          fStr "SEQ ..."
    | NOP ->            fStr "NOP"

let mark_line n ys =
  if n = 0 then ys else
    match ys with
        [] | LINE _ :: _ -> ys
      | _ -> LINE n :: ys

(* |canon| -- flatten a code sequence *)
let canon x =
  let rec accum x ys =
    match x with
        SEQ xs -> List.fold_right accum xs ys
      | NOP -> ys
      | LINE n -> 
          if n = 0 then 
            ys 
          else begin
            match ys with
                ([] | LINE _ :: _) -> ys
              | _ -> LINE n :: ys
          end
      | _ -> x :: ys in
  SEQ (accum x [])

(* |output| -- output code sequence *)
let output code =
  let line = ref 0 in
  let rec out =
    function 
        SEQ xs -> List.iter out xs
      | NOP -> ()
      | LINE n -> 
          if n <> 0 && !line <> n then begin
            printf "! $\n" [fStr (Source.get_line n)];
            line := n
          end
      | x -> printf "$\n" [fInst x] in
  out code




