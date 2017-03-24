(* lab3/kgen.mli *)

(* |translate| -- generate intermediate code *)
val translate : Tree.program -> unit

val optflag : bool ref
