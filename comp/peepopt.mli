(* ppc/peepopt.mli *)

(* |optimise| -- rewrite list of instructions *)
val optimise : Keiko.code -> Keiko.code

(* |debug| -- debugging level *)
val debug: int ref
