(* lab3/check.mli *)

open Tree

(* |annotate| -- check tree for type errors and annotate with definitions *)
val annotate : program -> unit

val lambda_lift : program -> unit

(* |Semantic_error| -- exception raised if error detected *)
exception Semantic_error of string * Print.arg list * int
