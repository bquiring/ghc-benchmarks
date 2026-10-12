(* 3cps higher-order/function-composition.sml, with a call to main: the MLton reference *)
(* function-composition.sml *)

fun compose (f, g) = fn x => f (g x)

fun add1  x = x + 1
fun double x = x * 2
fun negate x = 0 - x
fun triple x = x * 3
fun addFive x = x + 5

(* Static known chain: negate(double(add1(x))) -- optimizer can inline fully *)
val pipeline = compose (negate, compose (double, add1))
(* pipeline 3 = negate(double(4)) = negate(8) = -8 *)

(* Longer static chain *)
val pipeline2 = compose (addFive, compose (negate, compose (double, compose (triple, add1))))
(* pipeline2 2: add1->3, triple->9, double->18, negate->-18, addFive->-13 *)

(* Dynamic pipeline: composeAll folds compose over a list.
   Closures genuinely escape into the list structure. *)
fun composeAll (fs : (int -> int) list) : int -> int =
  case fs of
    []      => (fn x => x)
  | f::rest => compose (f, composeAll rest)

val dynPipeline = composeAll [add1, double, triple]
(* dynPipeline 2 = add1(double(triple(2))) = add1(double(6)) = add1(12) = 13 *)

(* iterate n f: apply f n times to x *)
fun iterate n f x =
  if n = 0 then x
  else iterate (n - 1) f (f x)
(* iterate 3 add1 0 = 3 *)
(* iterate 2 double 1 = 4 *)

fun loop n =
  let
    val r1 = pipeline 3       (* -8  *)
    val r2 = pipeline2 2      (* -13 *)
    val r3 = dynPipeline 2    (* 13  *)
    val r4 = iterate 3 add1 0 (* 3   *)
    val r5 = iterate 2 double 1 (* 4 *)
    val total = r1 + r2 + r3 + r4 + r5
    (* -8 + (-13) + 13 + 3 + 4 = -1 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 46771520
      val _ = if result = ~1 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
