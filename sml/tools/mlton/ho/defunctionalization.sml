(* 3cps higher-order/defunctionalization.sml, with a call to main: the MLton reference *)
(* defunctionalization.sml *)

(* The ONLY indirect call site in the program.
   A statically-bounded set of functions reaches it. *)
fun apply (f, x) = f x

(* Closed set of operations -- each captures a different free variable *)
fun addOp k = fn x => x + k
fun mulOp k = fn x => x * k
fun negOp _ = fn x => 0 - x    (* ignores captured constant *)
fun sqOp  _ = fn x => x * x

fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (acc, x)) xr

fun nth (xs, i) =
  case xs of
    []      => (fn x : int => x)
  | f::rest => if i = 0 then f else nth (rest, i - 1)

fun loop n =
  let
    (* All four ops in a list -- they all flow to apply *)
    val ops = [addOp 3, mulOp 2, negOp 0, sqOp 0, addOp 10]

    (* (a) foldl applying each op in turn to accumulator *)
    val r1 = foldl (fn (acc, f) => apply (f, acc)) 1 ops
    (* 1 -> +3 -> 4 -> x2 -> 8 -> neg -> -8 -> sq -> 64 -> +10 -> 74 *)

    (* (b) select by index and apply -- same apply site *)
    val r2 = apply (nth (ops, 1), 5)    (* mulOp 2 applied to 5 = 10 *)
    val r3 = apply (nth (ops, 0), 100)  (* addOp 3 applied to 100 = 103 *)

    (* Contrast: known inline-able call, does NOT flow to apply *)
    val r4 = (fn x => x + 7) 3         (* 10 *)

    val total = r1 + r2 + r3 + r4
    (* 74 + 10 + 103 + 10 = 197 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 21568176
      val _ = if result = 197 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
