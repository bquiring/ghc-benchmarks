(* 3cps higher-order/closure-capture.sml, with a call to main: the MLton reference *)
(* closure-capture.sml *)

(* Nested scopes: inner closure captures vars from multiple outer levels *)
fun outer a =
  let val b = a + 1
  in fn c =>
       let val d = b * 2
       in fn e => a + b + c + d + e
       end
  end
(* outer 3 10 5 : a=3,b=4,c=10,d=8,e=5 -> 3+4+10+8+5 = 30 *)

(* Sibling closures sharing overlapping free-var sets *)
fun makeSiblings x y =
  let
    val f = fn z => x + y + z   (* captures x and y *)
    val g = fn z => x * z       (* captures x only; y is NOT in env *)
    val h = fn z => x + y + x * z  (* captures x and y *)
  in
    (f, g, h)
  end

(* A closure that captures a "useless" variable (bound but never used) *)
fun withUseless a =
  let val useless = a * 1000   (* never referenced below *)
      val useful  = a + 7
  in fn x => useful + x        (* only 'useful' in environment *)
  end

fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr

fun loop n =
  let
    (* Escaping closures: built inside loop, applied after outer returns *)
    val inner = outer 3 10       (* fn e => 25 + e *)
    val r1    = inner 5           (* 30 *)

    val (f, g, h) = makeSiblings 3 4
    val r2 = f 1                  (* 3+4+1  = 8  *)
    val r3 = g 2                  (* 3*2    = 6  *)
    val r4 = h 1                  (* 3+4+3  = 10 *)

    val wf = withUseless 5        (* fn x => 12 + x *)
    val r5 = wf 8                 (* 20 *)

    (* Store closures in a list and apply them to confirm they escape *)
    val cls = [inner, wf]
    val r6  = foldl (fn (f, acc) => acc + f 1) 0 cls
    (* inner 1 = 26, wf 1 = 13; total = 39 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6
    (* 30 + 8 + 6 + 10 + 20 + 39 = 113 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 26069491
      val _ = if result = 113 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
