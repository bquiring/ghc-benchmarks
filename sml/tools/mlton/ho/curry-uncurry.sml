(* 3cps higher-order/curry-uncurry.sml, with a call to main: the MLton reference *)
(* curry-uncurry.sml *)

fun add3 a b c = a + b + c
fun mul4 a b c d = a * b * c * d

(* Explicit curried form *)
val add3' = fn a => fn b => fn c => a + b + c

fun revAppend (xs, ys) = case xs of [] => ys | x::xr => revAppend (xr, x::ys)
fun map f xs = case xs of [] => [] | x::xr => f x :: map f xr
fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr
fun sum xs = foldl (fn (x, acc) => x + acc) 0 xs

fun loop n =
  let
    (* Saturated calls: all args at once -- uncurrying candidates *)
    val s1 = add3 1 2 3          (* 6 *)
    val s2 = mul4 2 3 4 5        (* 120 *)
    val s3 = add3' 10 20 30      (* 60 *)

    (* Partial applications that genuinely escape *)
    val addTen = add3 10         (* fn b => fn c => 10+b+c *)
    val inc     = add3 1 0       (* fn c => 1+c *)
    val r1 = addTen 1 2          (* 13 *)
    val r2 = inc 3               (* 4 *)

    (* Partially-applied closures in a list, mapped over arg 10.
       Same curried fn flows to both known-saturated (s1) and
       unknown-escaping (list map) call sites. *)
    val partials = [add3 1 0, add3 2 0, add3 3 0]
    val results  = map (fn f => f 10) partials   (* [11, 12, 13] *)

    val total = s1 + s2 + s3 + r1 + r2 + sum results
    (* 6 + 120 + 60 + 13 + 4 + 36 = 239 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 23077555
      val _ = if result = 239 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
