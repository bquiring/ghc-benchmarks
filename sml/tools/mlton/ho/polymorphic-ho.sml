(* 3cps higher-order/polymorphic-ho.sml, with a call to main: the MLton reference *)
(* polymorphic-ho.sml *)

fun id x      = x
fun const x _ = x
fun apply f x = f x
fun flip f (x, y) = f (y, x)
fun twice f x = f (f x)

fun mapOption f opt = case opt of NONE => NONE | SOME x => SOME (f x)
fun mapPair (f, g) (x, y) = (f x, g y)

fun revAppend (xs, ys) = case xs of [] => ys | x::xr => revAppend (xr, x::ys)
fun rev xs = revAppend (xs, [])
fun length xs = case xs of [] => 0 | _::rest => 1 + length rest
fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr
fun sum xs = foldl (fn (x, a) => x + a) 0 xs
fun map f xs = case xs of [] => [] | x::xr => f x :: map f xr

fun loop n =
  let
    (* apply at int->int *)
    val r1 = apply (fn x => x + 1) 5          (* 6 *)
    (* apply at (int list)->(int list), reduce to length *)
    val r2 = length (apply rev [1,2,3])        (* 3 *)
    (* apply at bool->int *)
    val r3 = apply (fn b => if b then 1 else 0) true   (* 1 *)

    (* twice at int->int *)
    val r4 = twice (fn x => x + 1) 3           (* 5 *)
    (* twice at list->list, reduce to length *)
    val r5 = length (twice rev [1,2,3])         (* 3 -- rev(rev(xs))=xs *)

    (* mapOption at int option *)
    val r6 = case mapOption (fn x => x * 2) (SOME 5) of
               SOME x => x | NONE => 0          (* 10 *)
    (* mapOption on NONE *)
    val r7 = case mapOption (fn x => x + 1) NONE of
               SOME x => x | NONE => 0          (* 0 *)

    (* mapPair at (int * int) *)
    val (r8a, r8b) = mapPair (fn x => x * 3, fn x => x + 1) (4, 5)
    val r8 = r8a + r8b                          (* 12 + 6 = 18 *)

    (* const: result ignores second arg -- useless-elimination target *)
    val r9 = const 42 999                       (* 42 *)

    (* flip: swap pair elements before calling f *)
    val r10 = flip (fn (x, y) => x - y) (1, 10)   (* 10 - 1 = 9 *)

    (* List of int->int functions using id, twice, apply -- same data structure *)
    val fns = [id, twice (fn x => x + 1), apply (fn x => x + 1)]
    val r11 = sum (map (fn f => f 5) fns)      (* 5 + 7 + 6 = 18 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10 + r11
    (* 6+3+1+5+3+10+0+18+42+9+18 = 115 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 6907535
      val _ = if result = 115 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
