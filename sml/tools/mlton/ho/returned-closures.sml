(* 3cps higher-order/returned-closures.sml, with a call to main: the MLton reference *)
(* returned-closures.sml *)

(* Function factories: return closures that capture factory arguments *)
fun adder  n     = fn x => x + n
fun scaler k     = fn x => x * k
fun affine (a,b) = fn x => a * x + b   (* captures two values *)

(* Nested closures: twice captures another returned closure *)
fun twice f = fn x => f (f x)

(* Factory using only one of three params -- b and c must be dropped *)
fun ignoreSecond (a, b, c) = fn x => a + x   (* b, c unused *)

fun map f xs = case xs of [] => [] | x::xr => f x :: map f xr
fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr
fun sum xs = foldl (fn (x, acc) => x + acc) 0 xs

fun loop n =
  let
    (* Build a set of escaping closures *)
    val add3   = adder 3           (* fn x => x+3  *)
    val scale5 = scaler 5          (* fn x => x*5  *)
    val aff    = affine (2, 1)     (* fn x => 2x+1 *)
    val incInc = twice (adder 1)   (* fn x => x+2  -- closure-in-closure *)
    val dropBoth = ignoreSecond (10, 99, 999)  (* fn x => 10+x *)

    (* Collect into list; apply all to 4 (long after factory frame is gone) *)
    val closures = [add3, scale5, aff, incInc, dropBoth]
    val applied4 = map (fn f => f 4) closures
    (* [7, 20, 9, 6, 14] *)
    val r1 = sum applied4           (* 56 *)

    (* Map same closures over a second input list *)
    val inputs = [1, 2, 3]
    fun applyAll fs x = sum (map (fn f => f x) fs)
    val r2 = sum (map (applyAll closures) inputs)
    (* x=1: 4+5+3+3+11=26; x=2: 5+10+5+4+12=36; x=3: 6+15+7+5+13=46; total=108 *)

    val total = r1 + r2
    (* 56 + 108 = 164 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 2412969
      val _ = if result = 164 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
