(* 3cps higher-order/cps-explicit.sml, with a call to main: the MLton reference *)
(* cps-explicit.sml *)

(* Factorial in CPS: chain of fn r => k(n*r) continuations *)
fun factK (n, k) =
  if n = 0 then k 1
  else factK (n - 1, fn r => k (n * r))

(* Fibonacci in CPS: binary branching continuations *)
fun fibK (n, k) =
  if n < 2 then k n
  else fibK (n - 1, fn a => fibK (n - 2, fn b => k (a + b)))

(* CPS map: per-element continuation is a known local lambda *)
fun mapK (f, xs, k) =
  case xs of
    []      => k []
  | x::rest => f (x, fn y => mapK (f, rest, fn ys => k (y :: ys)))

(* CPS foldl *)
fun foldlK (f, acc, xs, k) =
  case xs of
    []      => k acc
  | x::rest => f (acc, x, fn acc' => foldlK (f, acc', rest, k))

(* Sum a list *)
fun sumList xs = case xs of [] => 0 | x::rest => x + sumList rest

(* Escaping continuation: save k in a ref, invoke it later.
   This continuation must remain a real heap closure -- it genuinely escapes. *)
fun withSavedK body =
  let val saved = ref (fn x : int => 0)
      val _     = body (fn k => (saved := k; 0))
  in (!saved) 10
  end

fun loop n =
  let
    val r1 = factK (6, fn x => x)       (* 720 *)
    val r2 = fibK  (8, fn x => x)       (* 21  *)

    (* mapK over [1,2,3,4] with doubling, sum result *)
    val r3 = mapK (fn (x, k) => k (x * 2), [1, 2, 3, 4],
                   fn ys => sumList ys)
    (* [2,4,6,8], sum = 20 *)

    (* foldlK: sum [1..5] *)
    val r4 = foldlK (fn (acc, x, k) => k (acc + x), 0, [1,2,3,4,5],
                     fn acc => acc)
    (* 15 *)

    (* Escaping continuation: body saves fn x=>x*3; we invoke it with 10 *)
    val r5 = withSavedK (fn capture =>
               let val _ = capture (fn x => x * 3)
               in 0
               end)
    (* !saved 10 = 10*3 = 30 *)

    val total = r1 + r2 + r3 + r4 + r5
    (* 720 + 21 + 20 + 15 + 30 = 806 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 1367269
      val _ = if result = 806 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
