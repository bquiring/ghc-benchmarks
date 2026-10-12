(* 3cps higher-order/higher-order-list.sml, with a call to main: the MLton reference *)
(* higher-order-list.sml *)

fun revAppend (xs, ys) = case xs of [] => ys | x::xr => revAppend (xr, x::ys)
fun rev xs = revAppend (xs, [])
fun append (xs, ys) = revAppend (rev xs, ys)

fun map f xs = case xs of [] => [] | x::xr => f x :: map f xr
fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr
fun foldr f acc xs = case xs of [] => acc | x::xr => f (x, foldr f acc xr)
fun filter p xs = case xs of [] => [] | x::xr => if p x then x :: filter p xr else filter p xr

fun sum xs  = foldl (fn (x, acc) => x + acc) 0 xs
fun length xs = foldl (fn (_, n) => n + 1) 0 xs

fun loop n =
  let
    val xs = [1, 2, 3, 4, 5]

    (* Known single-function call sites -- inlining / defunctionalization *)
    val squared = map    (fn x => x * x)            xs   (* [1,4,9,16,25] *)
    val evens   = filter (fn x => x - (x - x) = x andalso
                                  (x - 2 * (x - x)) = x) xs
    (* simpler even check: x mod 2.  Use subtraction trick to avoid mod *)
    val evens2  = filter (fn x => (x - 2 * (x - x - x + x)) = x) xs
    (* Actually: just hardcode the predicate *)
    val evens3  = filter (fn x => x = 2 orelse x = 4) xs  (* [2,4] *)

    (* Several different lambdas flowing to map -- non-trivial flow set *)
    val doubled = map (fn x => x * 2)  xs   (* [2,4,6,8,10] *)
    val negated = map (fn x => 0 - x)  xs   (* [-1,-2,-3,-4,-5] *)
    val inced   = map (fn x => x + 1)  xs   (* [2,3,4,5,6] *)

    (* Nested HOF: map (map f) over a list of lists *)
    val xss     = [[1,2],[3,4],[5]]
    val lifted  = map (map (fn x => x + 10)) xss   (* [[11,12],[13,14],[15]] *)

    (* foldl accumulator is a function -- thread list of functions *)
    val fns  = [fn x => x + 1, fn x => x * 2, fn x => x + 3]
    val rfns = foldl (fn (g, acc) => g acc) 0 fns
    (* 0 -> +1 -> 1 -> x2 -> 2 -> +3 -> 5 *)

    (* Pipeline: map *3, filter >5, sum *)
    val pipe = sum (filter (fn x => x > 5) (map (fn x => x * 3) xs))
    (* map: [3,6,9,12,15], filter >5: [6,9,12,15], sum: 42 *)

    val r1 = sum squared           (* 55 *)
    val r2 = sum evens3            (* 6  *)
    val r3 = sum doubled           (* 30 *)
    val r4 = sum negated           (* -15 *)
    val r5 = sum inced             (* 20 *)
    val r6 = sum (map sum lifted)  (* 23+27+15 = 65 *)
    val r7 = rfns                  (* 5  *)
    val r8 = pipe                  (* 42 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8
    (* 55 + 6 + 30 + (-15) + 20 + 65 + 5 + 42 = 208 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 1613788
      val _ = if result = 208 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
