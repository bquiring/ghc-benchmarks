(* 3cps higher-order/callback-state.sml, with a call to main: the MLton reference *)
(* callback-state.sml *)

(* Independent counters: each closure has its OWN ref *)
fun makeCounter () =
  let val c = ref 0
  in fn () => (c := !c + 1; !c)
  end

(* Shared ref: inc and get close over the SAME cell *)
fun makeSharedPair () =
  let val cell = ref 0
  in ( fn () => (cell := !cell + 1)   (* inc *)
     , fn () => !cell )               (* get *)
  end

(* Higher-order accumulate: folds list, calls stateful callback per element *)
fun accumulate (f : int * (int -> unit) -> unit) xs =
  let val acc = ref 0
      fun go xs = case xs of
            [] => ()
          | x::rest => (f (x, fn v => acc := !acc + v); go rest)
      val () = go xs
  in !acc
  end

(* Memoizer: captures a function and a ref in one closure *)
fun memo f =
  let val cache = ref NONE
  in fn x =>
       case !cache of
         SOME v => v
       | NONE   => let val v = f x in cache := SOME v; v end
  end

fun loop n =
  let
    (* Two independent counters -- different refs *)
    val ctr1 = makeCounter ()
    val ctr2 = makeCounter ()
    val _ = ctr1 (); val _ = ctr1 (); val _ = ctr1 ()   (* ctr1 = 3 *)
    val _ = ctr2 (); val _ = ctr2 ()                     (* ctr2 = 2 *)
    val r1 = ctr1 ()   (* 4 *)
    val r2 = ctr2 ()   (* 3 *)

    (* Shared ref *)
    val (inc, get) = makeSharedPair ()
    val () = inc (); val () = inc (); val () = inc ()
    val r3 = get ()    (* 3 *)

    (* Stateful accumulate *)
    val r4 = accumulate (fn (x, k) => k (x * 2)) [1, 2, 3]
    (* 2 + 4 + 6 = 12 *)

    (* Memoizer: wraps fn x => x*x+x; first call computes, second reads cache *)
    val memFn = memo (fn x => x * x + x)
    val r5 = memFn 5   (* 30, computed *)
    val r6 = memFn 5   (* 30, cached   *)

    val total = r1 + r2 + r3 + r4 + r5 + r6
    (* 4 + 3 + 3 + 12 + 30 + 30 = 82 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 18306267
      val _ = if result = 82 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
