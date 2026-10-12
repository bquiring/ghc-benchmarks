(* 3cps higher-order/environment-sharing.sml, with a call to main: the MLton reference *)
(* environment-sharing.sml *)

(* Tower of nested functions: each inner closure captures a subset of v1..v5.
   Forces the optimizer to choose between shared/linked and flat environments. *)
fun tower v1 v2 v3 v4 v5 =
  let
    val f12    = fn x => v1 + v2 + x           (* subset {v1,v2} *)
    val f345   = fn x => v3 + v4 + v5 + x      (* subset {v3,v4,v5} *)
    val fall   = fn x => v1 + v2 + v3 + v4 + v5 + x  (* all vars *)
    val f1only = fn x => v1 + x                (* only v1 *)
  in
    (f12, f345, fall, f1only)
  end

(* Siblings sharing overlapping free-var prefixes vs. disjoint sets *)
fun siblings a b c =
  let
    val s1 = fn x => a + b + x    (* shares a,b -- common prefix payoff *)
    val s2 = fn x => a + c + x    (* shares a,c *)
    val s3 = fn x => b + c + x    (* shares b,c *)
    val s4 = fn x => a + x        (* disjoint from s3 -- no sharing benefit *)
    val s5 = fn x => b + x        (* disjoint from s2 *)
  in
    (s1, s2, s3, s4, s5)
  end

(* Space-safety probe: 'big' is bound in scope but the returned closure
   captures ONLY 'small'.  A correct optimizer must not keep 'big' alive. *)
fun spaceSafe a =
  let val big   = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]   (* never referenced below *)
      val small  = a + 7
      val _      = big   (* force binding to exist, but closure must not keep it *)
  in fn x => small + x
  end

fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr

fun loop n =
  let
    val (f12, f345, fall, f1only) = tower 1 2 3 4 5
    val r1 = f12    10   (* 1+2+10  = 13 *)
    val r2 = f345   10   (* 3+4+5+10 = 22 *)
    val r3 = fall   10   (* 1+2+3+4+5+10 = 25 *)
    val r4 = f1only 10   (* 1+10    = 11 *)

    val (s1, s2, s3, s4, s5) = siblings 3 4 5
    val r5 = s1 1   (* 3+4+1 = 8  *)
    val r6 = s2 1   (* 3+5+1 = 9  *)
    val r7 = s3 1   (* 4+5+1 = 10 *)
    val r8 = s4 1   (* 3+1   = 4  *)
    val r9 = s5 1   (* 4+1   = 5  *)

    val sf  = spaceSafe 3   (* fn x => 10 + x *)
    val r10 = sf 2          (* 12 *)

    (* All closures escape: store in list, apply via fold *)
    val cls = [f12, f345, fall, f1only, s1, s2, s3, s4, s5, sf]
    val r11 = foldl (fn (f, acc) => acc + f 0) 0 cls
    (* f12 0=3, f345 0=12, fall 0=15, f1only 0=1, s1 0=7, s2 0=8,
       s3 0=9, s4 0=3, s5 0=4, sf 0=10  -> sum=72 *)

    val total = r1+r2+r3+r4+r5+r6+r7+r8+r9+r10+r11
    (* 13+22+25+11+8+9+10+4+5+12+72 = 191 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 8064612
      val _ = if result = 191 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
