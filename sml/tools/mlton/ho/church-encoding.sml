(* 3cps higher-order/church-encoding.sml, with a call to main: the MLton reference *)
(* church-encoding.sml *)

(* Church numerals specialized to int: (int->int)->int->int *)
fun czero f x = x
fun csucc n f x = f (n f x)
fun cplus m n f x = m f (n f x)
fun cmult m n f x = m (n f) x

fun realize n = n (fn x => x + 1) 0

(* Build numerals 0..5 using fun so each binding is a syntactic value
   (value restriction: val c1 = csucc c0 would be monomorphic and fail
   when passed to csucc again) *)
fun c0 f x = x
fun c1 f x = f x
fun c2 f x = f (f x)
fun c3 f x = f (f (f x))
fun c4 f x = f (f (f (f x)))
fun c5 f x = f (f (f (f (f x))))

(* Church booleans as int->int->int: select between two int branches *)
fun ctrue  (t : int) (f : int) = t
fun cfalse (t : int) (f : int) = f
(* cand: if a=1 then b else 0; cor: if a=1 then 1 else b *)
fun cand (a : int -> int -> int) (b : int -> int -> int) t f =
  a (b t f) (cfalse t f)
(* Observe boolean: apply to 1 0 *)
fun cboolObs b = b 1 0

(* Church pairs holding ints *)
fun cpair (a : int) (b : int) s = s a b
fun cfst p = p (fn a => fn b => a)
fun csnd p = p (fn a => fn b => b)

fun loop n =
  let
    (* Numeral arithmetic *)
    val r1 = realize (cplus c3 c4)    (* 7  *)
    val r2 = realize (cmult c3 c4)    (* 12 *)
    val r3 = realize (csucc c4)       (* 5  *)
    val r4 = realize (cplus (cmult c2 c3) c1)  (* 2*3+1 = 7 *)

    (* Boolean branching: ctrue selects first arg *)
    val r5 = cboolObs ctrue           (* 1 *)
    val r6 = cboolObs cfalse          (* 0 *)
    (* Use boolean to select numeral result *)
    val r7 = ctrue r1 r2              (* 7 (ctrue picks r1) *)

    (* Pairs *)
    val p  = cpair 10 20
    val r8 = cfst p                   (* 10 *)
    val r9 = csnd p                   (* 20 *)

    (* Combined: build pair of numeral results, project *)
    val p2  = cpair r1 r2
    val r10 = cfst p2 + csnd p2       (* 7 + 12 = 19 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10
    (* 7+12+5+7+1+0+7+10+20+19 = 88 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 5623379
      val expected = 7+12+5+7+1+0+7+10+20+19   (* 88 *)
      val _ = if result = expected then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
