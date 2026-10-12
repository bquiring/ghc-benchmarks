(* 3cps higher-order/applicative-functor.sml, with a call to main: the MLton reference *)
(* applicative-functor.sml *)

(* ---- Option applicative ---- *)
fun pureO x = SOME x
fun mapO f ox = case ox of NONE => NONE | SOME x => SOME (f x)
fun apO (SOME f, SOME x) = SOME (f x)
  | apO _                = NONE
fun liftA2O f a b = apO (mapO f a, b)
fun liftA3O f a b c = apO (liftA2O f a b, c)

(* traverse: sequence a list through the Option applicative *)
fun traverseO f xs =
  case xs of
    []      => SOME []
  | x::rest => liftA2O (fn y => fn ys => y :: ys) (f x) (traverseO f rest)

(* ---- Validation applicative: accumulates errors ---- *)
datatype 'a result = Ok of 'a | Err of int list

fun append (xs, ys) = case xs of [] => ys | x::xr => x :: append (xr, ys)

fun mapV f r = case r of Err e => Err e | Ok x => Ok (f x)
fun apV (Ok f,   Ok x)   = Ok (f x)
  | apV (Err e1, Err e2) = Err (append (e1, e2))
  | apV (Err e,  _)      = Err e
  | apV (_,      Err e)  = Err e
fun liftA2V f a b = apV (mapV f a, b)
fun liftA3V f a b c = apV (liftA2V f a b, c)

fun foldl f acc xs = case xs of [] => acc | x::xr => foldl f (f (x, acc)) xr
fun sumList xs = foldl (fn (x, a) => x + a) 0 xs
fun orElse opt def = case opt of NONE => def | SOME x => x

fun loop n =
  let
    (* Option: all present *)
    val r1 = orElse (liftA3O (fn a => fn b => fn c => a + b + c)
                              (SOME 1) (SOME 2) (SOME 3))
                    ~1
    (* 6 *)

    (* Option: one missing -> NONE, observe as 0 *)
    val r2 = orElse (liftA2O (fn a => fn b => a + b) (SOME 10) NONE) 0
    (* 0 *)

    (* Traverse option: double each element if positive *)
    val r3 = orElse
               (case traverseO (fn x => if x > 0 then SOME (x * 2) else NONE)
                               [1, 2, 3] of
                  NONE    => NONE
                | SOME xs => SOME (sumList xs))
               ~1
    (* SOME [2,4,6], sum = 12 *)

    (* Traverse fails on negative element *)
    val r4 = case traverseO (fn x => if x > 0 then SOME x else NONE)
                            [1, ~1, 3] of
               NONE   => 0
             | SOME _ => ~1
    (* NONE -> 0 *)

    (* Validation: both Ok *)
    val r5 = case liftA3V (fn a => fn b => fn c => a + b + c)
                          (Ok 4) (Ok 5) (Ok 6) of
               Ok x  => x
             | Err _  => ~1
    (* 15 *)

    (* Validation: two errors -> both accumulated *)
    val r6 = case liftA2V (fn a => fn b => a + b)
                          (Err [1, 2]) (Err [3]) of
               Ok _   => ~1
             | Err es => sumList es
    (* Err [1,2,3], sum = 6 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6
    (* 6 + 0 + 12 + 0 + 15 + 6 = 39 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 2513421
      val _ = if result = 39 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
