(* 3cps higher-order/continuation-combinators.sml, with a call to main: the MLton reference *)
(* continuation-combinators.sml *)

(* Continuation monad: 'a m = (int -> int) -> int  (answer type = int) *)
fun mReturn x k    = k x
fun mBind   m f  k = m (fn x => f x k)
fun mRun    m      = m (fn x => x)

(* State monad: 'a stm = int -> ('a * int) *)
fun stReturn x s       = (x, s)
fun stBind m f s       = let val (a, s') = m s in f a s' end
fun stGet s            = (s, s)
fun stPut ns           = fn _ => ((), ns)
fun stRun m init       = let val (v, _) = m init in v end

(* callcc-style: body receives a "capture" fn; captured k is saved and reused *)
fun withEscapingK body =
  let val cell = ref (fn x : int => x)
      val _    = body (fn k => (cell := k; 0))
  in (!cell) 7
  end

fun loop n =
  let
    (* Chain of binds: long nest of fn x => f x k closures *)
    val r1 = mRun (mBind (mReturn 3) (fn a =>
                   mBind (mReturn 4) (fn b =>
                   mReturn (a + b))))
    (* 7 *)

    val r2 = mRun (mBind (mReturn 10) (fn a =>
                   mBind (mReturn 5)  (fn b =>
                   mBind (mReturn (a - b)) (fn c =>
                   mReturn (a + b + c)))))
    (* 10+5+(10-5) = 20 *)

    (* State monad: thread an int state *)
    val r3 = stRun (stBind stGet       (fn a =>    (* a = 10 *)
                    stBind (stPut (a + 5)) (fn _ =>
                    stBind stGet       (fn b =>    (* b = 15 *)
                    stReturn (a + b))))) 10
    (* 10 + 15 = 25 *)

    (* Escaping continuation: body saves fn x=>x*3; then we call it with 7 *)
    val r4 = withEscapingK (fn capture =>
               let val _ = capture (fn x => x * 3)
               in 0
               end)
    (* 7 * 3 = 21 *)

    val total = r1 + r2 + r3 + r4
    (* 7 + 20 + 25 + 21 = 73 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 5644178
      val _ = if result = 73 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
