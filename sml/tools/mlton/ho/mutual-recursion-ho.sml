(* 3cps higher-order/mutual-recursion-ho.sml, with a call to main: the MLton reference *)
(* mutual-recursion-ho.sml *)

(* Even/odd generalized with a "what to do at base case" continuation *)
fun evenK (n, k) = if n = 0 then k true  else oddK  (n - 1, k)
and oddK  (n, k) = if n = 0 then k false else evenK (n - 1, k)

fun boolToInt b = if b then 1 else 0

(* Mutually recursive tree traversal over two alternating node shapes.
   The two traversal functions exchange a folding function + accumulator. *)
datatype tree = LeafT of int | NodeA of tree * tree | NodeB of int * tree

fun travA (t, f, acc) = travB (t, f, acc)
and travB (t, f, acc) =
  case t of
    LeafT x      => f (x, acc)
  | NodeA (l, r) => travB (r, f, travB (l, f, acc))
  | NodeB (k, c) => travB (c, f, f (k, acc))

(* One member passes a freshly-created closure on each step.
   Flow analysis should see the closure is loop-invariant in effect. *)
fun evenStep (n, k) =
  if n = 0 then k 0
  else oddStep (n - 1, fn x => k (x + 1))
and oddStep (n, k) =
  if n = 0 then k 0
  else evenStep (n - 1, fn x => k (x + 2))
(* evenStep(4, id): builds +1+2+1+2 = +6 on top of 0 -> 6 *)

fun loop n =
  let
    val r1 = evenK (6, boolToInt)    (* 6 is even  -> 1 *)
    val r2 = oddK  (5, boolToInt)    (* 5 is odd   -> 1 *)
    val r3 = evenK (3, boolToInt)    (* 3 not even -> 0 *)

    (* Tree with alternating node types *)
    val t = NodeA (NodeB (5, LeafT 3), NodeA (LeafT 2, LeafT 4))
    val r4 = travB (t, fn (x, acc) => acc + x, 0)
    (* 5+3+2+4 = 14 *)

    (* Always-same known callback: + accumulator (single flow target) *)
    val r5 = travB (NodeA (LeafT 7, NodeB (3, LeafT 2)),
                    fn (x, acc) => acc + x, 0)
    (* 7+3+2 = 12 *)

    val r6 = evenStep (4, fn x => x)   (* 6 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6
    (* 1 + 1 + 0 + 14 + 12 + 6 = 34 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 13913939
      val _ = if result = 34 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
