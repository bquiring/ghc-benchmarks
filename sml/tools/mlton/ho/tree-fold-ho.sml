(* 3cps higher-order/tree-fold-ho.sml, with a call to main: the MLton reference *)
(* tree-fold-ho.sml *)

datatype tree = Leaf of int | Node of tree * tree

(* Generic catamorphism: multiple function pairs flow to one recursive HOF *)
fun foldTree (leaf, node) t =
  case t of
    Leaf x      => leaf x
  | Node (l, r) => node (foldTree (leaf, node) l,
                          foldTree (leaf, node) r)

(* mapTree layered on top of foldTree -- HOFs on HOFs *)
fun mapTree f t =
  case t of
    Leaf x      => Leaf (f x)
  | Node (l, r) => Node (mapTree f l, mapTree f r)

fun maxInt (a : int, b : int) = if a > b then a else b

(* Fibonacci-like tree from seed; leaf values are all 1 *)
fun buildTree 0 = Leaf 1
  | buildTree 1 = Node (Leaf 1, Leaf 1)
  | buildTree n = Node (buildTree (n - 1), buildTree (n - 2))

(* Sum leaves of an int tree *)
fun sumTree t =
  foldTree (fn x => x, fn (l, r) => l + r) t

fun loop n =
  let
    val t = buildTree 5
    (* Fibonacci counts: bt5 has fib(7)=13 leaves *)

    (* Four instantiations of foldTree over the SAME tree t *)
    val tsum   = foldTree (fn x => x,        fn (l, r) => l + r)           t
    (* 13 -- sum of 13 leaves each = 1 *)

    val tcount = foldTree (fn _ => 1,         fn (l, r) => l + r)           t
    (* 13 *)

    val tdepth = foldTree (fn _ => 1,         fn (l, r) => 1 + maxInt (l, r)) t
    (* 6 -- depth of Fibonacci tree of depth 5 *)

    (* mapTree: increment all leaves, then sum *)
    val t2     = mapTree (fn x => x + 1) t
    val tsum2  = foldTree (fn x => x, fn (l, r) => l + r) t2
    (* 13 leaves each = 2; sum = 26 *)

    val total = tsum + tcount + tdepth + tsum2
    (* 13 + 13 + 6 + 26 = 58 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 1622010
      val _ = if result = 58 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
