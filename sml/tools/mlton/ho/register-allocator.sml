(* 3cps higher-order/register-allocator.sml, with a call to main: the MLton reference *)
(* register-allocator.sml
 * Graph-coloring register allocator for a tiny 3-address IR.
 * Higher-order: liveness via fixpoint closure, coloring via spill-cost HOF.
 * Two concrete map types to avoid polymorphic equality.
 *)

(* ---- Sorted-list IntSet ---- *)
fun setEmpty () = [] : int list
fun setMem (x, xs) = case xs of
    [] => false | y::ys => x = y orelse (x > y andalso setMem (x, ys))
fun setIns (x, xs) = case xs of
    [] => [x]
  | y::ys => if x = y then xs else if x < y then x::xs else y :: setIns (x, ys)
fun setDel (x, xs) = case xs of
    [] => []
  | y::ys => if x < y then xs         (* past where x would be; not found *)
             else if x > y then y :: setDel (x, ys)
             else ys                  (* x = y, drop it *)
fun setUnion (xs, ys) = case xs of [] => ys | x::rest => setUnion (rest, setIns (x, ys))
fun setDiff (xs, ys) = case xs of
    [] => [] | x::rest => if setMem (x, ys) then setDiff (rest, ys)
                          else x :: setDiff (rest, ys)
fun setSize xs = case xs of [] => 0 | _::rest => 1 + setSize rest
fun setFoldl f acc xs = case xs of [] => acc | x::rest => setFoldl f (f (x, acc)) rest

(* ---- IntMap (int -> int list): interference graph adjacency lists ---- *)
fun igEmpty () = [] : (int * int list) list
fun igFind (g, k) = case g of
    [] => setEmpty ()
  | (k', v)::rest => if k = k' then v
                     else if k > k' then igFind (rest, k)
                     else setEmpty ()
fun igIns (g, k, v) = case g of
    [] => [(k, v)]
  | (k', v')::rest =>
      if k = k' then (k, v)::rest
      else if k < k' then (k, v)::g
      else (k', v') :: igIns (rest, k, v)

(* ---- IntMap (int -> int): register coloring ---- *)
fun cmEmpty () = [] : (int * int) list
fun cmFind (m, k) = case m of
    [] => NONE
  | (k', v)::rest => if k = k' then SOME v
                     else if k > k' then cmFind (rest, k)
                     else NONE
fun cmIns (m, k, v) = case m of
    [] => [(k, v)]
  | (k', v')::rest =>
      if k = k' then (k, v)::rest
      else if k < k' then (k, v)::m
      else (k', v') :: cmIns (rest, k, v)

(* ---- IR ---- *)
datatype instr
  = MOV of int * int
  | ADD of int * int * int
  | SUB of int * int * int
  | MUL of int * int * int
  | RET of int

fun defOf i = case i of
    MOV (d, _)    => setIns (d, setEmpty ())
  | ADD (d, _, _) => setIns (d, setEmpty ())
  | SUB (d, _, _) => setIns (d, setEmpty ())
  | MUL (d, _, _) => setIns (d, setEmpty ())
  | RET _         => setEmpty ()

fun useOf i = case i of
    MOV (_, s)      => setIns (s, setEmpty ())
  | ADD (_, s1, s2) => setIns (s1, setIns (s2, setEmpty ()))
  | SUB (_, s1, s2) => setIns (s1, setIns (s2, setEmpty ()))
  | MUL (_, s1, s2) => setIns (s1, setIns (s2, setEmpty ()))
  | RET s           => setIns (s, setEmpty ())

(* ---- Liveness ---- *)
fun revList xs = case xs of [] => [] | x::rest => revList rest @ [x]

(* ---- Interference graph ---- *)
fun addEdge (g, u, v) =
  if u = v then g
  else igIns (igIns (g, u, setIns (v, igFind (g, u))),
              v, setIns (u, igFind (g, v)))

fun buildIG (instrs, liveOut) =
  let fun foldEdges (liveSet, g) =
        setFoldl (fn (u, g') =>
          setFoldl (fn (v, g'') => addEdge (g'', u, v)) g' liveSet)
          g liveSet
      fun go ([], live, g) = g
        | go (i::rest, live, g) =
            let val live' = setUnion (useOf i, setDiff (live, defOf i))
                val atDef = setUnion (defOf i, live)
                val g'    = foldEdges (atDef, g)
            in go (rest, live', g')
            end
  in go (revList instrs, liveOut, igEmpty ())
  end

(* ---- Graph coloring ---- *)
fun igDegree (g, v) = setSize (igFind (g, v))

fun removeNode (g, v) =
  let val nbrs = igFind (g, v)
      fun remU (g, u) = igIns (g, u, setDel (v, igFind (g, u)))
      val g' = setFoldl (fn (u, g'') => remU (g'', u)) g nbrs
  in igIns (g', v, setEmpty ())
  end

fun listDel (v, ns) = case ns of
    [] => []
  | x::rest => if x < v orelse x > v then x :: listDel (v, rest)
               else rest            (* x = v *)

(* spillCost : int -> int  is a higher-order argument *)
fun simplify (g, nodes, k, spillCost : int -> int) =
  let fun findLow ns = case ns of
          [] => NONE
        | v::rest => if igDegree (g, v) < k then SOME v else findLow rest
  in case findLow nodes of
       NONE => (g, [], nodes)
     | SOME v =>
         let val g'    = removeNode (g, v)
             val nodes' = listDel (v, nodes)
             val (g'', stk, spills) = simplify (g', nodes', k, spillCost)
         in (g'', v :: stk, spills)
         end
  end

fun selectColor (nbrs, k, coloring) =
  let val used = setFoldl (fn (u, cs) =>
                   case cmFind (coloring, u) of NONE => cs | SOME c => setIns (c, cs))
                 (setEmpty ()) nbrs
      fun findColor c =
        if c >= k then 0 - 1
        else if setMem (c, used) then findColor (c + 1)
        else c
  in findColor 0
  end

fun colorGraph (g, nodes, k, spillCost) =
  let val (g', stk, _) = simplify (g, nodes, k, spillCost)
      fun select ([], coloring) = coloring
        | select (v::rest, coloring) =
            let val nbrs = igFind (g, v)
                val c    = selectColor (nbrs, k, coloring)
            in select (rest, cmIns (coloring, v, c))
            end
  in select (stk, cmEmpty ())
  end

(* ---- Test program: r0=r1+r2, r3=r0*r1, r4=r3-r2, ret r4 ---- *)
val prog    = [ADD (0, 1, 2), MUL (3, 0, 1), SUB (4, 3, 2), RET 4]
val liveOut = setEmpty ()

fun listNodes instrs =
  let fun collect ([], s) = s
        | collect (i::rest, s) =
            collect (rest, setUnion (defOf i, setUnion (useOf i, s)))
  in collect (instrs, setEmpty ())
  end

fun spillCost v = v

fun loop n =
  let
    val nodes    = listNodes prog
    val ig       = buildIG (prog, liveOut)
    val coloring = colorGraph (ig, nodes, 3, spillCost)

    val checksum = setFoldl (fn (v, acc) =>
                     case cmFind (coloring, v) of
                       NONE   => acc
                     | SOME c => acc + v * (c + 1))
                   0 nodes

    val numColors = setSize (setFoldl (fn (v, cs) =>
                      case cmFind (coloring, v) of
                        NONE   => cs
                      | SOME c => setIns (c, cs))
                      (setEmpty ()) nodes)
  in
    if n <= 1 then (checksum, numColors) else loop (n - 1)
  end

fun main () =
  let val (checksum, numColors) = loop 458375
      val ok = numColors <= 3 andalso numColors >= 1 andalso checksum > 0
      val _ = if ok then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
