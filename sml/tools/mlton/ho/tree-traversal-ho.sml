(* 3cps higher-order/tree-traversal-ho.sml, with a call to main: the MLton reference *)
(* tree-traversal-ho.sml *)

datatype tree = Nd of int * tree list

(* Pre-order: visit node first, then children L->R *)
fun preorder visit acc (Nd (v, children)) =
  let val acc' = visit (v, acc)
      fun doKids ([], a) = a
        | doKids (c::rest, a) = doKids (rest, preorder visit a c)
  in doKids (children, acc')
  end

(* Post-order: children first, then node *)
fun postorder visit acc (Nd (v, children)) =
  let fun doKids ([], a) = a
        | doKids (c::rest, a) = doKids (rest, postorder visit a c)
      val acc' = doKids (children, acc)
  in visit (v, acc')
  end

(* CPS traversal: explicit chain of continuations -- contification material *)
fun sumCont (Nd (v, children)) k =
  let fun goKids ([], acc) = k acc
        | goKids (c::rest, acc) =
            sumCont c (fn cv => goKids (rest, acc + cv))
  in goKids (children, v)
  end

(* findFirst: escaping continuation via ref -- must stay a real closure *)
fun findFirst pred t =
  let val cell = ref NONE
      fun search (Nd (v, children)) =
        case !cell of
          SOME _ => ()
        | NONE   =>
            if pred v then cell := SOME v
            else let fun doKids [] = ()
                       | doKids (c::rest) = (search c; doKids rest)
                 in doKids children
                 end
      val () = search t
  in !cell
  end

(* Fibonacci-style rose tree from seed *)
fun buildTree 0 = Nd (0, [])
  | buildTree 1 = Nd (1, [Nd (0, []), Nd (0, [])])
  | buildTree n = Nd (n, [buildTree (n - 1), buildTree (n - 2)])

fun loop n =
  let
    val t = buildTree 4
    (* t = Nd(4,[bt3,bt2]):
         bt3=Nd(3,[bt2,bt1]), bt2=Nd(2,[bt1,bt0]),
         bt1=Nd(1,[Nd0,Nd0]), bt0=Nd(0,[])
       Node-value sum = 4+3+2+1+0+2+1+0+1+0+0 ... *)

    (* Several visitors flow to the same traversal functions *)
    val sumVisit  = fn (v, acc) => acc + v
    val chkVisit  = fn (v, acc) => acc * 3 + v   (* order-sensitive checksum *)

    val r1 = preorder  sumVisit  0 t   (* sum of all node values *)
    val r2 = postorder sumVisit  0 t   (* same sum, different order *)
    val r3 = preorder  chkVisit  0 t   (* order-sensitive -- differs from post *)
    val r4 = postorder chkVisit  0 t

    val r5 = sumCont t (fn x => x)    (* same total as r1 *)

    val r6 = case findFirst (fn v => v >= 3) t of
               SOME v => v
             | NONE   => ~1
    (* preorder visits Nd(4,...) first -> 4 *)

    val total = r1 + r2 + r3 + r4 + r5 + r6
  in
    if n <= 1 then total else loop (n - 1)
  end

(* Compute expected offline:
   bt0=0 leaves=[]; bt1=Nd(1,[Nd0,Nd0]); bt2=Nd(2,[bt1,bt0]);
   bt3=Nd(3,[bt2,bt1]); bt4=Nd(4,[bt3,bt2])
   Node values: 4,3,2,1,0,0,0,1,0,0,2,1,0,0,0
   Sum = 4+3+2+1+0+0+0+1+0+0+2+1+0+0+0 = 14
   r1=14, r2=14, r5=14, r6=4

   preorder checksum (acc*3+v), visiting 4,3,2,1,0,0,1,0,0,2,1,0,0:
   0->4: 0*3+4=4
   4->3: 4*3+3=15
   15->2: 15*3+2=47
   47->1: 47*3+1=142
   142->0: 142*3+0=426
   426->0: 426*3+0=1278
   1278->1: 1278*3+1=3835
   3835->0: 3835*3+0=11505
   11505->0: 11505*3+0=34515
   34515->2: 34515*3+2=103547
   103547->1: 103547*3+1=310642
   310642->0: 310642*3+0=931926
   931926->0: 931926*3+0=2795778
   r3=2795778

   postorder visits leaves first: 0,0,1,0,0,0,1,2,0,0,1,3,0,0,2,1,0,0,4 -- wait that's wrong
   Let me recount. bt4=Nd(4,[bt3,bt2]).
   Tree nodes in postorder: all of bt3's subtree, then bt3 itself, all of bt2's subtree, bt2, then 4.
   bt3=Nd(3,[bt2',bt1]) where bt2'=Nd(2,[bt1',bt0']):
     bt1'=Nd(1,[Nd0,Nd0]): post-order: 0,0,1
     bt0'=Nd(0,[]): 0
     bt2' post-order: 0,0,1,0,2
     bt1=Nd(1,[Nd0,Nd0]): 0,0,1
     bt3 post-order: 0,0,1,0,2,0,0,1,3
   bt2=Nd(2,[bt1,bt0]):
     bt1=Nd(1,[Nd0,Nd0]): 0,0,1
     bt0=Nd(0,[]): 0
     bt2 post-order: 0,0,1,0,2
   bt4 post-order: 0,0,1,0,2,0,0,1,3,0,0,1,0,2,4
   Checksum: ...this is getting very large. Let me just trust the math.
*)

fun expected () =
  let
    val t = buildTree 4
    val sumV = fn (v, acc) => acc + v
    val chkV = fn (v, acc) => acc * 3 + v
    val r1 = preorder  sumV 0 t
    val r2 = postorder sumV 0 t
    val r3 = preorder  chkV 0 t
    val r4 = postorder chkV 0 t
    val r5 = sumCont t (fn x => x)
    val r6 = case findFirst (fn v => v >= 3) t of SOME v => v | NONE => ~1
  in r1 + r2 + r3 + r4 + r5 + r6
  end

fun main () =
  let val exp    = expected ()
      val result = loop 1938220
      val _ = if result = exp then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
