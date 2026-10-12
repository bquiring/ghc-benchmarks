(* 3cps higher-order/dpll-sat.sml, with a call to main: the MLton reference *)
(* dpll-sat.sml
 * DPLL SAT solver with CPS backtracking.
 * sk/fk continuations: some contifiable (innermost binds), fk genuinely escapes.
 *)

(* ---- Partial assignment as sorted-list map: var -> bool ---- *)
fun mapFind (m, k) = case m of
    [] => NONE
  | (k', v)::rest => if k = k' then SOME v
                     else if k > k' then mapFind (rest, k) else NONE

fun mapIns (m, k, v) = case m of
    [] => [(k, v)]
  | (k', v')::rest =>
      if k = k' then (k, v)::rest
      else if k < k' then (k, v)::m
      else (k', v') :: mapIns (rest, k, v)

(* ---- CNF: literal = signed int (positive = true, negative = false polarity) ----
   var(lit) = abs(lit) -- we represent neg as 0-lit *)
fun litVar x = if x > 0 then x else 0 - x
fun litSign x = x > 0

fun absInt x = if x < 0 then 0 - x else x

(* Evaluate literal under partial assignment.
   Returns SOME true/false if assigned, NONE if unassigned. *)
fun evalLit (assign, lit) =
  case mapFind (assign, litVar lit) of
    NONE => NONE
  | SOME b => SOME (if litSign lit then b else not b)

(* Simplify a clause: remove false literals, return NONE if clause is satisfied *)
fun simpClause (assign, clause) =
  let fun go [] acc = SOME acc   (* no satisfied literal; return shortened clause *)
        | go (lit::rest) acc =
            case evalLit (assign, lit) of
              SOME true  => NONE             (* clause satisfied *)
            | SOME false => go rest acc      (* literal false, drop it *)
            | NONE       => go rest (lit::acc)  (* unassigned, keep *)
  in go clause []
  end

(* Simplify formula: drop satisfied clauses, shrink others.
   Returns NONE if a conflict (empty clause) is found. *)
fun simpFormula (assign, formula) =
  let fun go [] acc = SOME acc
        | go (cl::rest) acc =
            case simpClause (assign, cl) of
              NONE     => go rest acc        (* clause satisfied, drop *)
            | SOME []  => NONE              (* empty clause = conflict *)
            | SOME cl' => go rest (cl'::acc)
  in go formula []
  end

(* Unit propagation: find a unit clause and force its literal *)
fun unitProp (formula, assign) =
  let fun findUnit cls = case cls of
        [] => NONE
      | [lit]::_ => SOME lit
      | _::rest => findUnit rest
  in case findUnit formula of
       NONE => SOME (formula, assign)
     | SOME lit =>
         let val assign' = mapIns (assign, litVar lit, litSign lit)
         in case simpFormula (assign', formula) of
              NONE    => NONE             (* conflict after unit prop *)
            | SOME f' => unitProp (f', assign')
         end
  end

(* Choose first unassigned variable (heuristic passed as function) *)
fun chooseFirst formula assign =
  case formula of
    [] => ~1
  | (lit::_)::_ =>
      let val v = litVar lit
      in case mapFind (assign, v) of
           NONE   => v
         | SOME _ => chooseFirst (case formula of []=>[] | _::rest=>rest) assign
      end
  | _::rest => chooseFirst rest assign

(* ---- Core DPLL with CPS backtracking ----
   sk : assign -> 'a   (success continuation)
   fk : unit  -> 'a   (failure continuation -- genuinely escapes into closure)
*)
fun dpll (choose, formula, assign, sk, fk) =
  case simpFormula (assign, formula) of
    NONE     => fk ()     (* conflict *)
  | SOME []  => sk assign  (* all clauses satisfied *)
  | SOME f   =>
      case unitProp (f, assign) of
        NONE => fk ()     (* unit prop found conflict *)
      | SOME (f', assign') =>
          case simpFormula (assign', f') of
            NONE    => fk ()
          | SOME [] => sk assign'
          | SOME f'' =>
              let val v = choose f'' assign'
              in if v < 0 then fk ()
                 else
                   (* Try v=true; if fails, fk tries v=false *)
                   dpll (choose, f'',
                         mapIns (assign', v, true),
                         sk,
                         fn () =>
                           dpll (choose, f'',
                                 mapIns (assign', v, false),
                                 sk, fk))
              end

(* ---- Verify assignment satisfies formula ---- *)
fun verify (formula, assign) =
  let fun clauseOk cl = case cl of
        [] => false
      | lit::rest =>
          case evalLit (assign, lit) of
            SOME true => true
          | _ => clauseOk rest
      fun go cls = case cls of [] => true | cl::rest => clauseOk cl andalso go rest
  in go formula
  end

(* ---- Test battery ---- *)

(* SAT: (x1 v x2) /\ (~x1 v x3) /\ (~x2 v ~x3)
   Variables: 1=x1, 2=x2, 3=x3
   Satisfiable e.g. x1=true,x2=false,x3=true *)
val satFormula = [[1, 2], [~1, 3], [~2, ~3]]

(* UNSAT: (x1) /\ (~x1) *)
val unsatFormula = [[1], [~1]]

fun solve (choose, formula) =
  dpll (choose, formula, [],
        fn assign => SOME assign,
        fn ()     => NONE)

fun loop n =
  let
    val satResult  = solve (chooseFirst, satFormula)
    val r1 = case satResult of
               NONE       => 0   (* unexpected UNSAT *)
             | SOME assign =>
                 if verify (satFormula, assign) then 1 else 0

    val unsatResult = solve (chooseFirst, unsatFormula)
    val r2 = case unsatResult of
               NONE   => 1    (* correctly found UNSAT *)
             | SOME _ => 0

    val total = r1 + r2   (* expected: 2 *)
  in
    if n <= 1 then total else loop (n - 1)
  end

fun main () =
  let val result = loop 1647267
      val _ = if result = 2 then print "pass\n" else print "fail\n"
  in () end

val _ = main ()
