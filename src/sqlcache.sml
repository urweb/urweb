structure Sqlcache :> SQLCACHE = struct

open Mono

structure IS = IntBinarySet
structure IM = IntBinaryMap
structure SK = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure SIMM = MultimapFn(structure KeyMap = SM structure ValSet = IS)

fun iterate f n x = if n < 0
                    then raise Fail "Can't iterate function negative number of times."
                    else if n = 0
                    then x
                    else iterate f (n-1) (f x)

(* Filled in by [addFlushing]. *)
val ffiInfoRef : {index : int, params : int} list ref = ref []

fun resetFfiInfo () = ffiInfoRef := []

fun getFfiInfo () = !ffiInfoRef

(* Some FFIs have writing as their only effect, which the caching records. *)
val ffiEffectful =
    (* ASK: how can this be less hard-coded? *)
    let
        val okayWrites = SS.fromList ["htmlifyInt_w",
                                      "htmlifyFloat_w",
                                      "htmlifyString_w",
                                      "htmlifyBool_w",
                                      "htmlifyTime_w",
                                      "attrifyInt_w",
                                      "attrifyFloat_w",
                                      "attrifyString_w",
                                      "attrifyChar_w",
                                      "urlifyInt_w",
                                      "urlifyFloat_w",
                                      "urlifyString_w",
                                      "urlifyBool_w",
                                      "urlifyChannel_w"]
    in
        (* ASK: is it okay to hardcode Sqlcache functions as effectful? *)
        fn (m, f) => Settings.isEffectful (m, f)
                     andalso not (m = "Basis" andalso SS.member (okayWrites, f))
    end

val cache = ref LruCache.cache
fun setCache c = cache := c
fun getCache () = !cache

(* Used to have type context for local variables in MonoUtil functions. *)
val doBind =
 fn (env, MonoUtil.Exp.RelE (x, t)) => MonoEnv.pushERel env x t NONE
  | (env, MonoUtil.Exp.NamedE (x, n, t, eo, s)) => MonoEnv.pushENamed env x n t eo s
  | (env, MonoUtil.Exp.Datatype (x, n, cs)) => MonoEnv.pushDatatype env x n cs

val dummyLoc = ErrorMsg.dummySpan


(*********************)
(* General Utilities *)
(*********************)

(* From the MLton wiki. *)
infixr 3 />     fun f /> y = fn x => f (x, y)     (* Right section     *)
infixr 3 </     fun x </ f = f x                  (* Right application *)

fun mapFst f (x, y) = (f x, y)

(* Option monad. *)
fun obind (x, f) = Option.mapPartial f x
fun oguard (b, x) = if b then x else NONE
fun omap f = fn SOME x => SOME (f x) | _ => NONE
fun omap2 f = fn (SOME x, SOME y) => SOME (f (x,y)) | _ => NONE
fun osequence ys = List.foldr (omap2 op::) (SOME []) ys

fun indexOf test =
    let
        fun f n =
         fn [] => NONE
          | (x::xs) => if test x then SOME n else f (n+1) xs
    in
        f 0
    end

(*******************)
(* Effect Analysis *)
(*******************)

(* Makes an exception for [EWrite] (which is recorded when caching). *)
fun effectful (effs : IS.set) =
    let
        val isFunction =
         fn (TFun _, _) => true
          | _ => false
        fun doExp (env, e) =
            case e of
                EPrim _ => false
              (* For now: variables of function type might be effectful, but
                 others are fully evaluated and are therefore not effectful. *)
              | ERel n => isFunction (#2 (MonoEnv.lookupERel env n))
              | ENamed n => IS.member (effs, n)
              | EFfi (m, f) => ffiEffectful (m, f)
              | EFfiApp (m, f, _) => ffiEffectful (m, f)
              (* These aren't effectful unless a subexpression is. *)
              | ECon _ => false
              | ENone _ => false
              | ESome _ => false
              | EApp _ => false
              | EAbs _ => false
              | EUnop _ => false
              | EBinop _ => false
              | ERecord _ => false
              | EField _ => false
              | ECase _ => false
              | EStrcat _ => false
              (* EWrite is a special exception because we record writes when caching. *)
              | EWrite _ => false
              | ESeq _ => false
              | ELet _ => false
              | EUnurlify _ => false
              (* ASK: what should we do about closures? *)
              (* Everything else is some sort of effect. We could flip this and
                 explicitly list bits of Mono that are effectful, but this is
                 conservatively robust to future changes (however unlikely). *)
              | _ => true
    in
        MonoUtil.Exp.existsB {typ = fn _ => false, exp = doExp, bind = doBind}
    end

(* TODO: test this. *)
fun effectfulDecls (decls, _) =
    let
        fun doVal ((_, name, _, e, _), effs) =
            if effectful effs MonoEnv.empty e
            then IS.add (effs, name)
            else effs
        val doDecl =
         fn ((DVal v, _), effs) => doVal (v, effs)
          (* Repeat the list of declarations a number of times equal to its size,
             making sure effectfulness propagates everywhere it should. This is
             analagous to the Bellman-Ford algorithm. *)
          | ((DValRec vs, _), effs) =>
            List.foldl doVal effs (List.concat (List.map (fn _ => vs) vs))
          (* ASK: any other cases? *)
          | (_, effs) => effs
    in
        List.foldl doDecl IS.empty decls
    end


(*********************************)
(* Boolean Formula Normalization *)
(*********************************)

datatype junctionType = Conj | Disj

datatype 'atom formula =
         Atom of 'atom
       | Negate of 'atom formula
       | Combo of junctionType * 'atom formula list

(* Guaranteed to have all negation pushed to the atoms. *)
datatype 'atom formula' =
         Atom' of 'atom
       | Combo' of junctionType * 'atom formula' list

val flipJt = fn Conj => Disj | Disj => Conj

fun concatMap f xs = List.concat (map f xs)

val rec cartesianProduct : 'a list list -> 'a list list =
 fn [] => [[]]
  | (xs :: xss) => concatMap (fn ys => concatMap (fn x => [x :: ys]) xs)
                             (cartesianProduct xss)

(* Pushes all negation to the atoms.*)
fun pushNegate (normalizeAtom : bool * 'atom -> 'atom) (negating : bool) =
 fn Atom x => Atom' (normalizeAtom (negating, x))
  | Negate f => pushNegate normalizeAtom (not negating) f
  | Combo (j, fs) => Combo' (if negating then flipJt j else j,
                             map (pushNegate normalizeAtom negating) fs)

val rec flatten =
 fn Combo' (_, [f]) => flatten f
  | Combo' (j, fs) =>
    Combo' (j, List.foldr (fn (f, acc) =>
                              case f of
                                  Combo' (j', fs') =>
                                  if j = j' orelse length fs' = 1
                                  then fs' @ acc
                                  else f :: acc
                                | _ => f :: acc)
                          []
                          (map flatten fs))
  | f => f

(* [simplify] operates on the desired normal form. E.g., if [junc] is [Disj],
   consider the list of lists to be a disjunction of conjunctions. *)
fun normalize' (simplify : 'a list list -> 'a list list)
               (junc : junctionType) =
    let
        fun norm junc =
            simplify
            o (fn Atom' x => [[x]]
                | Combo' (j, fs) =>
                  let
                      val fss = map (norm junc) fs
                  in
                      if j = junc
                      then List.concat fss
                      else map List.concat (cartesianProduct fss)
                  end)
    in
        norm junc
    end

fun normalize simplify normalizeAtom junc =
    normalize' simplify junc
    o flatten
    o pushNegate normalizeAtom false

fun mapFormula mf =
 fn Atom x => Atom (mf x)
  | Negate f => Negate (mapFormula mf f)
  | Combo (j, fs) => Combo (j, map (mapFormula mf) fs)


(****************)
(* SQL Analysis *)
(****************)

structure CmpKey = struct

    type ord_key = Sql.cmp

    val compare =
     fn (Sql.Eq, Sql.Eq) => EQUAL
      | (Sql.Eq, _) => LESS
      | (_, Sql.Eq) => GREATER
      | (Sql.Ne, Sql.Ne) => EQUAL
      | (Sql.Ne, _) => LESS
      | (_, Sql.Ne) => GREATER
      | (Sql.Lt, Sql.Lt) => EQUAL
      | (Sql.Lt, _) => LESS
      | (_, Sql.Lt) => GREATER
      | (Sql.Le, Sql.Le) => EQUAL
      | (Sql.Le, _) => LESS
      | (_, Sql.Le) => GREATER
      | (Sql.Gt, Sql.Gt) => EQUAL
      | (Sql.Gt, _) => LESS
      | (_, Sql.Gt) => GREATER
      | (Sql.Ge, Sql.Ge) => EQUAL

end

val rec chooseTwos : 'a list -> ('a * 'a) list =
 fn [] => []
  | x :: ys => map (fn y => (x, y)) ys @ chooseTwos ys

fun removeRedundant madeRedundantBy zs =
    let
        fun removeRedundant' (xs, ys) =
            case xs of
                [] => ys
              | x :: xs' =>
                removeRedundant' (xs',
                                  if List.exists (fn y => madeRedundantBy (x, y)) (xs' @ ys)
                                  then ys
                                  else x :: ys)
    in
        removeRedundant' (zs, [])
    end

datatype atomExp =
         QueryArg of int
       | DmlRel of int
       | Prim of Prim.t
       | Field of string * string

structure AtomExpKey : ORD_KEY = struct

    type ord_key = atomExp

    val compare =
     fn (QueryArg n1, QueryArg n2) => Int.compare (n1, n2)
      | (QueryArg _, _) => LESS
      | (_, QueryArg _) => GREATER
      | (DmlRel n1, DmlRel n2) => Int.compare (n1, n2)
      | (DmlRel _, _) => LESS
      | (_, DmlRel _) => GREATER
      | (Prim p1, Prim p2) => Prim.compare (p1, p2)
      | (Prim _, _) => LESS
      | (_, Prim _) => GREATER
      | (Field (t1, f1), Field (t2, f2)) =>
        case String.compare (t1, t2) of
            EQUAL => String.compare (f1, f2)
          | ord => ord

end

structure AtomOptionKey = OptionKeyFn(AtomExpKey)

val rec tablesOfQuery =
 fn Sql.Query1 {From = tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Sql.Union (q1, q2) => SS.union (tablesOfQuery q1, tablesOfQuery q2)

val tableOfDml =
 fn Sql.Insert (tab, _) => tab
  | Sql.Delete (tab, _) => tab
  | Sql.Update (tab, _, _) => tab

val freeVars =
    MonoUtil.Exp.foldB
        {typ = #2,
         exp = fn (bound, ERel n, vars) => if n < bound
                                           then vars
                                           else IS.add (vars, n - bound)
                | (_, _, vars) => vars,
         bind = fn (bound, MonoUtil.Exp.RelE _) => bound + 1 | (bound, _) => bound}
        0
        IS.empty

datatype unbind = Known of exp | Unknowns of int

structure InvalInfo :> sig
    type t
    type state = {tableToIndices : SIMM.multimap,
                  indexToInvalInfo : (t * int) IntBinaryMap.map,
                  ffiInfo : {index : int, params : int} list,
                  index : int}
    val empty : t
    val singleton : Sql.query -> t
    val query : t -> Sql.query
    val orderArgs : t * IS.set -> int list
    val unbind : t * unbind -> t option
    val union : t * t -> t
    val updateState : t * int * state -> state
end = struct

    type t = Sql.query list

    type state = {tableToIndices : SIMM.multimap,
                  indexToInvalInfo : (t * int) IntBinaryMap.map,
                  ffiInfo : {index : int, params : int} list,
                  index : int}

    val empty = []

    fun singleton q = [q]

    val union = op@

    (* Need lift', etc. because we don't have rank-2 polymorphism. This should
       probably use a functor, but this works for now. *)
    fun traverseSqexp (pure, _, lift, _, lift'', lift2, _) f =
        let
            val rec tr =
             fn Sql.SqNot se => lift Sql.SqNot (tr se)
              | Sql.Binop (r, se1, se2) =>
                lift2 (fn (trse1, trse2) => Sql.Binop (r, trse1, trse2)) (tr se1, tr se2)
              | Sql.SqKnown se => lift Sql.SqKnown (tr se)
              | Sql.Inj (e', loc) => lift'' (fn fe' => Sql.Inj (fe', loc)) (f e')
              | Sql.SqFunc (s, se) => lift (fn trse => Sql.SqFunc (s, trse)) (tr se)
              | se => pure se
        in
            tr
        end

    fun traverseQuery (ops as (_, pure', _, lift', _, _, lift2')) f =
        let
            val rec mp =
             fn Sql.Query1 q =>
                (case #Where q of
                     NONE => pure' (Sql.Query1 q)
                   | SOME se =>
                     lift' (fn mpse => Sql.Query1 {Select = #Select q,
                                                   From = #From q,
                                                   Where = SOME mpse})
                           (traverseSqexp ops f se))
              | Sql.Union (q1, q2) => lift2' Sql.Union (mp q1, mp q2)
        in
            mp
        end

    fun foldMapQuery plus zero = traverseQuery (fn _ => zero,
                                                fn _ => zero,
                                                fn _ => fn x => x,
                                                fn _ => fn x => x,
                                                fn _ => fn x => x,
                                                fn _ => plus,
                                                fn _ => plus)

    val omapQuery = traverseQuery (SOME, SOME, omap, omap, omap, omap2, omap2)

    val varsOfQuery = foldMapQuery IS.union
                                   IS.empty
                                   (fn e' => freeVars (e', dummyLoc))

    val varsOfList =
     fn [] => IS.empty
      | (q::qs) => varsOfQuery (List.foldl Sql.Union q qs)

    fun orderArgs (qs, vars) =
        let
            val invalVars = varsOfList qs
        in
            (* Put arguments we might invalidate by first. *)
            IS.listItems invalVars @ IS.listItems (IS.difference (vars, invalVars))
        end

    (* As a kludge, we rename the variables in the query to correspond to the
       argument of the cache they're part of. *)
    val query =
     fn (q::qs) =>
        let
            val q = List.foldl Sql.Union q qs
            val ns = IS.listItems (varsOfQuery q)
            val rename =
             fn ERel n => omap ERel (indexOf (fn n' => n' = n) ns)
              | _ => raise Match
        in
            case omapQuery rename q of
                SOME q => q
              (* We should never get NONE because indexOf should never fail. *)
              | NONE => raise Match
        end
      (* We should never reach this case because [updateState] won't put
         anything in the state if there are no queries. *)
      | [] => raise Match

    fun unbind1 ub =
        case ub of
            Known (e', loc) =>
            let
                val replaceRel0 = case e' of
                                      ERel m => SOME (ERel m)
                                    | _ => NONE
            in
                omapQuery (fn ERel 0 => replaceRel0
                            | ERel n => SOME (ERel (n-1))
                            | _ => raise Match)
            end
          | Unknowns k =>
            omapQuery (fn ERel n => if n >= k then NONE else SOME (ERel (n-k))
                        | _ => raise Match)

    fun unbind (qs, ub) =
        case ub of
            (* Shortcut if nothing's changing. *)
            Unknowns 0 => SOME qs
          | _ => osequence (map (unbind1 ub) qs)

    fun updateState ((qs, numArgs, state as {index, ...}) : t * int * state) =
        {tableToIndices = List.foldr (fn (q, acc) =>
                                         SS.foldl (fn (tab, acc) =>
                                                      SIMM.insert (acc, tab, index))
                                                  acc
                                                  (tablesOfQuery q))
                                     (#tableToIndices state)
                                     qs,
         indexToInvalInfo = IM.insert (#indexToInvalInfo state, index, (qs, numArgs)),
         ffiInfo = {index = index, params = numArgs} :: #ffiInfo state,
         index = index + 1}

end

structure UF = UnionFindFn(AtomExpKey)

structure ConflictMaps = struct

    structure TK = TripleKeyFn(structure I = CmpKey
                               structure J = AtomOptionKey
                               structure K = AtomOptionKey)
    structure TS : ORD_SET = BinarySetFn(TK)

    val toKnownEquality =
     (* [NONE] here means unkown. Anything that isn't a comparison between two
        knowns shouldn't be used, and simply dropping unused terms is okay in
        disjunctive normal form. *)
     fn (Sql.Eq, SOME e1, SOME e2) => SOME (e1, e2)
      | _ => NONE

    val equivClasses : (Sql.cmp * atomExp option * atomExp option) list -> atomExp list list =
        UF.classes
        o List.foldl UF.union' UF.empty
        o List.mapPartial toKnownEquality

    fun addToEqs (eqs, n, e) =
        case IM.find (eqs, n) of
            (* Comparing to a constant is probably better than comparing to a
               variable? Checking that existing constants match a new ones is
               handled by [accumulateEqs]. *)
            SOME (Prim _) => eqs
          | _ => IM.insert (eqs, n, e)

    val accumulateEqs =
     (* [NONE] means we have a contradiction. *)
     fn (_, NONE) => NONE
      | ((Prim p1, Prim p2), eqso) =>
        (case Prim.compare (p1, p2) of
             EQUAL => eqso
           | _ => NONE)
      | ((QueryArg n, Prim p), SOME eqs) => SOME (addToEqs (eqs, n, Prim p))
      | ((QueryArg n, DmlRel r), SOME eqs) => SOME (addToEqs (eqs, n, DmlRel r))
      | ((Prim p, QueryArg n), SOME eqs) => SOME (addToEqs (eqs, n, Prim p))
      | ((DmlRel r, QueryArg n), SOME eqs) => SOME (addToEqs (eqs, n, DmlRel r))
      (* TODO: deal with equalities between [DmlRel]s and [Prim]s.
         This would involve guarding the invalidation with a check for the
         relevant comparisons. *)
      | (_, eqso) => eqso

    val eqsOfClass : atomExp list -> atomExp IM.map option =
        List.foldl accumulateEqs (SOME IM.empty)
        o chooseTwos

    fun toAtomExps rel (cmp, e1, e2) =
        let
            val qa =
             (* Here [NONE] means unkown. *)
             fn Sql.SqConst p => SOME (Prim p)
              | Sql.Field tf => SOME (Field tf)
              | Sql.Inj (EPrim p, _) => SOME (Prim p)
              | Sql.Inj (ERel n, _) => SOME (rel n)
              (* We can't deal with anything else, e.g., CURRENT_TIMESTAMP
                 becomes Sql.Unmodeled, which becomes NONE here. *)
              | _ => NONE
        in
            (cmp, qa e1, qa e2)
        end

    val negateCmp =
     fn Sql.Eq => Sql.Ne
      | Sql.Ne => Sql.Eq
      | Sql.Lt => Sql.Ge
      | Sql.Le => Sql.Gt
      | Sql.Gt => Sql.Le
      | Sql.Ge => Sql.Lt

    fun normalizeAtom (negating, (cmp, e1, e2)) =
        (* Restricting to Le/Lt and sorting the expressions in Eq/Ne helps with
           simplification, where we put the triples in sets. *)
        case (if negating then negateCmp cmp else cmp) of
            Sql.Eq => (case AtomOptionKey.compare (e1, e2) of
                           LESS => (Sql.Eq, e2, e1)
                         | _ => (Sql.Eq, e1, e2))
          | Sql.Ne => (case AtomOptionKey.compare (e1, e2) of
                           LESS => (Sql.Ne, e2, e1)
                         | _ => (Sql.Ne, e1, e2))
          | Sql.Lt => (Sql.Lt, e1, e2)
          | Sql.Le => (Sql.Le, e1, e2)
          | Sql.Gt => (Sql.Lt, e2, e1)
          | Sql.Ge => (Sql.Le, e2, e1)

    val markQuery : (Sql.cmp * Sql.sqexp * Sql.sqexp) formula ->
                    (Sql.cmp * atomExp option * atomExp option) formula =
        mapFormula (toAtomExps QueryArg)

    val markDml : (Sql.cmp * Sql.sqexp * Sql.sqexp) formula ->
                  (Sql.cmp * atomExp option * atomExp option) formula =
        mapFormula (toAtomExps DmlRel)

    (* No eqs should have key conflicts because no variable is in two
       equivalence classes, so the [#1] could be [#2]. *)
    val mergeEqs : (atomExp IntBinaryMap.map option list
                    -> atomExp IntBinaryMap.map option) =
        List.foldr (omap2 (IM.unionWith #1)) (SOME IM.empty)

    val simplify =
        map TS.listItems
        o removeRedundant (fn (x, y) => TS.isSubset (y, x))
        o map (fn xs => TS.addList (TS.empty, xs))

    fun dnf (fQuery, fDml) =
        normalize simplify normalizeAtom Disj (Combo (Conj, [markQuery fQuery, markDml fDml]))

    val conflictMaps = List.mapPartial (mergeEqs o map eqsOfClass o equivClasses) o dnf

end

val conflictMaps = ConflictMaps.conflictMaps

val rec sqexpToFormula =
 fn Sql.SqTrue => Combo (Conj, [])
  | Sql.SqFalse => Combo (Disj, [])
  | Sql.SqNot e => Negate (sqexpToFormula e)
  | Sql.Binop (Sql.RCmp c, e1, e2) => Atom (c, e1, e2)
  | Sql.Binop (Sql.RLop l, p1, p2) => Combo (case l of Sql.And => Conj | Sql.Or => Disj,
                                             [sqexpToFormula p1, sqexpToFormula p2])
  (* ASK: any other sqexps that can be props? *)
  | _ => raise Match

fun renameTables tablePairs =
    let
        fun renameString table =
            case List.find (fn (_, t) => table = t) tablePairs of
                NONE => table
              | SOME (realTable, _) => realTable
        val renameSqexp =
         fn Sql.Field (table, field) => Sql.Field (renameString table, field)
          | e => e
        fun renameAtom (cmp, e1, e2) = (cmp, renameSqexp e1, renameSqexp e2)
    in
        mapFormula renameAtom
    end

val rec queryToFormula =
 fn Sql.Query1 {Where = NONE, ...} => Combo (Conj, [])
  | Sql.Query1 {From = tablePairs, Where = SOME e, ...} =>
    renameTables tablePairs (sqexpToFormula e)
  | Sql.Union (q1, q2) => Combo (Disj, [queryToFormula q1, queryToFormula q2])

fun valsToFormula (table, vals) =
    Combo (Conj, map (fn (field, v) => Atom (Sql.Eq, Sql.Field (table, field), v)) vals)

val rec dmlToFormula =
 fn Sql.Insert (table, vals) => valsToFormula (table, vals)
  | Sql.Delete (table, wher) => renameTables [(table, "T")] (sqexpToFormula wher)
  | Sql.Update (table, vals, wher) =>
    let
        val fWhere = sqexpToFormula wher
        val fVals = valsToFormula (table, vals)
        val modifiedFields = SS.addList (SS.empty, map #1 vals)
        (* TODO: don't use field name hack. *)
        val markField =
         fn e as Sql.Field (t, v) => if SS.member (modifiedFields, v)
                                     then Sql.Field (t, v ^ "'")
                                     else e
          | e => e
        val mark = mapFormula (fn (cmp, e1, e2) => (cmp, markField e1, markField e2))
    in
        renameTables [(table, "T")]
                     (Combo (Disj, [Combo (Conj, [fVals, mark fWhere]),
                                    Combo (Conj, [mark fVals, fWhere])]))
    end


(*************************************)
(* Program Instrumentation Utilities *)
(*************************************)

val varName =
    let
        val varNumber = ref 0
    in
        fn s => (varNumber := !varNumber + 1; s ^ Int.toString (!varNumber))
    end

val {check, store, flush, ...} = getCache ()

val dummyTyp = (TRecord [], dummyLoc)

fun stringExp s = (EPrim (Prim.String (Prim.Normal, s)), dummyLoc)

val stringTyp = (TFfi ("Basis", "string"), dummyLoc)

val sequence =
 fn (exp :: exps) =>
    let
        val loc = dummyLoc
    in
        List.foldl (fn (e', seq) => ESeq ((seq, loc), (e', loc))) exp exps
    end
  | _ => raise Match

(* Always increments negative indices as a hack we use later. *)
fun incRels inc =
    MonoUtil.Exp.mapB
        {typ = fn t' => t',
         exp = fn bound =>
                  (fn ERel n => ERel (if n >= bound orelse n < 0 then n + inc else n)
                    | e' => e'),
         bind = fn (bound, MonoUtil.Exp.RelE _) => bound + 1 | (bound, _) => bound}
        0

fun fileTopLevelMapfoldB doTopLevelExp (decls, sideInfo) state =
    let
        fun doVal env ((x, n, t, exp, s), state) =
            let
                val (exp, state) = doTopLevelExp env exp state
            in
                ((x, n, t, exp, s), state)
            end
        fun doDecl' env (decl', state) =
            case decl' of
                DVal v =>
                let
                    val (v, state) = doVal env (v, state)
                in
                    (DVal v, state)
                end
              | DValRec vs =>
                let
                    val (vs, state) = ListUtil.foldlMap (doVal env) state vs
                in
                    (DValRec vs, state)
                end
              | _ => (decl', state)
        fun doDecl (decl as (decl', loc), (env, state)) =
            let
                val env = MonoEnv.declBinds env decl
                val (decl', state) = doDecl' env (decl', state)
            in
                ((decl', loc), (env, state))
            end
        val (decls, (_, state)) = (ListUtil.foldlMap doDecl (MonoEnv.empty, state) decls)
    in
        ((decls, sideInfo), state)
    end

fun fileAllMapfoldB doExp file start =
    case MonoUtil.File.mapfoldB
             {typ = Search.return2,
              exp = fn env => fn e' => fn s => Search.Continue (doExp env e' s),
              decl = fn _ => Search.return2,
              bind = doBind}
             MonoEnv.empty file start of
        Search.Continue x => x
      | Search.Return _ => raise Match

fun fileMap doExp file = #1 (fileAllMapfoldB (fn _ => fn e => fn _ => (doExp e, ())) file ())

(* TODO: make this a bit prettier.... *)
val simplifySql =
    let
        fun factorOutNontrivial text =
            let
                val loc = dummyLoc
                fun strcat (e1, e2) = (EStrcat (e1, e2), loc)
                val chunks = Sql.chunkify text
                val (newText, newVariables) =
                    (* Important that this is foldr (to oppose foldl below). *)
                    List.foldr
                        (fn (chunk, (qText, newVars)) =>
                            (* Variable bound to the head of newVars will have the lowest index. *)
                            case chunk of
                                (* EPrim should always be a string in this case. *)
                                Sql.Exp (e as (EPrim _, _)) => (strcat (e, qText), newVars)
                              | Sql.Exp e =>
                                let
                                    val n = length newVars
                                in
                                    (* This is the (n+1)th new variable, so there are
                                       already n new variables bound, so we increment
                                       indices by n. *)
                                    (strcat ((ERel (~(n+1)), loc), qText), incRels n e :: newVars)
                                end
                              | Sql.String s => (strcat (stringExp s, qText), newVars))
                        (stringExp "", [])
                        chunks
                fun wrapLets e' =
                    (* Important that this is foldl (to oppose foldr above). *)
                    List.foldl (fn (v, e') => ELet (varName "sqlArg", stringTyp, v, (e', loc)))
                               e'
                               newVariables
                val numArgs = length newVariables
            in
                (newText, wrapLets, numArgs)
            end
        fun doExp exp' =
            let
                val text = case exp' of
                               EQuery {query = text, ...} => text
                             | EDml (text, _) => text
                             | _ => raise Match
                val (newText, wrapLets, numArgs) = factorOutNontrivial text
                val newExp' = case exp' of
                                 EQuery q => EQuery {query = newText,
                                                     exps = #exps q,
                                                     tables = #tables q,
                                                     state = #state q,
                                                     body = #body q,
                                                     initial = #initial q}
                               | EDml (_, failureMode) => EDml (newText, failureMode)
                               | _ => raise Match
            in
                (* Increment once for each new variable just made. This is
                   where we use the negative De Bruijn indices hack. *)
                (* TODO: please don't use that hack. As anyone could have
                   predicted, it was incomprehensible a year later.... *)
                wrapLets (#1 (incRels numArgs (newExp', dummyLoc)))
            end
    in
        fileMap (fn exp' => case exp' of
                                EQuery _ => doExp exp'
                              | EDml _ => doExp exp'
                              | _ => exp')
    end


(**********************)
(* Mono Type Checking *)
(**********************)

fun typOfExp' (env : MonoEnv.env) : exp' -> typ option =
 fn EPrim p => SOME (TFfi ("Basis", case p of
                                        Prim.Int _ => "int"
                                      | Prim.Float _ => "double"
                                      | Prim.String _ => "string"
                                      | Prim.Char _ => "char"),
                     dummyLoc)
  | ERel n => SOME (#2 (MonoEnv.lookupERel env n))
  | ENamed n => SOME (#2 (MonoEnv.lookupENamed env n))
  (* ASK: okay to make a new [ref] each time? *)
  | ECon (dk, PConVar nCon, _) =>
    let
        val (_, _, nData) = MonoEnv.lookupConstructor env nCon
        val (_, cs) = MonoEnv.lookupDatatype env nData
    in
        SOME (TDatatype (nData, ref (dk, cs)), dummyLoc)
    end
  | ECon (_, PConFfi {mod = s, datatyp, ...}, _) => SOME (TFfi (s, datatyp), dummyLoc)
  | ENone t => SOME (TOption t, dummyLoc)
  | ESome (t, _) => SOME (TOption t, dummyLoc)
  | EFfi _ => NONE
  | EFfiApp _ => NONE
  | EApp (e1, e2) => (case typOfExp env e1 of
                          SOME (TFun (_, t), _) => SOME t
                        | _ => NONE)
  | EAbs (_, t1, t2, _) => SOME (TFun (t1, t2), dummyLoc)
  (* ASK: is this right? *)
  | EUnop (unop, e) => (case unop of
                            "!" => SOME (TFfi ("Basis", "bool"), dummyLoc)
                          | "-" => typOfExp env e
                          | _ => NONE)
  (* ASK: how should this (and other "=> NONE" cases) work? *)
  | EBinop _ => NONE
  | ERecord fields => SOME (TRecord (map (fn (s, _, t) => (s, t)) fields), dummyLoc)
  | EField (e, s) => (case typOfExp env e of
                          SOME (TRecord fields, _) =>
                          (case List.find (fn (s', _) => s = s') fields of
                               SOME (_, t) => SOME t
                             | _ => NONE)
                        | _ => NONE)
  | ECase (_, _, {result, ...}) => SOME result
  | EStrcat _ => SOME (TFfi ("Basis", "string"), dummyLoc)
  | EWrite _ => SOME (TRecord [], dummyLoc)
  | ESeq (_, e) => typOfExp env e
  | ELet (s, t, e1, e2) => typOfExp (MonoEnv.pushERel env s t (SOME e1)) e2
  | EClosure _ => NONE
  | EUnurlify (_, t, _) => SOME t
  | EQuery {state, ...} => SOME state
  | _ => NONE

and typOfExp env (e', loc) = typOfExp' env e'


(***********)
(* Caching *)
(***********)

(*

To get the invalidations for a dml, we need (each <- is list-monad-y):
  * table <- dml
  * cache <- table
  * query <- cache
  * inval <- (query, dml),
where inval is a list of query argument indices, so
  * way to change query args in inval to cache args.
For now, the last one is just
  * a map from query arg number to the corresponding free variable (per query)
  * a map from free variable to cache arg number (per cache).
Both queries and caches should have IDs.

*)

type state = InvalInfo.state

datatype subexp = Cachable of InvalInfo.t * (state -> exp * state) | Impure of exp

val isImpure =
 fn Cachable _ => false
  | Impure _ => true

val runSubexp : subexp * state -> exp * state =
 fn (Cachable (_, f), state) => f state
  | (Impure e, state) => (e, state)

val invalInfoOfSubexp =
 fn Cachable (invalInfo, _) => invalInfo
  | Impure _ => raise Match

fun cacheWrap (env, exp, typ, args, index) =
    let
        val loc = dummyLoc
        val rel0 = (ERel 0, loc)
    in
        case MonoFooify.urlify env (rel0, typ) of
            NONE => NONE
          | SOME urlified =>
            let
                (* We ensure before this step that all arguments aren't effectful.
                   by turning them into local variables as needed. *)
                val argsInc = map (incRels 1) args
                val check = (check (index, args), loc)
                val store = (store (index, argsInc, urlified), loc)
            in
                SOME (ECase (check,
                             [((PNone stringTyp, loc),
                               (ELet (varName "q", typ, exp, (ESeq (store, rel0), loc)), loc)),
                              ((PSome (stringTyp, (PVar (varName "hit", stringTyp), loc)), loc),
                               (* Boolean is false because we're not unurlifying from a cookie. *)
                               (EUnurlify (rel0, typ, false), loc))],
                             {disc = (TOption stringTyp, loc), result = typ}))
            end
    end

val expSize = MonoUtil.Exp.fold {typ = #2, exp = fn (_, n) => n+1} 0

(* TODO: pick a number. *)
val sizeWorthCaching = 5

val worthCaching =
 fn EQuery _ => true
  | exp' => expSize (exp', dummyLoc) > sizeWorthCaching

fun cacheExp ((env, exp', invalInfo, state) : MonoEnv.env * exp' * InvalInfo.t * state) =
    case (worthCaching exp')
             </oguard/>
             typOfExp' env exp' of
        NONE => NONE
      | SOME (TFun _, _) => NONE
      | SOME typ =>
        let
            val ns = InvalInfo.orderArgs (invalInfo, freeVars (exp', dummyLoc))
            val numArgs = length ns
        in  (List.foldr (fn (_, NONE) => NONE
                        | ((n, typ), SOME args) =>
                          (MonoFooify.urlify env ((ERel n, dummyLoc), typ))
                              </obind/>
                              (fn arg => SOME (arg :: args)))
                        (SOME [])
                        (map (fn n => (n, #2 (MonoEnv.lookupERel env n))) ns))
                </obind/>
                (fn args =>
                    (cacheWrap (env, (exp', dummyLoc), typ, args, #index state))
                        </obind/>
                        (fn cachedExp =>
                            SOME (cachedExp, InvalInfo.updateState (invalInfo, numArgs, state))))
        end

fun cacheQuery (effs, env, q) : subexp =
    let
        (* We use dummyTyp here. I think this is okay because databases don't
           store (effectful) functions, but perhaps there's some pathalogical
           corner case missing.... *)
        fun safe bound =
            not
            o effectful effs
                        (iterate (fn env => MonoEnv.pushERel env "_" dummyTyp NONE)
                                 bound
                                 env)
        val {query = queryText, initial, body, ...} = q
        (* DEBUG *)
        (* val () = Print.preface ("sqlcache> ", MonoPrint.p_exp MonoEnv.empty queryText) *)
        val attempt =
            (* Ziv misses Haskell's do notation.... *)
            (safe 0 queryText andalso safe 0 initial andalso safe 2 body)
            </oguard/>
            Sql.parse Sql.query queryText
            </obind/>
            (fn queryParsed =>
                let
                    val invalInfo = InvalInfo.singleton queryParsed
                    fun mkExp state =
                        case cacheExp (env, EQuery q, invalInfo, state) of
                            NONE => ((EQuery q, dummyLoc), state)
                          | SOME (cachedExp, state) => ((cachedExp, dummyLoc), state)
                in
                    SOME (Cachable (invalInfo, mkExp))
                end)
    in
        case attempt of
            NONE => Impure (EQuery q, dummyLoc)
          | SOME subexp => subexp
    end

fun cacheTree (effs : IS.set) ((env, exp as (exp', loc)), state) =
    let
        fun wrapBindN (f : exp list -> exp')
                      (args : ((MonoEnv.env * exp) * unbind) list) =
            let
                val (subexps, state) =
                    ListUtil.foldlMap (cacheTree effs)
                                      state
                                      (map #1 args)
                fun mkExp state = mapFst (fn exps => (f exps, loc))
                                         (ListUtil.foldlMap runSubexp state subexps)
                val attempt =
                    if List.exists isImpure subexps
                    then NONE
                    else (List.foldl (omap2 InvalInfo.union)
                                     (SOME InvalInfo.empty)
                                     (ListPair.map
                                          (fn (subexp, (_, unbinds)) =>
                                              InvalInfo.unbind (invalInfoOfSubexp subexp, unbinds))
                                          (subexps, args)))
                             </obind/>
                             (fn invalInfo =>
                                 SOME (Cachable (invalInfo,
                                                 fn state =>
                                                    case cacheExp (env,
                                                                   f (map (#2 o #1) args),
                                                                   invalInfo,
                                                                   state) of
                                                        NONE => mkExp state
                                                      | SOME (e', state) => ((e', loc), state)),
                                       state))
            in
                case attempt of
                    SOME (subexp, state) => (subexp, state)
                  | NONE => mapFst Impure (mkExp state)
            end
        fun wrapBind1 f arg =
            wrapBindN (fn [arg] => f arg | _ => raise Match) [arg]
        fun wrapBind2 f (arg1, arg2) =
            wrapBindN (fn [arg1, arg2] => f (arg1, arg2) | _ => raise Match) [arg1, arg2]
        fun wrapN f es = wrapBindN f (map (fn e => ((env, e), Unknowns 0)) es)
        fun wrap1 f e = wrapBind1 f ((env, e), Unknowns 0)
        fun wrap2 f (e1, e2) = wrapBind2 f (((env, e1), Unknowns 0), ((env, e2), Unknowns 0))
    in
        case exp' of
            ECon (dk, pc, SOME e) => wrap1 (fn e => ECon (dk, pc, SOME e)) e
          | ESome (t, e) => wrap1 (fn e => ESome (t, e)) e
          | EFfiApp (s1, s2, args) =>
            if ffiEffectful (s1, s2)
            then (Impure exp, state)
            else wrapN (fn es =>
                           EFfiApp (s1, s2, ListPair.map (fn (e, (_, t)) => (e, t)) (es, args)))
                       (map #1 args)
          | EApp (e1, e2) => wrap2 EApp (e1, e2)
          | EAbs (s, t1, t2, e) =>
            wrapBind1 (fn e => EAbs (s, t1, t2, e))
                      ((MonoEnv.pushERel env s t1 NONE, e), Unknowns 1)
          | EUnop (s, e) => wrap1 (fn e => EUnop (s, e)) e
          | EBinop (bi, s, e1, e2) => wrap2 (fn (e1, e2) => EBinop (bi, s, e1, e2)) (e1, e2)
          | ERecord fields =>
            wrapN (fn es => ERecord (ListPair.map (fn (e, (s, _, t)) => (s, e, t)) (es, fields)))
                  (map #2 fields)
          | EField (e, s) => wrap1 (fn e => EField (e, s)) e
          | ECase (e, cases, {disc, result}) =>
            wrapBindN (fn (e::es) =>
                          ECase (e,
                                 (ListPair.map (fn (e, (p, _)) => (p, e)) (es, cases)),
                                 {disc = disc, result = result})
                        | _ => raise Match)
                      (((env, e), Unknowns 0)
                       :: map (fn (p, e) =>
                                  ((MonoEnv.patBinds env p, e), Unknowns (MonoEnv.patBindsN p)))
                              cases)
          | EStrcat (e1, e2) => wrap2 EStrcat (e1, e2)
          (* We record page writes, so they're cachable. *)
          | EWrite e => wrap1 EWrite e
          | ESeq (e1, e2) => wrap2 ESeq (e1, e2)
          | ELet (s, t, e1, e2) =>
            wrapBind2 (fn (e1, e2) => ELet (s, t, e1, e2))
                      (((env, e1), Unknowns 0),
                       ((MonoEnv.pushERel env s t (SOME e1), e2), Known e1))
          (* ASK: | EClosure (n, es) => ? *)
          | EUnurlify (e, t, b) => wrap1 (fn e => EUnurlify (e, t, b)) e
          | EQuery q => (cacheQuery (effs, env, q), state)
          | _ => (if effectful effs env exp
                  then Impure exp
                  else Cachable (InvalInfo.empty,
                                 fn state =>
                                    case cacheExp (env, exp', InvalInfo.empty, state) of
                                        NONE => ((exp', loc), state)
                                      | SOME (exp', state) => ((exp', loc), state)),
                  state)
    end

fun addCaching file =
    let
        val effs = effectfulDecls file
        fun doTopLevelExp env exp state = runSubexp (cacheTree effs ((env, exp), state))
    in
        (fileTopLevelMapfoldB doTopLevelExp
                              file
                              {tableToIndices = SIMM.empty,
                               indexToInvalInfo = IM.empty,
                               ffiInfo = [],
                               index = 0},
         effs)
    end


(************)
(* Flushing *)
(************)

structure Invalidations = struct

    val loc = dummyLoc

    val optionAtomExpToExp =
     fn NONE => (ENone stringTyp, loc)
      | SOME e => (ESome (stringTyp,
                          (case e of
                               DmlRel n => ERel n
                             | Prim p => EPrim p
                             (* TODO: make new type containing only these two. *)
                             | _ => raise Match,
                           loc)),
                   loc)

    fun eqsToInvalidation numArgs eqs =
        List.tabulate (numArgs, (fn n => IM.find (eqs, n)))

    (* Tests if [ys] makes [xs] a redundant cache invalidation. [NONE] here
       represents unknown, which means a wider invalidation. *)
    val rec madeRedundantBy : atomExp option list * atomExp option list -> bool =
     fn ([], []) => true
      | (_ :: xs, NONE :: ys) => madeRedundantBy (xs, ys)
      | (SOME x :: xs, SOME y :: ys) => (case AtomExpKey.compare (x, y) of
                                             EQUAL => madeRedundantBy (xs, ys)
                                           | _ => false)
      | _ => false

    fun eqss (query, dml) = conflictMaps (queryToFormula query, dmlToFormula dml)

    fun invalidations ((invalInfo, numArgs), dml) =
        let
            val query = InvalInfo.query invalInfo
        in
            (map (map optionAtomExpToExp)
             o removeRedundant madeRedundantBy
             o map (eqsToInvalidation numArgs)
             o eqss)
                (query, dml)
        end

end

val invalidations = Invalidations.invalidations

(* DEBUG *)
(* val gunk : ((Sql.query * int) * Sql.dml) list ref = ref [] *)
(* val gunk' : exp list ref = ref [] *)

fun addFlushing ((file, {tableToIndices, indexToInvalInfo, ffiInfo, index}), effs) =
    let
        val flushes = List.concat
                      o map (fn (i, argss) => map (fn args => flush (i, args)) argss)
        val doExp =
         fn dmlExp as EDml (dmlText, failureMode) =>
            let
                (* DEBUG *)
                (* val () = gunk' := origDmlText :: !gunk' *)
                (* val () = Print.preface ("SQLCACHE: ", (MonoPrint.p_exp MonoEnv.empty origDmlText)) *)
                val inval =
                    case Sql.parse Sql.dml dmlText of
                        SOME dmlParsed =>
                        SOME (map (fn i => (case IM.find (indexToInvalInfo, i) of
                                                SOME invalInfo =>
                                                (i, invalidations (invalInfo, dmlParsed))
                                              (* TODO: fail more gracefully. *)
                                              (* This probably means invalidating everything.... *)
                                              | NONE => raise Match))
                                  (SIMM.findList (tableToIndices, tableOfDml dmlParsed)))
                      | NONE => NONE
            in
                case inval of
                    (* TODO: fail more gracefully. *)
                    NONE => raise Match
                  | SOME invs => sequence (flushes invs @ [dmlExp])
            end
          | e' => e'
    in
        (* DEBUG *)
        (* gunk := []; *)
        ffiInfoRef := ffiInfo;
        fileMap doExp file
    end


(************************)
(* Compiler Entry Point *)
(************************)

val inlineSql =
    let
        val doExp =
         (* TODO: EQuery, too? *)
         (* ASK: should this live in [MonoOpt]? *)
         fn EDml ((ECase (disc, cases, {disc = dTyp, ...}), loc), failureMode) =>
            let
                val newCases = map (fn (p, e) => (p, (EDml (e, failureMode), loc))) cases
            in
                ECase (disc, newCases, {disc = dTyp, result = (TRecord [], loc)})
            end
          | e => e
    in
        fileMap doExp
    end

fun insertAfterDatatypes ((decls, sideInfo), newDecls) =
    let
        val (datatypes, others) = List.partition (fn (DDatatype _, _) => true | _ => false) decls
    in
        (datatypes @ newDecls @ others, sideInfo)
    end

val go' = addFlushing o addCaching o simplifySql o inlineSql

fun go file =
    let
        (* TODO: do something nicer than [Sql] being in one of two modes. *)
        val () = (resetFfiInfo (); Sql.sqlcacheMode := true)
        val file = go' file
        (* Important that this happens after [MonoFooify.urlify] calls! *)
        val fmDecls = MonoFooify.getNewFmDecls ()
        val () = Sql.sqlcacheMode := false
    in
        insertAfterDatatypes (file, rev fmDecls)
    end

end
