structure Sqlcache :> SQLCACHE = struct


(*********************)
(* General Utilities *)
(*********************)

structure IK = struct type ord_key = int val compare = Int.compare end
structure IS = IntBinarySet
structure IM = IntBinaryMap
structure SK = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure IIMM = MultimapFn(structure KeyMap = IM structure ValSet = IS)
structure SIMM = MultimapFn(structure KeyMap = SM structure ValSet = IS)

fun id x = x

fun iterate f n x = if n < 0
                    then raise Fail "Can't iterate function negative number of times."
                    else if n = 0
                    then x
                    else iterate f (n-1) (f x)

(* From the MLton wiki. *)
infix  3 <\     fun x <\ f = fn y => f (x, y)     (* Left section      *)
infix  3 \>     fun f \> y = f y                  (* Left application  *)

fun mapFst f (x, y) = (f x, y)

(* Option monad. *)
fun obind (x, f) = Option.mapPartial f x
fun oguard (b, x) = if b then x () else NONE
fun omap f = fn SOME x => SOME (f x) | _ => NONE
fun omap2 f = fn (SOME x, SOME y) => SOME (f (x,y)) | _ => NONE
fun osequence ys = List.foldr (omap2 op::) (SOME []) ys

fun concatMap f xs = List.concat (map f xs)

val rec cartesianProduct : 'a list list -> 'a list list =
 fn [] => [[]]
  | (xs :: xss) => concatMap (fn ys => concatMap (fn x => [x :: ys]) xs)
                             (cartesianProduct xss)

fun indexOf test =
    let
        fun f n =
         fn [] => NONE
          | (x::xs) => if test x then SOME n else f (n+1) xs
    in
        f 0
    end


(************)
(* Settings *)
(************)

open Mono

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

val cacheRef = ref LruCache.cache
fun setCache c = cacheRef := c
fun getCache () = !cacheRef

datatype heuristic = Smart | Always | Never | NoPureAll | NoPureOne | NoCombo

val heuristicRef = ref NoPureOne
fun setHeuristic h = heuristicRef := (case h of
                                          "smart" => Smart
                                        | "always" => Always
                                        | "never" => Never
                                        | "nopureall" => NoPureAll
                                        | "nopureone" => NoPureOne
                                        | "nocombo" => NoCombo
                                        | _ => raise Fail "Sqlcache: setHeuristic")
fun getHeuristic () = !heuristicRef


(************************)
(* Really Useful Things *)
(************************)

(* Used to have type context for local variables in MonoUtil functions. *)
val doBind =
 fn (env, MonoUtil.Exp.RelE (x, t)) => MonoEnv.pushERel env x t NONE
  | (env, MonoUtil.Exp.NamedE (x, n, t, eo, s)) => MonoEnv.pushENamed env x n t eo s
  | (env, MonoUtil.Exp.Datatype (x, n, cs)) => MonoEnv.pushDatatype env x n cs

val dummyLoc = ErrorMsg.dummySpan

(* DEBUG *)
fun printExp msg exp =
    (Print.preface ("SQLCACHE: " ^ msg ^ ":", MonoPrint.p_exp MonoEnv.empty exp); exp)
fun printExp' msg exp' = (printExp msg (exp', dummyLoc); exp')
fun printTyp msg typ =
    (Print.preface ("SQLCACHE: " ^ msg ^ ":", MonoPrint.p_typ MonoEnv.empty typ); typ)
fun printTyp' msg typ' = (printTyp msg (typ', dummyLoc); typ')
fun obindDebug printer (x, f) =
    case x of
        NONE => NONE
      | SOME x' => case f x' of
                       NONE => (printer (); NONE)
                     | y => y


(*******************)
(* Effect Analysis *)
(*******************)

(* TODO: test this. *)
fun transitiveAnalysis doVal state (decls, _) =
    let
        val doDecl =
         fn ((DVal v, _), state) => doVal (v, state)
          (* Pass over the list of values a number of times equal to its size,
             making sure whatever property we're testing propagates everywhere
             it should. This is analagous to the Bellman-Ford algorithm. *)
          | ((DValRec vs, _), state) =>
            iterate (fn state => List.foldl doVal state vs) (length vs) state
          | (_, state) => state
    in
        List.foldl doDecl state decls
    end

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
fun effectfulDecls file =
    transitiveAnalysis (fn ((_, name, _, e, _), effs) =>
                           if effectful effs MonoEnv.empty e
                           then IS.add (effs, name)
                           else effs)
                       IS.empty
                       file


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

fun mapFormulaExps mf = mapFormula (fn (cmp, e1, e2) => (cmp, mf e1, mf e2))


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
         True
       | False
       | QueryArg of int
       | DmlRel of int
       | Prim of Prim.t
       | Field of string * string

structure AtomExpKey : ORD_KEY = struct

    type ord_key = atomExp

    val compare =
     fn (True, True) => EQUAL
      | (True, _) => LESS
      | (_, True) => GREATER
      | (False, False) => EQUAL
      | (False, _) => LESS
      | (_, False) => GREATER
      | (QueryArg n1, QueryArg n2) => Int.compare (n1, n2)
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
 fn Sql.Query1 {From = fitems, ...} => List.foldl SS.union SS.empty (map tableOfFitem fitems)
  | Sql.Union (q1, q2) => SS.union (tablesOfQuery q1, tablesOfQuery q2)
and tableOfFitem =
 fn Sql.Table (t, _) => SS.singleton t
  | Sql.Nested (q, _) => tablesOfQuery q
  | Sql.Join (_, f1, f2, _) => SS.union (tableOfFitem f1, tableOfFitem f2)

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
         bind = fn (bound, MonoUtil.Exp.RelE _) => bound + 1
                 | (bound, _) => bound}
        0
        IS.empty

(* A path is a number of field projections of a variable. *)
type path = int * string list
structure PK = PairKeyFn(structure I = IK structure J = ListKeyFn(SK))
structure PS = BinarySetFn(PK)

val pathOfExp =
    let
        fun readFields acc exp =
            acc
            <\obind\>
             (fn fs =>
                 case #1 exp of
                     ERel n => SOME (n, fs)
                   | EField (exp, f) => readFields (SOME (f::fs)) exp
                   | _ => NONE)
    in
        readFields (SOME [])
    end

fun expOfPath (n, fs) =
    List.foldl (fn (f, exp) => (EField (exp, f), dummyLoc)) (ERel n, dummyLoc) fs

fun freePaths'' bound exp paths =
    case pathOfExp (exp, dummyLoc) of
        NONE => paths
      | SOME (n, fs) => if n < bound then paths else PS.add (paths, (n - bound, fs))

(* ASK: nicer way? :( *)
fun freePaths' bound exp =
    case #1 exp of
        EPrim _ => id
      | e as ERel _ => freePaths'' bound e
      | ENamed _ => id
      | ECon (_, _, data) => (case data of NONE => id | SOME e => freePaths' bound e)
      | ENone _ => id
      | ESome (_, e) => freePaths' bound e
      | EFfi _ => id
      | EFfiApp (_, _, args) =>
        List.foldl (fn ((e, _), acc) => freePaths' bound e o acc) id args
      | EApp (e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | EAbs (_, _, _, e) => freePaths' (bound + 1) e
      | EUnop (_, e) => freePaths' bound e
      | EBinop (_, _, e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | ERecord fields => List.foldl (fn ((_, e, _), acc) => freePaths' bound e o acc) id fields
      | e as EField _ => freePaths'' bound e
      | ECase (e, cases, _) =>
        List.foldl (fn ((p, e), acc) => freePaths' (MonoEnv.patBindsN p + bound) e o acc)
                   (freePaths' bound e)
                   cases
      | EStrcat (e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | EError (e, _) => freePaths' bound e
      | EReturnBlob {blob, mimeType = e, ...} =>
        freePaths' bound e o (case blob of NONE => id | SOME e => freePaths' bound e)
      | ERedirect (e, _) => freePaths' bound e
      | EWrite e => freePaths' bound e
      | ESeq (e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | ELet (_, _, e1, e2) => freePaths' bound e1 o freePaths' (bound + 1) e2
      | EClosure (_, es) => List.foldl (fn (e, acc) => freePaths' bound e o acc) id es
      | EQuery {query = e1, body = e2, initial = e3, ...} =>
        freePaths' bound e1 o freePaths' (bound + 2) e2 o freePaths' bound e3
      | EDml (e, _) => freePaths' bound e
      | ENextval e => freePaths' bound e
      | ESetval (e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | EUnurlify (e, _, _) => freePaths' bound e
      | EJavaScript (_, e) => freePaths' bound e
      | ESignalReturn e => freePaths' bound e
      | ESignalBind (e1, e2) => freePaths' bound e1 o freePaths' bound e2
      | ESignalSource e => freePaths' bound e
      | EServerCall (e, _, _, _) => freePaths' bound e
      | ERecv (e, _) => freePaths' bound e
      | ESleep e => freePaths' bound e
      | ESpawn e => freePaths' bound e

fun freePaths exp = freePaths' 0 exp PS.empty

datatype unbind = Known of exp | Unknowns of int

datatype cacheArg = AsIs of exp | Urlify of exp

structure InvalInfo :> sig
    type t
    type state = {tableToIndices : SIMM.multimap,
                  indexToInvalInfo : (t * int) IntBinaryMap.map,
                  ffiInfo : {index : int, params : int} list,
                  index : int}
    val empty : t
    val singleton : Sql.query -> t
    val query : t -> Sql.query
    val orderArgs : t * Mono.exp -> cacheArg list option
    val unbind : t * unbind -> t option
    val union : t * t -> t
    val updateState : t * int * state -> state
end = struct

    (* Variable, field projections, possible wrapped sqlification FFI call. *)
    type sqlArg = path * (string * string * typ) option

    type subst = sqlArg IM.map

    (* TODO: store free variables as well? *)
    type t = (Sql.query * subst) list

    type state = {tableToIndices : SIMM.multimap,
                  indexToInvalInfo : (t * int) IntBinaryMap.map,
                  ffiInfo : {index : int, params : int} list,
                  index : int}

    structure AK = PairKeyFn(
        structure I = PK
        structure J = OptionKeyFn(TripleKeyFn(
            structure I = SK
            structure J = SK
            structure K = struct type ord_key = Mono.typ val compare = MonoUtil.Typ.compare end)))
    structure AS = BinarySetFn(AK)
    structure AM = BinaryMapFn(AK)

    (* Traversal Utilities *)
    (* TODO: get rid of unused ones. *)

    (* Need lift', etc. because we don't have rank-2 polymorphism. This should
       probably use a functor (an ML one, not Haskell) but works for now. *)
    fun traverseSqexp (pure, _, _, _, lift, lift', _, _, lift2, _, _, _, _, _) f =
        let
            val rec tr =
             fn Sql.SqNot se => lift Sql.SqNot (tr se)
              | Sql.Binop (r, se1, se2) =>
                lift2 (fn (trse1, trse2) => Sql.Binop (r, trse1, trse2)) (tr se1, tr se2)
              | Sql.SqKnown se => lift Sql.SqKnown (tr se)
              | Sql.Inj (e', loc) => lift' (fn fe' => Sql.Inj (fe', loc)) (f e')
              | Sql.SqFunc (s, se) => lift (fn trse => Sql.SqFunc (s, trse)) (tr se)
              | se => pure se
        in
            tr
        end

    fun traverseFitem (ops as (_, _, _, pure''', _, _, _, lift''', _, _, _, _, lift2'''', lift2''''')) f =
        let
            val rec tr =
             fn Sql.Table t => pure''' (Sql.Table t)
              | Sql.Join (jt, fi1, fi2, se) =>
                lift2'''' (fn ((trfi1, trfi2), trse) => Sql.Join (jt, trfi1, trfi2, trse))
                          (lift2''''' id (tr fi1, tr fi2), traverseSqexp ops f se)
              | Sql.Nested (q, s) => lift''' (fn trq => Sql.Nested (trq, s))
                                             (traverseQuery ops f q)
        in
            tr
        end

    and traverseQuery (ops as (_, pure', pure'', _, _, _, lift'', _, _, lift2', lift2'', lift2''', _, _)) f =
        let
            val rec seqList =
             fn [] => pure'' []
              | (x::xs) => lift2''' op:: (x, seqList xs)
            val rec tr =
             fn Sql.Query1 q =>
                (* TODO: make sure we don't need to traverse [#Select q]. *)
                lift2' (fn (trfrom, trwher) => Sql.Query1 {Select = #Select q,
                                                           From = trfrom,
                                                           Where = trwher})
                       (seqList (map (traverseFitem ops f) (#From q)),
                        case #Where q of
                            NONE => pure' NONE
                          | SOME se => lift'' SOME (traverseSqexp ops f se))
              | Sql.Union (q1, q2) => lift2'' Sql.Union (tr q1, tr q2)
        in
            tr
        end

    (* Include unused tuple elements in argument for convenience of using same
       argument as [traverseQuery]. *)
    fun traverseIM (pure, _, _, _, _, _, _, _, _, lift2, _, _, _, _) f =
        IM.foldli (fn (k, v, acc) => lift2 (fn (acc, w) => IM.insert (acc, k, w)) (acc, f (k,v)))
                  (pure IM.empty)

    fun traverseSubst (ops as (_, pure', _, _, lift, _, _, _, _, lift2', _, _, _, _)) f =
        let
            fun mp ((n, fields), sqlify) =
                lift (fn ((n', fields'), sqlify') =>
                         let
                             fun wrap sq = ((n', fields' @ fields), sq)
                         in
                             case (fields', sqlify', fields, sqlify) of
                                 (_, NONE, _, NONE) => wrap NONE
                               | (_, NONE, _, sq as SOME _) => wrap sq
                               (* Last case should suffice because we don't
                                  project from a sqlified value (which is a
                                  string). *)
                               | (_, sq as SOME _, [], NONE) => wrap sq
                               | _ => raise Fail "Sqlcache: traverseSubst"
                         end)
                     (f n)
        in
            traverseIM ops (fn (_, v) => mp v)
        end

    fun monoidOps plus zero =
        (fn _ => zero, fn _ => zero, fn _ => zero, fn _ => zero,
         fn _ => fn x => x, fn _ => fn x => x, fn _ => fn x => x, fn _ => fn x => x,
         fn _ => plus, fn _ => plus, fn _ => plus, fn _ => plus, fn _ => plus, fn _ => plus)

    val optionOps = (SOME, SOME, SOME, SOME,
                     omap, omap, omap, omap,
                     omap2, omap2, omap2, omap2, omap2, omap2)

    fun foldMapQuery plus zero = traverseQuery (monoidOps plus zero)
    val omapQuery = traverseQuery optionOps
    fun foldMapIM plus zero = traverseIM (monoidOps plus zero)
    fun omapIM f = traverseIM optionOps f
    fun foldMapSubst plus zero = traverseSubst (monoidOps plus zero)
    fun omapSubst f = traverseSubst optionOps f

    val varsOfQuery = foldMapQuery IS.union
                                   IS.empty
                                   (fn e' => freeVars (e', dummyLoc))

    fun varsOfSubst subst = foldMapSubst IS.union IS.empty IS.singleton subst

    val varsOfList =
     fn [] => IS.empty
      | (q::qs) => varsOfQuery (List.foldl Sql.Union q qs)

    (* Signature Implementation *)

    val empty = []

    fun singleton q = [(q, IS.foldl (fn (n, acc) => IM.insert (acc, n, ((n, []), NONE)))
                                    IM.empty
                                    (varsOfQuery q))]

    val union = op@

    fun sqlArgsSet (q, subst) =
        IM.foldl AS.add' AS.empty subst

    fun sqlArgsMap (qs : t) =
        let
            val args =
                List.foldl (fn ((q, subst), acc) =>
                               IM.foldl (fn (arg, acc) => AM.insert (acc, arg, ())) acc subst)
                           AM.empty
                           qs
            val countRef = ref (~1)
            fun count () = (countRef := !countRef + 1; !countRef)
        in
            (* Maps each arg to a different consecutive integer, starting from 0. *)
            AM.map count args
        end

    fun expOfArg (path, sqlify) =
        let
            val exp = expOfPath path
        in
            case sqlify of
                NONE => exp
              | SOME (m, x, typ) => (EFfiApp (m, x, [(exp, typ)]), dummyLoc)
        end

    fun orderArgs (qs : t, exp) =
        let
            val paths = freePaths exp
            fun erel n = (ERel n, dummyLoc)
            val argsMap = sqlArgsMap qs
            val args = map (expOfArg o #1) (AM.listItemsi argsMap)
            val invalPaths = List.foldl PS.union PS.empty (map freePaths args)
            (* TODO: make sure these variables are okay to remove from the argument list. *)
            val pureArgs = PS.difference (paths, invalPaths)
            val shouldCache =
                case getHeuristic () of
                    Smart =>
                    (case (qs, PS.numItems pureArgs) of
                         ((q::qs), 0) =>
                         let
                             val args = sqlArgsSet q
                             val argss = map sqlArgsSet qs
                             fun test (args, acc) =
                                 acc
                                 <\obind\>
                                  (fn args' =>
                                      let
                                          val both = AS.union (args, args')
                                      in
                                          (AS.numItems args = AS.numItems both
                                           orelse AS.numItems args' = AS.numItems both)
                                          <\oguard\>
                                           (fn _ => SOME both)
                                      end)
                         in
                             case List.foldl test (SOME args) argss of
                                 NONE => false
                               | SOME _ => true
                         end
                       | _ => false)
                  | Always => true
                  | Never => (case qs of [_] => PS.numItems pureArgs = 0 | _ => false)
                  | NoPureAll => (case qs of [] => false | _ => true)
                  | NoPureOne => (case qs of [] => false | _ => PS.numItems pureArgs = 0)
                  | NoCombo => PS.numItems pureArgs = 0 orelse AM.numItems argsMap = 0
        in
            (* Put arguments we might invalidate by first. *)
            if shouldCache
            then SOME (map AsIs args @ map (Urlify o expOfPath) (PS.listItems pureArgs))
            else NONE
        end

    (* As a kludge, we rename the variables in the query to correspond to the
       argument of the cache they're part of. *)
    fun query (qs : t) =
        let
            val argsMap = sqlArgsMap qs
            fun substitute subst =
             fn ERel n => IM.find (subst, n)
                          <\obind\>
                           (fn arg =>
                               AM.find (argsMap, arg)
                               <\obind\>
                                (fn n' => SOME (ERel n')))
              | _ => raise Fail "Sqlcache: query (a)"
        in
            case (map #1 qs) of
                (q :: qs) =>
                let
                    val q = List.foldl Sql.Union q qs
                    val ns = IS.listItems (varsOfQuery q)
                    val rename =
                     fn ERel n => omap ERel (indexOf (fn n' => n' = n) ns)
                      | _ => raise Fail "Sqlcache: query (b)"
                in
                    case omapQuery rename q of
                        SOME q => q
                      (* We should never get NONE because indexOf should never fail. *)
                      | NONE => raise Fail "Sqlcache: query (c)"
                end
              (* We should never reach this case because [updateState] won't
                 put anything in the state if there are no queries. *)
              | [] => raise Fail "Sqlcache: query (d)"
        end

    val argOfExp =
        let
            fun doFields acc exp =
                acc
                <\obind\>
                 (fn (fs, sqlify) =>
                     case #1 exp of
                         ERel n => SOME (n, fs, sqlify)
                       | EField (exp, f) => doFields (SOME (f::fs, sqlify)) exp
                       | _ => NONE)
        in
         fn (EFfiApp ("Basis", x, [(exp, typ)]), _) =>
            if String.isPrefix "sqlify" x
            then omap (fn path => (path, SOME ("Basis", x, typ))) (pathOfExp exp)
            else NONE
          | exp => omap (fn path => (path, NONE)) (pathOfExp exp)
        end

    val unbind1 =
     fn Known e =>
        let
            val replacement = argOfExp e
        in
            omapSubst (fn 0 => replacement
                        | n => SOME ((n-1, []), NONE))
        end
      | Unknowns k => omapSubst (fn n => if n < k then NONE else SOME ((n-k, []), NONE))

    fun unbind (qs, ub) =
        case ub of
            (* Shortcut if nothing's changing. *)
            Unknowns 0 => SOME qs
          | _ => osequence (map (fn (q, subst) => unbind1 ub subst
                                                  <\obind\>
                                                   (fn subst' => SOME (q, subst'))) qs)

    fun updateState (qs, numArgs, state as {index, ...} : state) =
        {tableToIndices = List.foldr (fn ((q, _), acc) =>
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

val rec sqexpToFormula =
 fn Sql.SqTrue => Combo (Conj, [])
  | Sql.SqFalse => Combo (Disj, [])
  | Sql.SqNot e => Negate (sqexpToFormula e)
  | Sql.Binop (Sql.RCmp c, e1, e2) => Atom (c, e1, e2)
  | Sql.Binop (Sql.RLop l, p1, p2) => Combo (case l of Sql.And => Conj | Sql.Or => Disj,
                                             [sqexpToFormula p1, sqexpToFormula p2])
  | e as Sql.Field f => Atom (Sql.Eq, e, Sql.SqTrue)
  (* ASK: any other sqexps that can be props? *)
  | Sql.SqConst prim =>
    (case prim of
         (Prim.String (Prim.Normal, s)) =>
         if s = #trueString (Settings.currentDbms ())
         then Combo (Conj, [])
         else if s = #falseString (Settings.currentDbms ())
         then Combo (Disj, [])
         else raise Fail "Sqlcache: sqexpToFormula (SqConst a)"
       | _ => raise Fail "Sqlcache: sqexpToFormula (SqConst b)")
  | Sql.Computed _ => raise Fail "Sqlcache: sqexpToFormula (Computed)"
  | Sql.SqKnown _ => raise Fail "Sqlcache: sqexpToFormula (SqKnown)"
  | Sql.Inj _ => raise Fail "Sqlcache: sqexpToFormula (Inj)"
  | Sql.SqFunc _ => raise Fail "Sqlcache: sqexpToFormula (SqFunc)"
  | Sql.Unmodeled => raise Fail "Sqlcache: sqexpToFormula (Unmodeled)"
  | Sql.Null => raise Fail "Sqlcache: sqexpToFormula (Null)"

fun mapSqexpFields f =
 fn Sql.Field (t, v) => f (t, v)
  | Sql.SqNot e => Sql.SqNot (mapSqexpFields f e)
  | Sql.Binop (r, e1, e2) => Sql.Binop (r, mapSqexpFields f e1, mapSqexpFields f e2)
  | Sql.SqKnown e => Sql.SqKnown (mapSqexpFields f e)
  | Sql.SqFunc (s, e) => Sql.SqFunc (s, mapSqexpFields f e)
  | e => e

fun renameTables tablePairs =
    let
        fun rename table =
            case List.find (fn (_, t) => table = t) tablePairs of
                NONE => table
              | SOME (realTable, _) => realTable
    in
        mapSqexpFields (fn (t, f) => Sql.Field (rename t, f))
    end

structure FlattenQuery = struct

    datatype substitution = RenameTable of string | SubstituteExp of Sql.sqexp SM.map

    fun applySubst substTable =
        let
            fun substitute (table, field) =
                case SM.find (substTable, table) of
                    NONE => Sql.Field (table, field)
                  | SOME (RenameTable realTable) => Sql.Field (realTable, field)
                  | SOME (SubstituteExp substField) =>
                    case SM.find (substField, field) of
                        NONE => raise Fail "Sqlcache: applySubst"
                      | SOME se => se
        in
            mapSqexpFields substitute
        end

    fun addToSubst (substTable, table, substField) =
        SM.insert (substTable,
                   table,
                   case substField of
                       RenameTable _ => substField
                     | SubstituteExp subst => SubstituteExp (SM.map (applySubst substTable) subst))

    fun newSubst (t, s) = addToSubst (SM.empty, t, s)

    datatype sitem' = Named of Sql.sqexp * string | Unnamed of Sql.sqexp

    type queryFlat = {Select : sitem' list, Where : Sql.sqexp}

    val sitemsToSubst =
        List.foldl (fn (Named (se, s), acc) => SM.insert (acc, s, se)
                     | (Unnamed _, _) => raise Fail "Sqlcache: sitemsToSubst")
                   SM.empty

    fun unionSubst (s1, s2) = SM.unionWith (fn _ => raise Fail "Sqlcache: unionSubst") (s1, s2)

    fun sqlAnd (se1, se2) = Sql.Binop (Sql.RLop Sql.And, se1, se2)

    val rec flattenFitem : Sql.fitem -> (Sql.sqexp * substitution SM.map) list =
     fn Sql.Table (real, alias) => [(Sql.SqTrue, newSubst (alias, RenameTable real))]
      | Sql.Nested (q, s) =>
        let
            val qfs = flattenQuery q
        in
            map (fn (qf, subst) =>
                    (#Where qf, addToSubst (subst, s, SubstituteExp (sitemsToSubst (#Select qf)))))
                qfs
        end
      | Sql.Join (jt, fi1, fi2, se) =>
        concatMap (fn ((wher1, subst1)) =>
                      map (fn (wher2, subst2) =>
                              let
                                  val subst = unionSubst (subst1, subst2)
                              in
                                  (* ON clause becomes part of the accumulated WHERE. *)
                                  (sqlAnd (sqlAnd (wher1, wher2), applySubst subst se), subst)
                              end)
                          (flattenFitem fi2))
                  (flattenFitem fi1)

    and flattenQuery : Sql.query -> (queryFlat * substitution SM.map) list =
     fn Sql.Query1 q =>
        let
            val fifss = cartesianProduct (map flattenFitem (#From q))
        in
            map (fn fifs =>
                    let
                        val subst = List.foldl (fn ((_, subst), acc) => unionSubst (acc, subst))
                                               SM.empty
                                               fifs
                        val wher = List.foldr (fn ((wher, _), acc) => sqlAnd (wher, acc))
                                              (case #Where q of
                                                   NONE => Sql.SqTrue
                                                 | SOME wher => wher)
                                              fifs
                    in
                        (* ASK: do we actually need to pass the substitution through here? *)
                        (* We use the substitution later, but it's not clear we
                       need any of its currently present fields again. *)
                        ({Select = map (fn Sql.SqExp (se, s) => Named (applySubst subst se, s)
                                         | Sql.SqField tf =>
                                           Unnamed (applySubst subst (Sql.Field tf)))
                                       (#Select q),
                          Where = applySubst subst wher},
                         subst)
                    end)
                fifss
        end
      | Sql.Union (q1, q2) => (flattenQuery q1) @ (flattenQuery q2)

end

val flattenQuery = map #1 o FlattenQuery.flattenQuery

fun queryFlatToFormula marker {Select = sitems, Where = wher} =
    let
        val fWhere = sqexpToFormula wher
    in
        case marker of
             NONE => fWhere
           | SOME markFields =>
             let
                 val fWhereMarked = mapFormulaExps markFields fWhere
                 val toSqexp =
                  fn FlattenQuery.Named (se, _) => se
                   | FlattenQuery.Unnamed se => se
                 fun ineq se = Atom (Sql.Ne, se, markFields se)
                 val fIneqs = Combo (Disj, map (ineq o toSqexp) sitems)
             in
                 (Combo (Conj,
                         [fWhere,
                          Combo (Disj,
                                 [Negate fWhereMarked,
                                  Combo (Conj, [fWhereMarked, fIneqs])])]))
             end
    end

fun queryToFormula marker q = Combo (Disj, map (queryFlatToFormula marker) (flattenQuery q))

fun valsToFormula (markLeft, markRight) (table, vals) =
    Combo (Conj,
           map (fn (field, v) => Atom (Sql.Eq, markLeft (Sql.Field (table, field)), markRight v))
               vals)

(* TODO: verify logic for insertion and deletion. *)
val rec dmlToFormulaMarker =
 fn Sql.Insert (table, vals) => (valsToFormula (id, id) (table, vals), NONE)
  | Sql.Delete (table, wher) => (sqexpToFormula (renameTables [(table, "T")] wher), NONE)
  | Sql.Update (table, vals, wher) =>
    let
        val fWhere = sqexpToFormula (renameTables [(table, "T")] wher)
        fun fVals marks = valsToFormula marks (table, vals)
        val modifiedFields = SS.addList (SS.empty, map #1 vals)
        (* TODO: don't use field name hack. *)
        val markFields =
            mapSqexpFields (fn (t, v) => if t = table andalso SS.member (modifiedFields, v)
                                         then Sql.Field (t, v ^ "'")
                                         else Sql.Field (t, v))
        val mark = mapFormulaExps markFields
    in
        ((Combo (Disj, [Combo (Conj, [fVals (id, markFields), mark fWhere]),
                        Combo (Conj, [fVals (markFields, id), fWhere])])),
         SOME markFields)
    end

fun pairToFormulas (query, dml) =
    let
        val (fDml, marker) = dmlToFormulaMarker dml
    in
        (queryToFormula marker query, fDml)
    end

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

    fun equivClasses atoms : atomExp list list option =
        let
            val uf = List.foldl UF.union' UF.empty (List.mapPartial toKnownEquality atoms)
            val contradiction =
             fn (cmp, SOME ae1, SOME ae2) => (cmp = Sql.Ne orelse cmp = Sql.Lt orelse cmp = Sql.Gt)
                                             andalso UF.together (uf, ae1, ae2)
              (* If we don't know one side of the comparision, not a contradiction. *)
              | _ => false
        in
            not (List.exists contradiction atoms) <\oguard\> (fn _ => SOME (UF.classes uf))
        end

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
       equivalence classes. *)
    val mergeEqs : (atomExp IntBinaryMap.map option list
                    -> atomExp IntBinaryMap.map option) =
        List.foldr (omap2 (IM.unionWith (fn _ => raise Fail "Sqlcache: ConflictMaps.mergeEqs")))
                   (SOME IM.empty)

    val simplify =
        map TS.listItems
        o removeRedundant (fn (x, y) => TS.isSubset (y, x))
        o map (fn xs => TS.addList (TS.empty, xs))

    fun dnf (fQuery, fDml) =
        normalize simplify normalizeAtom Disj (Combo (Conj, [markQuery fQuery, markDml fDml]))

    val conflictMaps =
        List.mapPartial (mergeEqs o map eqsOfClass)
        o List.mapPartial equivClasses
        o dnf

end

val conflictMaps = ConflictMaps.conflictMaps


(*************************************)
(* Program Instrumentation Utilities *)
(*************************************)

val {check, store, flush, lock, ...} = getCache ()

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
  | _ => raise Fail "Sqlcache: sequence"

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
      | Search.Return _ => raise Fail "Sqlcache: fileAllMapfoldB"

fun fileMap doExp file = #1 (fileAllMapfoldB (fn _ => fn e => fn _ => (doExp e, ())) file ())

(* TODO: make this a bit prettier.... *)
(* TODO: factour out identical subexpressions to the same variable.... *)
val simplifySql =
    let
        fun factorOutNontrivial text =
            let
                val loc = dummyLoc
                val strcat =
                 fn (e1, (EPrim (Prim.String (Prim.Normal, "")), _)) => e1
                  | ((EPrim (Prim.String (Prim.Normal, "")), _), e2) => e2
                  | (e1, e2) => (EStrcat (e1, e2), loc)
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
                    List.foldl (fn (v, e') => ELet ("sqlArg", stringTyp, v, (e', loc)))
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
                             | _ => raise Fail "Sqlcache: simplifySql (a)"
                val (newText, wrapLets, numArgs) = factorOutNontrivial text
                val newExp' = case exp' of
                                 EQuery q => EQuery {query = newText,
                                                     exps = #exps q,
                                                     tables = #tables q,
                                                     state = #state q,
                                                     body = #body q,
                                                     initial = #initial q}
                               | EDml (_, failureMode) => EDml (newText, failureMode)
                               | _ => raise Fail "Sqlcache: simplifySql (b)"
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
                          omap #2 (List.find (fn (s', _) => s = s') fields)
                        | _ => NONE)
  | ECase (_, _, {result, ...}) => SOME result
  | EStrcat _ => SOME (TFfi ("Basis", "string"), dummyLoc)
  | EWrite _ => SOME (TRecord [], dummyLoc)
  | ESeq (_, e) => typOfExp env e
  | ELet (s, t, e1, e2) => typOfExp (MonoEnv.pushERel env s t (SOME e1)) e2
  | EClosure _ => NONE
  | EUnurlify (_, t, _) => SOME t
  | EQuery {state, ...} => SOME state
  | e => NONE

and typOfExp env (e', loc) = typOfExp' env e'


(***********)
(* Caching *)
(***********)

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
  | Impure _ => raise Fail "Sqlcache: invalInfoOfSubexp"

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
                               (ELet ("q", typ, exp, (ESeq (store, rel0), loc)), loc)),
                              ((PSome (stringTyp, (PVar ("hit", stringTyp), loc)), loc),
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

fun cacheExp (env, exp', invalInfo, state : state) =
    case worthCaching exp' <\oguard\> (fn _ => typOfExp' env exp') of
        NONE => NONE
      | SOME (TFun _, _) => NONE
      | SOME typ =>
        InvalInfo.orderArgs (invalInfo, (exp', dummyLoc))
        <\obind\>
         (fn args =>
             List.foldr (fn (arg, acc) =>
                            acc
                            <\obind\>
                             (fn args' =>
                                 (case arg of
                                      AsIs exp => SOME exp
                                    | Urlify exp =>
                                      (typOfExp env exp)
                                      <\obind\>
                                       (fn typ => MonoFooify.urlify env (exp, typ)))
                                 <\obind\>
                                  (fn arg' => SOME (arg' :: args'))))
                        (SOME [])
                        args
             <\obind\>
              (fn args' =>
                  cacheWrap (env, (exp', dummyLoc), typ, args', #index state)
                  <\obind\>
                   (fn cachedExp =>
                       SOME (cachedExp,
                             InvalInfo.updateState (invalInfo, length args', state)))))

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
        val attempt =
            (* Ziv misses Haskell's do notation.... *)
            (safe 0 queryText andalso safe 0 initial andalso safe 2 body)
            <\oguard\>
             (fn _ =>
                 Sql.parse Sql.query queryText
                 <\obind\>
                  (fn queryParsed =>
                      let
                          val invalInfo = InvalInfo.singleton queryParsed
                          fun mkExp state =
                              case cacheExp (env, EQuery q, invalInfo, state) of
                                  NONE => ((EQuery q, dummyLoc), state)
                                | SOME (cachedExp, state) => ((cachedExp, dummyLoc), state)
                      in
                          SOME (Cachable (invalInfo, mkExp))
                      end))
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
                             <\obind\>
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
            wrapBindN (fn [arg] => f arg
                        | _ => raise Fail "Sqlcache: cacheTree (a)") [arg]
        fun wrapBind2 f (arg1, arg2) =
            wrapBindN (fn [arg1, arg2] => f (arg1, arg2)
                        | _ => raise Fail "Sqlcache: cacheTree (b)") [arg1, arg2]
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
                        | _ => raise Fail "Sqlcache: cacheTree (c)")
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
                             | _ => raise Fail "Sqlcache: Invalidations.optionAtomExpToExp",
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

    fun invalidations ((invalInfo, numArgs), dml) =
        let
            val query = InvalInfo.query invalInfo
        in
            (map (map optionAtomExpToExp)
             o removeRedundant madeRedundantBy
             o map (eqsToInvalidation numArgs)
             o conflictMaps)
                (pairToFormulas (query, dml))
        end

end

val invalidations = Invalidations.invalidations

fun addFlushing ((file, {tableToIndices, indexToInvalInfo, ffiInfo, ...} : state), effs) =
    let
        val flushes = List.concat
                      o map (fn (i, argss) => map (fn args => flush (i, args)) argss)
        val doExp =
         fn dmlExp as EDml (dmlText, failureMode) =>
            let
                val inval =
                    case Sql.parse Sql.dml dmlText of
                        SOME dmlParsed =>
                        SOME (map (fn i => case IM.find (indexToInvalInfo, i) of
                                                SOME invalInfo =>
                                                (i, invalidations (invalInfo, dmlParsed))
                                              (* TODO: fail more gracefully. *)
                                              (* This probably means invalidating everything.... *)
                                              | NONE => raise Fail "Sqlcache: addFlushing (a)")
                                  (SIMM.findList (tableToIndices, tableOfDml dmlParsed)))
                      | NONE => NONE
            in
                case inval of
                    (* TODO: fail more gracefully. *)
                    NONE => (Print.preface ("DML", MonoPrint.p_exp MonoEnv.empty dmlText);
                             raise Fail "Sqlcache: addFlushing (b)")
                  | SOME invs => sequence (flushes invs @ [dmlExp])
            end
          | e' => e'
        val file = fileMap doExp file

    in
        ffiInfoRef := ffiInfo;
        file
    end


(***********)
(* Locking *)
(***********)

(* TODO: do this less evilly by not relying on specific FFI names, please? *)
fun locksNeeded (lockMap : {store : IIMM.multimap, flush : IIMM.multimap}) =
    MonoUtil.Exp.fold
        {typ = #2,
         exp = fn (EFfiApp ("Sqlcache", x, _), state as {store, flush}) =>
                  (case Int.fromString (String.extract (x, 5, NONE)) of
                       NONE => state
                     | SOME index =>
                       if String.isPrefix "flush" x
                       then {store = store, flush = IS.add (flush, index)}
                       else if String.isPrefix "store" x
                       then {store = IS.add (store, index), flush = flush}
                       else state)
         | (ENamed n, {store, flush}) =>
           {store = IS.union (store, IIMM.findSet (#store lockMap, n)),
            flush = IS.union (flush, IIMM.findSet (#flush lockMap, n))}
         | (_, state) => state}
        {store = IS.empty, flush = IS.empty}

fun lockMapOfFile file =
    transitiveAnalysis
        (fn ((_, name, _, e, _), state) =>
            let
                val locks = locksNeeded state e
            in
                {store = IIMM.insertSet (#store state, name, #store locks),
                 flush = IIMM.insertSet (#flush state, name, #flush locks)}
            end)
        {store = IIMM.empty, flush = IIMM.empty}
        file

fun exports (decls, _) =
    List.foldl (fn ((DExport (_, _, n, _, _, _), _), ns) => IS.add (ns, n)
                 | (_, ns) => ns)
               IS.empty
               decls

fun wrapLocks (locks, (exp', loc)) =
    case exp' of
        EAbs (s, t1, t2, exp) => (EAbs (s, t1, t2, wrapLocks (locks, exp)), loc)
      | _ => (List.foldr (fn (l, e') => sequence [lock l, e']) exp' locks, loc)

fun addLocking file =
    let
        val lockMap = lockMapOfFile file
        fun lockList {store, flush} =
            let
                val ls = map (fn i => (i, true)) (IS.listItems flush)
                         @ map (fn i => (i, false)) (IS.listItems (IS.difference (store, flush)))
            in
                ListMergeSort.sort (fn ((i, _), (j, _)) => i > j) ls
            end
        fun locksOfName n =
            lockList {flush = IIMM.findSet (#flush lockMap, n),
                      store = IIMM.findSet (#store lockMap, n)}
        val locksOfExp = lockList o locksNeeded lockMap
        val expts = exports file
        fun doVal (v as (x, n, t, exp, s)) =
            if IS.member (expts, n)
            then (x, n, t, wrapLocks ((locksOfName n), exp), s)
            else v
        val doDecl =
         fn (DVal v, loc) => (DVal (doVal v), loc)
          | (DValRec vs, loc) => (DValRec (map doVal vs), loc)
          | (DTask (exp1, exp2), loc) => (DTask (exp1, wrapLocks (locksOfExp exp2, exp2)), loc)
          | decl => decl
    in
        mapFst (map doDecl) file
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

val go' = addLocking o addFlushing o addCaching o simplifySql o inlineSql

fun go file =
    let
        (* TODO: do something nicer than [Sql] being in one of two modes. *)
        val () = (resetFfiInfo (); Sql.sqlcacheMode := true)
        val file = go' file
        (* Important that this happens after [MonoFooify.urlify] calls! *)
        val fmDecls = MonoFooify.getNewFmDecls ()
        val () = Sql.sqlcacheMode := false
        val file = insertAfterDatatypes (file, rev fmDecls)
    in
        MonoReduce.reduce file
    end

end
