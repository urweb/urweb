structure Sqlcache (* DEBUG: add back :> SQLCACHE. *) = struct

open Mono

structure IS = IntBinarySet
structure IM = IntBinaryMap
structure SK = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure SIMM = MultimapFn(structure KeyMap = SM structure ValSet = IS)

(* Filled in by [cacheWrap] during [Sqlcache]. *)
val ffiInfo : {index : int, params : int} list ref = ref []

fun resetFfiInfo () = ffiInfo := []

fun getFfiInfo () = !ffiInfo

(* Some FFIs have writing as their only effect, which the caching records. *)
val ffiEffectful =
    (* ASK: how can this be less hard-coded? *)
    let
        val fs = SS.fromList ["htmlifyInt_w",
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
        fn (m, f) => Settings.isEffectful (m, f)
                     andalso not (m = "Basis" andalso SS.member (fs, f))
    end

val cache = ref LruCache.cache
fun setCache c = cache := c
fun getCache () = !cache


(* Effect analysis. *)

(* Makes an exception for [EWrite] (which is recorded when caching). *)
fun effectful doPrint (effs : IS.set) (inFunction : bool) (bound : int) : exp -> bool =
    (* If result is true, expression is definitely effectful. If result is
       false, then expression is definitely not effectful if effs is fully
       populated. The intended pattern is to use this a number of times equal
       to the number of declarations in a file, Bellman-Ford style. *)
    (* TODO: make incrementing of the number of bound variables cleaner,
       probably by using [MonoUtil] instead of all this. *)
    let
        (* DEBUG: remove printing when done. *)
        fun tru msg = if doPrint then (print (msg ^ "\n"); true) else true
        val rec eff' =
         (* ASK: is there a better way? *)
         fn EPrim _ => false
          (* We don't know if local functions have effects when applied. *)
          | ERel idx => if inFunction andalso idx >= bound
                        then tru ("rel" ^ Int.toString idx) else false
          | ENamed name => if IS.member (effs, name) then tru "named" else false
          | ECon (_, _, NONE) => false
          | ECon (_, _, SOME e) => eff e
          | ENone _ => false
          | ESome (_, e) => eff e
          | EFfi (m, f) => if ffiEffectful (m, f) then tru "ffi" else false
          | EFfiApp (m, f, _) => if ffiEffectful (m, f) then tru "ffiapp" else false
          (* ASK: we're calling functions effectful if they have effects when
             applied or if the function expressions themselves have effects.
             Is that okay? *)
          (* This is okay because the values we ultimately care about aren't
             functions, and this is a conservative approximation, anyway. *)
          | EApp (eFun, eArg) => effectful doPrint effs true bound eFun orelse eff eArg
          | EAbs (_, _, _, e) => effectful doPrint effs inFunction (bound+1) e
          | EUnop (_, e) => eff e
          | EBinop (_, _, e1, e2) => eff e1 orelse eff e2
          | ERecord xs => List.exists (fn (_, e, _) => eff e) xs
          | EField (e, _) => eff e
          (* If any case could be effectful, consider it effectful. *)
          | ECase (e, xs, _) => eff e orelse List.exists (fn (_, e) => eff e) xs
          | EStrcat (e1, e2) => eff e1 orelse eff e2
          (* ASK: how should we treat these three? *)
          | EError _ => tru "error"
          | EReturnBlob _ => tru "blob"
          | ERedirect _ => tru "redirect"
          (* EWrite is a special exception because we record writes when caching. *)
          | EWrite _ => false
          | ESeq (e1, e2) => eff e1 orelse eff e2
          (* TODO: keep context of which local variables aren't effectful? Only
             makes a difference for function expressions, though. *)
          | ELet (_, _, eBind, eBody) => eff eBind orelse
                                         effectful doPrint effs inFunction (bound+1) eBody
          | EClosure (_, es) => List.exists eff es
          (* TODO: deal with EQuery. *)
          | EQuery _ => tru "query"
          | EDml _ => tru "dml"
          | ENextval _ => tru "nextval"
          | ESetval _ => tru "setval"
          | EUnurlify (e, _, _) => eff e
          (* ASK: how should we treat this? *)
          | EJavaScript _ => tru "javascript"
          (* ASK: these are all effectful, right? *)
          | ESignalReturn _ => tru "signalreturn"
          | ESignalBind _ => tru "signalbind"
          | ESignalSource _ => tru "signalsource"
          | EServerCall _ => tru "servercall"
          | ERecv _ => tru "recv"
          | ESleep _ => tru "sleep"
          | ESpawn _ => tru "spawn"
        and eff = fn (e', _) => eff' e'
    in
        eff
    end

(* TODO: test this. *)
val effectfulMap =
    let
        fun doVal ((_, name, _, e, _), effMap) =
            if effectful false effMap false 0 e
            then IS.add (effMap, name)
            else effMap
        val doDecl =
         fn (DVal v, effMap) => doVal (v, effMap)
          (* Repeat the list of declarations a number of times equal to its size. *)
          | (DValRec vs, effMap) =>
            List.foldl doVal effMap (List.concat (List.map (fn _ => vs) vs))
          (* ASK: any other cases? *)
          | (_, effMap) => effMap
    in
        MonoUtil.File.fold {typ = #2, exp = #2, decl = doDecl} IS.empty
    end


(* Boolean formula normalization. *)

datatype junctionType = Conj | Disj

datatype 'atom formula =
         Atom of 'atom
       | Negate of 'atom formula
       | Combo of junctionType * 'atom formula list

val flipJt = fn Conj => Disj | Disj => Conj

fun concatMap f xs = List.concat (map f xs)

val rec cartesianProduct : 'a list list -> 'a list list =
 fn [] => [[]]
  | (xs :: xss) => concatMap (fn ys => concatMap (fn x => [x :: ys]) xs)
                             (cartesianProduct xss)

(* Pushes all negation to the atoms.*)
fun pushNegate (negate : 'atom -> 'atom) (negating : bool) =
 fn Atom x => Atom (if negating then negate x else x)
  | Negate f => pushNegate negate (not negating) f
  | Combo (n, fs) => Combo (if negating then flipJt n else n, map (pushNegate negate negating) fs)

val rec flatten =
 fn Combo (_, [f]) => flatten f
  | Combo (j, fs) =>
    Combo (j, List.foldr (fn (f, acc) =>
                             case f of
                                 Combo (j', fs') =>
                                 if j = j' orelse length fs' = 1
                                 then fs' @ acc
                                 else f :: acc
                               | _ => f :: acc)
                         []
                         (map flatten fs))
  | f => f

fun normalize' (simplify : 'a list list -> 'a list list)
               (negate : 'a -> 'a)
               (junc : junctionType) =
    let
        fun norm junc =
            simplify
            o (fn Atom x => [[x]]
                | Negate f => map (map negate) (norm (flipJt junc) f)
                | Combo (j, fs) =>
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

fun normalize simplify negate junc =
    normalize' simplify negate junc
    o flatten
    o pushNegate negate false

fun mapFormula mf =
 fn Atom x => Atom (mf x)
  | Negate f => Negate (mapFormula mf f)
  | Combo (j, fs) => Combo (j, map (mapFormula mf) fs)


(* SQL analysis. *)

structure CmpKey : ORD_KEY = struct

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

(*
functor ListKeyFn (K : ORD_KEY) : ORD_KEY = struct

    type ord_key = K.ord_key list

    val rec compare =
     fn ([], []) => EQUAL
      | ([], _) => LESS
      | (_, []) => GREATER
      | (x :: xs, y :: ys) => (case K.compare (x, y) of
                                   EQUAL => compare (xs, ys)
                                 | ord => ord)

end
*)

functor OptionKeyFn (K : ORD_KEY) : ORD_KEY = struct

    type ord_key = K.ord_key option

    val compare =
     fn (NONE, NONE) => EQUAL
      | (NONE, _) => LESS
      | (_, NONE) => GREATER
      | (SOME x, SOME y) => K.compare (x, y)

end

functor TripleKeyFn (structure I : ORD_KEY
                     structure J : ORD_KEY
                     structure K : ORD_KEY)
        : ORD_KEY where type ord_key = I.ord_key * J.ord_key * K.ord_key = struct

    type ord_key = I.ord_key * J.ord_key * K.ord_key

    fun compare ((i1, j1, k1), (i2, j2, k2)) =
        case I.compare (i1, i2) of
            EQUAL => (case J.compare (j1, j2) of
                          EQUAL => K.compare (k1, k2)
                        | ord => ord)
          | ord => ord

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

structure UF = UnionFindFn(AtomExpKey)

structure ConflictMaps = struct

    structure TK = TripleKeyFn(structure I = CmpKey
                               structure J = OptionKeyFn(AtomExpKey)
                               structure K = OptionKeyFn(AtomExpKey))
    structure TS = BinarySetFn(TK)
    (* structure TLS = BinarySetFn(ListKeyFn(TK)) *)

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

    fun negateCmp (cmp, e1, e2) =
        (case cmp of
             Sql.Eq => Sql.Ne
           | Sql.Ne => Sql.Eq
           | Sql.Lt => Sql.Ge
           | Sql.Le => Sql.Gt
           | Sql.Gt => Sql.Le
           | Sql.Ge => Sql.Lt,
         e1, e2)

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
        List.foldr (fn (SOME eqs, SOME acc) => SOME (IM.unionWith #1 (eqs, acc)) | _ => NONE)
                   (SOME IM.empty)

    fun dnf (fQuery, fDml) =
        let
            val simplify =
                map TS.listItems
                o removeRedundant (fn (x, y) => TS.isSubset (y, x))
                o map (fn xs => TS.addList (TS.empty, xs))
        in
            normalize simplify negateCmp Disj (Combo (Conj, [markQuery fQuery, markDml fDml]))
        end

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

val rec tablesQuery =
 fn Sql.Query1 {From = tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Sql.Union (q1, q2) => SS.union (tablesQuery q1, tablesQuery q2)

val tableDml =
 fn Sql.Insert (tab, _) => tab
  | Sql.Delete (tab, _) => tab
  | Sql.Update (tab, _, _) => tab


(* Program instrumentation. *)

val varName =
    let
        val varNumber = ref 0
    in
        fn s => (varNumber := !varNumber + 1; s ^ Int.toString (!varNumber))
    end

val {check, store, flush, ...} = getCache ()

val dummyLoc = ErrorMsg.dummySpan

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

(* Always increments negative indices because that's what we need later. *)
fun incRelsBound bound inc =
    MonoUtil.Exp.mapB
        {typ = fn x => x,
         exp = fn level =>
                  (fn ERel n => ERel (if n >= level orelse n < 0 then n + inc else n)
                    | x => x),
         bind = fn (level, MonoUtil.Exp.RelE _) => level + 1 | (level, _) => level}
        bound

val incRels = incRelsBound 0

fun cacheWrap (query, i, urlifiedRel0, resultTyp, args) =
    let
        val () = ffiInfo := {index = i, params = length args} :: !ffiInfo
        val loc = dummyLoc
        (* We ensure before this step that all arguments aren't effectful.
           by turning them into local variables as needed. *)
        val argsInc = map (incRels 1) args
        val check = (check (i, args), dummyLoc)
        val store = (store (i, argsInc, urlifiedRel0), dummyLoc)
        val rel0 = (ERel 0, loc)
    in
        ECase (check,
               [((PNone stringTyp, loc),
                 (ELet (varName "q", resultTyp, query, (ESeq (store, rel0), loc)), loc)),
                ((PSome (stringTyp, (PVar (varName "hit", stringTyp), loc)), loc),
                 (* Boolean is false because we're not unurlifying from a cookie. *)
                 (EUnurlify (rel0, resultTyp, false), loc))],
               {disc = stringTyp, result = resultTyp})
    end

fun fileMapfold doExp file start =
    case MonoUtil.File.mapfold {typ = Search.return2,
                                exp = fn x => (fn s => Search.Continue (doExp x s)),
                                decl = Search.return2} file start of
        Search.Continue x => x
      | Search.Return _ => raise Match

fun fileMap doExp file = #1 (fileMapfold (fn x => fn _ => (doExp x, ())) file ())

fun factorOutNontrivial text =
    let
        val loc = dummyLoc
        fun strcat (e1, e2) = (EStrcat (e1, e2), loc)
        val chunks = Sql.chunkify text
        val (newText, newVariables) =
            (* Important that this is foldr (to oppose foldl below). *)
            List.foldr
                (fn (chunk, (qText, newVars)) =>
                    (* Variable bound to the head of newBs will have the lowest index. *)
                    case chunk of
                        Sql.Exp (e as (EPrim _, _)) => (strcat (e, qText), newVars)
                      | Sql.Exp e =>
                        let
                            val n = length newVars
                        in
                            (* This is the (n + 1)th new variable, so there are
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

fun addChecking file =
    let
        fun doExp (queryInfo as (tableToIndices, indexToQueryNumArgs, index)) =
         fn e' as EQuery {query = origQueryText,
                          sqlcacheInfo = urlifiedRel0,
                          state = resultTyp,
                          initial, body, tables, exps} =>
            let
                val (newQueryText, wrapLets, numArgs) = factorOutNontrivial origQueryText
                (* Increment once for each new variable just made. *)
                val queryExp = incRels numArgs
                                       (EQuery {query = newQueryText,
                                                sqlcacheInfo = urlifiedRel0,
                                                state = resultTyp,
                                                initial = initial,
                                                body = body,
                                                tables = tables,
                                                exps = exps},
                                        dummyLoc)
                val (EQuery {query = queryText, ...}, _) = queryExp
                (* DEBUG *)
                val () = Print.preface ("sqlcache> ", (MonoPrint.p_exp MonoEnv.empty queryText))
                val args = List.tabulate (numArgs, fn n => (ERel n, dummyLoc))
                fun bind x f = Option.mapPartial f x
                fun guard b x = if b then x else NONE
                (* DEBUG: set first boolean argument to true to turn on printing. *)
                fun safe bound = not o effectful true (effectfulMap file) false bound
                val attempt =
                    (* Ziv misses Haskell's do notation.... *)
                    guard (safe 0 queryText andalso safe 0 initial andalso safe 2 body) (
                    bind (Sql.parse Sql.query queryText) (fn queryParsed =>
                    SOME (wrapLets (cacheWrap (queryExp, index, urlifiedRel0, resultTyp, args)),
                          (SS.foldr (fn (tab, qi) => SIMM.insert (qi, tab, index))
                                    tableToIndices
                                    (tablesQuery queryParsed),
                           IM.insert (indexToQueryNumArgs, index, (queryParsed, numArgs)),
                           index + 1))))
            in
                case attempt of
                    SOME pair => pair
                  | NONE => (e', queryInfo)
            end
          | e' => (e', queryInfo)
    in
        fileMapfold (fn exp => fn state => doExp state exp) file (SIMM.empty, IM.empty, 0)
    end

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
        let
            fun inv n = if n < 0 then [] else IM.find (eqs, n) :: inv (n - 1)
        in
            inv (numArgs - 1)
        end

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

    fun invalidations ((query, numArgs), dml) =
        (map (map optionAtomExpToExp)
         o removeRedundant madeRedundantBy
         o map (eqsToInvalidation numArgs)
         o eqss)
            (query, dml)

end

val invalidations = Invalidations.invalidations

(* DEBUG *)
val gunk : ((Sql.query * int) * Sql.dml) list ref = ref []

fun addFlushing (file, (tableToIndices, indexToQueryNumArgs, _)) =
    let
        val flushes = List.concat o
                      map (fn (i, argss) => map (fn args => flush (i, args)) argss)
        val doExp =
         fn EDml (origDmlText, failureMode) =>
            let
                val (newDmlText, wrapLets, numArgs) = factorOutNontrivial origDmlText
                val dmlText = incRels numArgs newDmlText
                val dmlExp = EDml (dmlText, failureMode)
                (* DEBUG *)
                val () = Print.preface ("sqlcache> ", (MonoPrint.p_exp MonoEnv.empty dmlText))
                val invs =
                    case Sql.parse Sql.dml dmlText of
                        SOME dmlParsed =>
                        map (fn i => (case IM.find (indexToQueryNumArgs, i) of
                                          SOME queryNumArgs =>
                                          (* DEBUG *)
                                          (gunk := (queryNumArgs, dmlParsed) :: !gunk;
                                           (i, invalidations (queryNumArgs, dmlParsed)))
                                        (* TODO: fail more gracefully. *)
                                        | NONE => raise Match))
                            (SIMM.findList (tableToIndices, tableDml dmlParsed))
                      (* TODO: fail more gracefully. *)
                      | NONE => raise Match
            in
                wrapLets (sequence (flushes invs @ [dmlExp]))
            end
          | e' => e'
    in
        (* DEBUG *)
        gunk := [];
        fileMap doExp file
    end

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

fun go file =
    let
        (* TODO: do something nicer than [Sql] being in one of two modes. *)
        val () = (resetFfiInfo (); Sql.sqlcacheMode := true)
        val file' = addFlushing (addChecking (inlineSql file))
        val () = Sql.sqlcacheMode := false
    in
        file'
    end

end
