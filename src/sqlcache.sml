structure Sqlcache (* :> SQLCACHE *) = struct

open Mono

structure IS = IntBinarySet
structure IM = IntBinaryMap
structure SK = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure SIMM = MultimapFn(structure KeyMap = SM structure ValSet = IS)

(* Filled in by [cacheWrap] during [Sqlcache]. *)
val ffiInfo : {index : int, params : int} list ref = ref []

fun getFfiInfo () = !ffiInfo

(* Some FFIs have writing as their only effect, which the caching records. *)
val ffiEffectful =
    (* TODO: have this less hard-coded. *)
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


(* Effect analysis. *)

(* Makes an exception for [EWrite] (which is recorded when caching). *)
fun effectful doPrint (effs : IS.set) (inFunction : bool) (bound : int) : Mono.exp -> bool =
    (* If result is true, expression is definitely effectful. If result is
       false, then expression is definitely not effectful if effs is fully
       populated. The intended pattern is to use this a number of times equal
       to the number of declarations in a file, Bellman-Ford style. *)
    (* TODO: make incrementing of bound less janky, probably by using MonoUtil
       instead of all this. *)
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

datatype normalForm = Cnf | Dnf

datatype 'atom formula =
         Atom of 'atom
       | Negate of 'atom formula
       | Combo of normalForm * 'atom formula list

val flipNf = fn Cnf => Dnf | Dnf => Cnf

fun bind xs f = List.concat (map f xs)

val rec cartesianProduct : 'a list list -> 'a list list =
 fn [] => [[]]
  | (xs :: xss) => bind (cartesianProduct xss)
                        (fn ys => bind xs (fn x => [x :: ys]))

(* Pushes all negation to the atoms.*)
fun pushNegate (negate : 'atom -> 'atom) (negating : bool) =
 fn Atom x => Atom (if negating then negate x else x)
  | Negate f => pushNegate negate (not negating) f
  | Combo (n, fs) => Combo (if negating then flipNf n else n, map (pushNegate negate negating) fs)

val rec flatten =
 fn Combo (n, fs) =>
    Combo (n, List.foldr (fn (f, acc) =>
                             case f of
                                 Combo (n', fs') => if n = n' then fs' @ acc else f :: acc
                               | _ => f :: acc)
                         []
                         (map flatten fs))
  | f => f

fun normalize' (negate : 'atom -> 'atom) (norm : normalForm) =
 fn Atom x => [[x]]
  | Negate f => map (map negate) (normalize' negate (flipNf norm) f)
  | Combo (n, fs) =>
    let
        val fss = bind fs (normalize' negate n)
    in
        if n = norm then fss else cartesianProduct fss
    end

fun normalize negate norm = normalize' negate norm o flatten o pushNegate negate false

fun mapFormulaSigned positive mf =
 fn Atom x => Atom (mf (positive, x))
  | Negate f => Negate (mapFormulaSigned (not positive) mf f)
  | Combo (n, fs) => Combo (n, map (mapFormulaSigned positive mf) fs)

fun mapFormula mf = mapFormulaSigned true (fn (_, x) => mf x)

(* SQL analysis. *)

val rec chooseTwos : 'a list -> ('a * 'a) list =
 fn [] => []
  | x :: ys => map (fn y => (x, y)) ys @ chooseTwos ys

datatype atomExp =
         QueryArg of int
       | DmlRel of int
       | Prim of Prim.t
       | Field of string * string

val equalAtomExp =
    let
        val isEqual = fn EQUAL => true | _ => false
    in
     fn (QueryArg n1, QueryArg n2) => n1 = n2
      | (DmlRel n1, DmlRel n2) => n1 = n2
      | (Prim p1, Prim p2) => isEqual (Prim.compare (p1, p2))
      | (Field (t1, f1), Field (t2, f2)) => isEqual (String.compare (t1 ^ "." ^ f1, t2 ^ "." ^ f2))
      | _ => false
    end

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
  | (Field (t1, f1), Field (t2, f2)) => String.compare (t1 ^ "." ^ f1, t2 ^ "." ^ f2)

end

structure UF = UnionFindFn(AtomExpKey)

(* val conflictMaps : (Sql.cmp * Sql.sqexp * Sql.sqexp) formula *)
(*                    * (Sql.cmp * Sql.sqexp * Sql.sqexp) formula *)
(*                    -> Mono.exp' IM.map list = *)
(*     let *)
        val toKnownEquality =
         (* [NONE] here means unkown. Anything that isn't a comparison between
            two knowns shouldn't be used, and simply dropping unused terms is
            okay in disjunctive normal form. *)
         fn (Sql.Eq, SOME e1, SOME e2) => SOME (e1, e2)
          | _ => NONE
        val equivClasses : (Sql.cmp * atomExp option * atomExp option) list -> atomExp list list =
            UF.classes
            o List.foldl UF.union' UF.empty
            o List.mapPartial toKnownEquality
        fun addToEqs (eqs, n, e) =
            case IM.find (eqs, n) of
                (* Comparing to a constant seems better? *)
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
          (* TODO: deal with equalities involving just [DmlRel]s and [Prim]s. *)
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
                  (* We can't deal with anything else. *)
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
           equivalence classes, so the [#1] can be anything. *)
        val mergeEqs : (atomExp IntBinaryMap.map option list
                        -> atomExp IntBinaryMap.map option) =
            List.foldr (fn (SOME eqs, SOME acc) => SOME (IM.unionWith #1 (eqs, acc)) | _ => NONE)
                       (SOME IM.empty)
        fun dnf (fQuery, fDml) =
            normalize negateCmp Dnf (Combo (Cnf, [markQuery fQuery, markDml fDml]))
    (* in *)
        val conflictMaps : (Sql.cmp * Sql.sqexp * Sql.sqexp) formula
                           * (Sql.cmp * Sql.sqexp * Sql.sqexp) formula
                           -> atomExp IM.map list =
            List.mapPartial (mergeEqs o map eqsOfClass o equivClasses) o dnf
    (* end *)

val rec sqexpToFormula =
 fn Sql.SqTrue => Combo (Cnf, [])
  | Sql.SqFalse => Combo (Dnf, [])
  | Sql.SqNot e => Negate (sqexpToFormula e)
  | Sql.Binop (Sql.RCmp c, e1, e2) => Atom (c, e1, e2)
  | Sql.Binop (Sql.RLop l, p1, p2) => Combo (case l of Sql.And => Cnf | Sql.Or => Dnf,
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
 fn Sql.Query1 {Where = NONE, ...} => Combo (Cnf, [])
  | Sql.Query1 {From = tablePairs, Where = SOME e, ...} =>
    renameTables tablePairs (sqexpToFormula e)
  | Sql.Union (q1, q2) => Combo (Dnf, [queryToFormula q1, queryToFormula q2])

fun valsToFormula (table, vals) =
    Combo (Cnf, map (fn (field, v) => Atom (Sql.Eq, Sql.Field (table, field), v)) vals)

val rec dmlToFormula =
 fn Sql.Insert tableVals => valsToFormula tableVals
  | Sql.Delete (table, wher) => renameTables [(table, "T")] (sqexpToFormula wher)
  (* TODO: refine formula for the vals part, which could take into account the wher part. *)
  | Sql.Update (table, vals, wher) =>
    let
        val f = sqexpToFormula wher
        fun update (positive, a) =
            let
                fun updateIfNecessary field =
                    case List.find (fn (f, _) => field = f) vals of
                        SOME (_, v) => (if positive then Sql.Eq else Sql.Ne,
                                        Sql.Field (table, field),
                                        v)
                      | NONE => a
            in
                case a of
                    (_, Sql.Field (_, field), _) => updateIfNecessary field
                  | (_, _, Sql.Field (_, field)) => updateIfNecessary field
                  | _ => a
            end
    in
        renameTables [(table, "T")]
                     (Combo (Dnf, [f,
                                   Combo (Cnf, [valsToFormula (table, vals),
                                                mapFormulaSigned true update f])]))
    end

val rec tablesQuery =
 fn Sql.Query1 {From = tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Sql.Union (q1, q2) => SS.union (tablesQuery q1, tablesQuery q2)

val tableDml =
 fn Sql.Insert (tab, _) => tab
  | Sql.Delete (tab, _) => tab
  | Sql.Update (tab, _, _) => tab


(* Program instrumentation. *)

fun stringExp s = (EPrim (Prim.String (Prim.Normal, s)), ErrorMsg.dummySpan)

val stringTyp = (TFfi ("Basis", "string"), ErrorMsg.dummySpan)

val sequence =
 fn (exp :: exps) =>
    let
        val loc = ErrorMsg.dummySpan
    in
        List.foldl (fn (e', seq) => ESeq ((seq, loc), (e', loc))) exp exps
    end
  | _ => raise Match

fun ffiAppCache' (func, index, args) : Mono.exp' =
    EFfiApp ("Sqlcache", func ^ Int.toString index, args)

fun ffiAppCache (func, index, args) : Mono.exp =
    (ffiAppCache' (func, index, args), ErrorMsg.dummySpan)

val varPrefix = "queryResult"

fun indexOfName varName =
    if String.isPrefix varPrefix varName
    then Int.fromString (String.extract (varName, String.size varPrefix, NONE))
    else NONE

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

(* Filled in by instrumentQuery during [Monoize], used during [Sqlcache]. *)
val urlifiedRel0s : Mono.exp IM.map ref = ref IM.empty

(* Used by [Monoize]. *)
val instrumentQuery =
    let
        val nextQuery = ref 0
        fun iq (query, urlifiedRel0) =
            case query of
                (EQuery {state = typ, ...}, loc) =>
                let
                    val i = !nextQuery before nextQuery := !nextQuery + 1
                in
                    urlifiedRel0s := IM.insert (!urlifiedRel0s, i, urlifiedRel0);
                    (ELet (varPrefix ^ Int.toString i, typ, query,
                           (* Uses a dummy FFI call to keep the urlified expression around, which
                              in turn keeps the declarations required for urlification safe from
                              [MonoShake]. The dummy call is removed during [Sqlcache]. *)
                           (* TODO: thread a [Monoize.Fm.t] through this module. *)
                           (ESeq ((EFfiApp ("Sqlcache",
                                            "dummy",
                                            [(urlifiedRel0, stringTyp)]),
                                   loc),
                                  (ERel 0, loc)),
                            loc)),
                     loc)
                end
              | _ => raise Match
    in
        iq
    end

fun cacheWrap (query, i, urlifiedRel0, args) =
    case query of
        (EQuery {state = typ, ...}, _) =>
        let
            val () = ffiInfo := {index = i, params = length args} :: !ffiInfo
            val loc = ErrorMsg.dummySpan
            (* We ensure before this step that all arguments aren't effectful.
               by turning them into local variables as needed. *)
            val argTyps = map (fn e => (e, stringTyp)) args
            val argTypsInc = map (fn (e, typ) => (incRels 1 e, typ)) argTyps
            val check = ffiAppCache ("check", i, argTyps)
            val store = ffiAppCache ("store", i, (urlifiedRel0, stringTyp) :: argTypsInc)
            val rel0 = (ERel 0, loc)
        in
            (ECase (check,
                    [((PNone stringTyp, loc),
                      (ELet ("q", typ, query, (ESeq (store, rel0), loc)), loc)),
                     ((PSome (stringTyp, (PVar ("hit", stringTyp), loc)), loc),
                      (* Boolean is false because we're not unurlifying from a cookie. *)
                      (EUnurlify (rel0, typ, false), loc))],
                    {disc = stringTyp, result = typ}),
             loc)
        end
      | _ => raise Match

fun fileMapfold doExp file start =
    case MonoUtil.File.mapfold {typ = Search.return2,
                                exp = fn x => (fn s => Search.Continue (doExp x s)),
                                decl = Search.return2} file start of
        Search.Continue x => x
      | Search.Return _ => raise Match

fun fileMap doExp file = #1 (fileMapfold (fn x => fn _ => (doExp x, ())) file ())

fun addChecking file =
    let
        fun doExp (queryInfo as (tableToIndices, indexToQuery)) =
         fn e' as ELet (v, t,
                        queryExp' as (EQuery {query = origQueryText,
                                              initial, body, state, tables, exps}, queryLoc),
                        letBody) =>
            let
                val loc = ErrorMsg.dummySpan
                val chunks = Sql.chunkify origQueryText
                fun strcat (e1, e2) = (EStrcat (e1, e2), loc)
                val (newQueryText, newVariables) =
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
                                    (* This is the (n + 1)th new variable, so
                                       there are already n new variables bound,
                                       so we increment indices by n. *)
                                    (strcat ((ERel (~(n+1)), loc), qText), incRels n e :: newVars)
                                end
                              | Sql.String s => (strcat (stringExp s, qText), newVars))
                        (stringExp "", [])
                        chunks
                fun wrapLets e' =
                    (* Important that this is foldl (to oppose foldr above). *)
                    List.foldl (fn (v, e') => ELet ("sqlArgz", stringTyp, v, (e', loc)))
                               e'
                               newVariables
                val numArgs = length newVariables
                (* Increment once for each new variable just made. *)
                val queryExp = incRels (length newVariables)
                                       (EQuery {query = newQueryText,
                                                initial = initial,
                                                body = body,
                                                state = state,
                                                tables = tables,
                                                exps = exps},
                                        queryLoc)
                val (EQuery {query = queryText, ...}, _) = queryExp
                val () = Print.preface ("sqlcache> ", (MonoPrint.p_exp MonoEnv.empty queryText));
                val args = List.tabulate (numArgs, fn n => (ERel n, loc))
                fun bind x f = Option.mapPartial f x
                fun guard b x = if b then x else NONE
                (* DEBUG: set first boolean argument to true to turn on printing. *)
                fun safe bound = not o effectful true (effectfulMap file) false bound
                val attempt =
                    (* Ziv misses Haskell's do notation.... *)
                    guard (safe 0 queryText andalso safe 0 initial andalso safe 2 body) (
                    bind (Sql.parse Sql.query queryText) (fn queryParsed =>
                    bind (indexOfName v) (fn index =>
                    bind (IM.find (!urlifiedRel0s, index)) (fn urlifiedRel0 =>
                    SOME (wrapLets (ELet (v, t,
                                          cacheWrap (queryExp, index, urlifiedRel0, args),
                                          incRelsBound 1 (length newVariables) letBody)),
                          (SS.foldr (fn (tab, qi) => SIMM.insert (qi, tab, index))
                                    tableToIndices
                                    (tablesQuery queryParsed),
                           IM.insert (indexToQuery, index, (queryParsed, numArgs))))))))
            in
                case attempt of
                    SOME pair => pair
                  | NONE => (e', queryInfo)
            end
          | ESeq ((EFfiApp ("Sqlcache", "dummy", _), _), (e', _)) => (e', queryInfo)
          | e' => (e', queryInfo)
    in
        fileMapfold (fn exp => fn state => doExp state exp) file (SIMM.empty, IM.empty)
    end

val gunk' : (((Sql.cmp * Sql.sqexp * Sql.sqexp) formula)
             * ((Sql.cmp * Sql.sqexp * Sql.sqexp) formula)) list ref = ref []

fun invalidations (nQueryArgs, query, dml) =
    let
        val loc = ErrorMsg.dummySpan
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
        fun eqsToInvalidation eqs =
            let
                fun inv n = if n < 0 then [] else IM.find (eqs, n) :: inv (n - 1)
            in
                inv (nQueryArgs - 1)
            end
        (* *)
        val rec madeRedundantBy : atomExp option list * atomExp option list -> bool =
         fn ([], []) => true
          | (NONE :: xs, _ :: ys) => madeRedundantBy (xs, ys)
          | (SOME x :: xs, SOME y :: ys) => equalAtomExp (x, y) andalso madeRedundantBy (xs, ys)
          | _ => false
        fun removeRedundant' (xss, yss) =
            case xss of
                [] => yss
              | xs :: xss' =>
                removeRedundant' (xss',
                                  if List.exists (fn ys => madeRedundantBy (xs, ys)) (xss' @ yss)
                                  then yss
                                  else xs :: yss)
        fun removeRedundant xss = removeRedundant' (xss, [])
        val eqss = conflictMaps (queryToFormula query, dmlToFormula dml)
    in
        gunk' := (queryToFormula query, dmlToFormula dml) :: !gunk';
        (map (map optionAtomExpToExp) o removeRedundant o map eqsToInvalidation) eqss
    end

val gunk : Mono.exp list list list ref = ref []

fun addFlushing (file, queryInfo as (tableToIndices, indexToQuery)) =
    let
        val allIndices = SM.foldr (fn (x, acc) => IS.listItems x @ acc) [] tableToIndices
        val flushes = map (fn i => ffiAppCache' ("flush", i, []))
        val doExp =
         fn dmlExp as EDml (dmlText, _) =>
            let
                val indices =
                    case Sql.parse Sql.dml dmlText of
                        SOME dmlParsed =>
                        map (fn i => ((case IM.find (indexToQuery, i) of
                                           NONE => ()
                                         | SOME (queryParsed, numArgs) =>
                                           gunk := invalidations (numArgs, queryParsed, dmlParsed) :: !gunk);
                                      i)) (SIMM.findList (tableToIndices, tableDml dmlParsed))
                      | NONE => allIndices
            in
                sequence (flushes indices @ [dmlExp])
            end
          | e' => e'
    in
        fileMap doExp file
    end

fun go file =
    let
        val () = Sql.sqlcacheMode := true
        val file' = addFlushing (addChecking file)
        val () = Sql.sqlcacheMode := false
    in
         file'
    end

end
