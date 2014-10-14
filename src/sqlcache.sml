structure Sqlcache (* :> SQLCACHE *) = struct

open Sql
open Mono

structure IS = IntBinarySet
structure IM = IntBinaryMap
structure SK = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)
structure SIMM = MultimapFn(structure KeyMap = SM structure ValSet = IS)

(* Filled in by cacheWrap during Sqlcache. *)
val ffiInfo : {index : int, params : int} list ref = ref []

fun getFfiInfo () = !ffiInfo

(* Program analysis. *)

val useInjIfPossible =
 fn SqConst prim => Inj (EPrim (Prim.String (Prim.Normal, Prim.toString prim)), ErrorMsg.dummySpan)
  | sqexp => sqexp

fun equalities (canonicalTable : string -> string) :
    sqexp -> ((string * string) * Mono.exp) list option =
    let
        val rec eqs =
         fn Binop (Exps f, e1, e2) =>
            (* TODO: use a custom datatype in Exps instead of a function. *)
            (case f (Var 1, Var 2) of
                 Reln (Eq, [Var 1, Var 2]) =>
                 let
                     val (e1', e2') = (useInjIfPossible e1, useInjIfPossible e2)
                 in
                     case (e1', e2') of
                         (Field (t, f), Inj i) => SOME [((canonicalTable t, f), i)]
                       | (Inj i, Field (t, f)) => SOME [((canonicalTable t, f), i)]
                       | _ => NONE
                 end
               | _ => NONE)
          | Binop (Props f, e1, e2) =>
            (* TODO: use a custom datatype in Props instead of a function. *)
            (case f (True, False) of
                 And (True, False) =>
                 (case (eqs e1, eqs e2) of
                      (SOME eqs1, SOME eqs2) => SOME (eqs1 @ eqs2)
                    | _ => NONE)
               | _ => NONE)
          | _ => NONE
    in
        eqs
    end

val equalitiesQuery =
 fn Query1 {From = tablePairs, Where = SOME exp, ...} =>
    equalities
        (* If we have [SELECT ... FROM T AS T' ...], use T, not T'. *)
        (fn t =>
            case List.find (fn (_, tAs) => t = tAs) tablePairs of
                NONE => t
              | SOME (tOrig, _) => tOrig)
        exp
  | Query1 {Where = NONE, ...} => SOME []
  | _ => NONE

val equalitiesDml =
 fn Insert (tab, eqs) => SOME (List.mapPartial
                                   (fn (name, sqexp) =>
                                       case useInjIfPossible sqexp of
                                           Inj e => SOME ((tab, name), e)
                                         | _ => NONE)
                                   eqs)
  | Delete (tab, exp) => equalities (fn _ => tab) exp
  (* TODO: examine the updated values and not just the way they're filtered. *)
  (* For example, UPDATE foo SET Id = 9001 WHERE Id = 42 should update both the
     Id = 42 and Id = 9001 cache entries. Could also think of it as doing a
     Delete immediately followed by an Insert. *)
  | Update (tab, _, exp) => equalities (fn _ => tab) exp

val rec tablesQuery =
 fn Query1 {From = tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Union (q1, q2) => SS.union (tablesQuery q1, tablesQuery q2)

val tableDml =
 fn Insert (tab, _) => tab
  | Delete (tab, _) => tab
  | Update (tab, _, _) => tab


(* Program instrumentation. *)

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

fun ffiAppCache (func, index, args) : Mono. exp =
    (ffiAppCache' (func, index, args), ErrorMsg.dummySpan)

val varPrefix = "queryResult"

fun indexOfName varName =
    if String.isPrefix varPrefix varName
    then Int.fromString (String.extract (varName, String.size varPrefix, NONE))
    else NONE

val incRels = MonoUtil.Exp.map {typ = fn x => x, exp = fn ERel n => ERel (n + 1) | x => x}

(* Filled in by instrumentQuery during Monoize, used during Sqlcache. *)
val urlifiedRel0s : Mono.exp IM.map ref = ref IM.empty

(* Used by Monoize. *)
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
                    (* ASK: name variables properly? *)
                    (ELet (varPrefix ^ Int.toString i, typ, query,
                           (* Uses a dummy FFI call to keep the urlified expression around, which
                              in turn keeps the declarations required for urlification safe from
                              MonoShake. The dummy call is removed during Sqlcache. *)
                           (* ASK: is there a better way? *)
                           (ESeq ((EFfiApp ("Sqlcache", "dummy", [(urlifiedRel0, stringTyp)]), loc),
                                  (ERel 0, loc)),
                            loc)),
                     loc)
                end
              | _ => raise Match
    in
        iq
    end

val gunk : ((string * string) * Mono.exp) list list ref = ref [[]]

fun cacheWrap (query, i, urlifiedRel0, eqs) =
    case query of
        (EQuery {state = typ, ...}, _) =>
        let
            val loc = ErrorMsg.dummySpan
            (* TODO: deal with effectful injected expressions. *)
            val args = (ffiInfo := {index = i, params = length eqs} :: !ffiInfo;
                        map (fn (_, e) => (e, stringTyp)) eqs) before gunk := eqs :: !gunk
            val argsInc = map (fn (e, t) => (incRels e, t)) args
        in
            (ECase (ffiAppCache ("check", i, args),
                    [((PNone stringTyp, loc),
                      (ELet ("q", typ, query,
                             (ESeq (ffiAppCache ("store", i, (urlifiedRel0, stringTyp) :: argsInc),
                                    (ERel 0, loc)),
                              loc)),
                       loc)),
                     ((PSome (stringTyp, (PVar ("hit", stringTyp), loc)), loc),
                      (* ASK: what does this bool do? *)
                      (EUnurlify ((ERel 0, loc), typ, false), loc))],
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

val addChecking =
    let
        fun doExp queryInfo =
         fn e' as ELet (v, t, queryExp as (EQuery {query = queryText, ...}, _), body) =>
            let
                fun bind x f = Option.mapPartial f x
                val attempt =
                    (* Ziv misses Haskell's do notation.... *)
                    bind (parse query queryText) (fn queryParsed =>
                    (Print.preface ("gunk> ", (MonoPrint.p_exp MonoEnv.empty queryExp));
                    bind (indexOfName v) (fn i =>
                    bind (equalitiesQuery queryParsed) (fn eqs =>
                    bind (IM.find (!urlifiedRel0s, i)) (fn urlifiedRel0 =>
                    SOME (ELet (v, t, cacheWrap (queryExp, i, urlifiedRel0, eqs), body),
                          SS.foldr (fn (tab, qi) => SIMM.insert (qi, tab, i))
                                   queryInfo
                                   (tablesQuery queryParsed)))))))
            in
                case attempt of
                    SOME pair => pair
                  | NONE => (e', queryInfo)
            end
          | ESeq ((EFfiApp ("Sqlcache", "dummy", _), _), (e', _)) => (e', queryInfo)
          | e' => (e', queryInfo)
    in
        fn file => fileMapfold (fn exp => fn state => doExp state exp) file SIMM.empty
    end

fun addFlushing (file, queryInfo) =
    let
        val allIndices : int list = SM.foldr (fn (x, acc) => IS.listItems x @ acc) [] queryInfo
        fun flushes indices = map (fn i => ffiAppCache' ("flush", i, [])) indices
        val doExp =
         fn dmlExp as EDml (dmlText, _) =>
            let
                val indices =
                    case parse dml dmlText of
                        SOME dmlParsed => SIMM.findList (queryInfo, tableDml dmlParsed)
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
    in
        addFlushing (addChecking file) before Sql.sqlcacheMode := false
    end


(* BEGIN OLD

fun intExp (n, loc) = (EPrim (Prim.Int (Int64.fromInt n)), loc)
fun intTyp loc = (TFfi ("Basis", "int"), loc)
fun stringExp (s, loc) = (EPrim (Prim.String (Prim.Normal, s)), loc)

fun boolPat (b, loc) = (PCon (Enum,
                              PConFfi {mod = "Basis", datatyp = "bool", arg = NONE,
                                       con = if b then "True" else "False"},
                              NONE),
                        loc)
fun boolTyp loc = (TFfi ("Basis", "int"), loc)

fun ffiAppExp (module, func, index, args, loc) =
    (EFfiApp (module, func ^ Int.toString index, args), loc)

val sequence =
 fn ((exp :: exps), loc) =>
    List.foldl (fn (exp, seq) => (ESeq (seq, exp), loc)) exp exps
  | _ => raise Match

fun antiguardUnit (cond, exp, loc) =
    (ECase (cond,
            [(boolPat (false, loc), exp),
             (boolPat (true, loc), (ERecord [], loc))],
            {disc = boolTyp loc, result = (TRecord [], loc)}),
     loc)

fun underAbs f (exp as (exp', loc)) =
    case exp' of
        EAbs (x, y, z, body) => (EAbs (x, y, z, underAbs f body), loc)
      | _ => f exp


val rec tablesRead =
 fn Query1 {From = tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Union (q1, q2) => SS.union (tablesRead q1, tablesRead q2)

val tableWritten =
 fn Insert (tab, _) => tab
  | Delete (tab, _) => tab
  | Update (tab, _, _) => tab

fun tablesInExp' exp' =
    let
        val nothing = {read = SS.empty, written = SS.empty}
    in
        case exp' of
            EQuery {query = e, ...} =>
            (case parse query e of
                 SOME q => {read = tablesRead q, written = SS.empty}
               | NONE => nothing)
          | EDml (e, _) =>
            (case parse dml e of
                 SOME q => {read = SS.empty, written = SS.singleton (tableWritten q)}
               | NONE => nothing)
          | _ => nothing
    end

val tablesInExp =
    let
        fun addTables (exp', {read, written}) =
            let
                val {read = r, written = w} = tablesInExp' exp'
            in
                {read = SS.union (r, read), written = SS.union (w, written)}
            end
    in
        MonoUtil.Exp.fold {typ = #2, exp = addTables}
                          {read = SS.empty, written = SS.empty}
    end

fun addCacheCheck (index, exp) =
    let
        fun f (body as (_, loc)) =
            let
                val check = ffiAppExp ("Cache", "check", index, loc)
                val store = ffiAppExp ("Cache", "store", index, loc)
            in
                antiguardUnit (check, sequence ([body, store], loc), loc)
            end
    in
        underAbs f exp
    end

fun addCacheFlush (exp, tablesToIndices) =
    let
        fun addIndices (table, indices) = IS.union (indices, SIMM.find (tablesToIndices, table))
        fun f (body as (_, loc)) =
            let
                fun mapFfi func = List.map (fn i => ffiAppExp ("Cache", func, i, loc))
                val flushes =
                    IS.listItems (SS.foldr addIndices IS.empty (#written (tablesInExp body)))
            in
                sequence (mapFfi "flush" flushes @ [body] @ mapFfi "ready" flushes, loc)
            end
    in
        underAbs f exp
    end

val handlerIndices =
    let
        val isUnit =
         fn (TRecord [], _) => true
          | _ => false
        fun maybeAdd (d, soFar as {readers, writers}) =
            case d of
                DExport (Link ReadOnly, _, name, typs, typ, _) =>
                if List.all isUnit (typ::typs)
                then {readers = IS.add (readers, name), writers = writers}
                else soFar
              | DExport (_, _, name, _, _, _) => (* Not read only. *)
                {readers = readers, writers = IS.add (writers, name)}
              | _ => soFar
    in
        MonoUtil.File.fold {typ = #2, exp = #2, decl = maybeAdd}
                           {readers = IS.empty, writers = IS.empty}
    end

fun fileFoldMapiSelected f init (file, indices) =
    let
        fun doExp (original as ((a, index, b, exp, c), state)) =
            if IS.member (indices, index)
            then let val (newExp, newState) = f (index, exp, state)
                 in ((a, index, b, newExp, c), newState) end
            else original
        fun doDecl decl state =
            let
                val result =
                    case decl of
                        DVal x =>
                        let val (y, newState) = doExp (x, state)
                        in (DVal y, newState) end
                      | DValRec xs =>
                        let val (ys, newState) = ListUtil.foldlMap doExp state xs
                        in (DValRec ys, newState) end
                      | _ => (decl, state)
            in
                Search.Continue result
            end
        fun nada x y = Search.Continue (x, y)
    in
        case MonoUtil.File.mapfold {typ = nada, exp = nada, decl = doDecl} file init of
            Search.Continue x => x
          | _ => raise Match (* Should never happen. *)
    end

fun fileMapSelected f = #1 o fileFoldMapiSelected (fn (_, x, _) => (f x, ())) ()

val addCacheChecking =
    let
        fun f (index, exp, tablesToIndices) =
            (addCacheCheck (index, exp),
             SS.foldr (fn (table, tsToIs) => SIMM.insert (tsToIs, table, index))
                      tablesToIndices
                      (#read (tablesInExp exp)))
    in
        fileFoldMapiSelected f (SM.empty)
    end

fun addCacheFlushing (file, tablesToIndices, writers) =
    fileMapSelected (fn exp => addCacheFlush (exp, tablesToIndices)) (file, writers)

fun go file =
    let
        val {readers, writers} = handlerIndices file
        val (fileWithChecks, tablesToIndices) = addCacheChecking (file, readers)
    in
        ffiIndices := IS.listItems readers;
        addCacheFlushing (fileWithChecks, tablesToIndices, writers)
    end

END OLD *)

end
