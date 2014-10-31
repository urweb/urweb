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

(* Some FFIs have writing as their only effect, which the caching records. *)
val ffiEffectful =
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

(* Makes an exception for EWrite (which is recorded when caching). *)
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
          (* TODO: use FFI whitelist. *)
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


(* SQL analysis. *)

val useInjIfPossible =
 fn SqConst prim => Inj (EPrim (Prim.String (Prim.Normal, Prim.toString prim)),
                         ErrorMsg.dummySpan)
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
                    (ELet (varPrefix ^ Int.toString i, typ, query,
                           (* Uses a dummy FFI call to keep the urlified expression around, which
                              in turn keeps the declarations required for urlification safe from
                              MonoShake. The dummy call is removed during Sqlcache. *)
                           (* TODO: thread a Monoize.Fm.t through this module. *)
                           (ESeq ((EFfiApp ("Sqlcache", "dummy", [(urlifiedRel0, stringTyp)]), loc),
                                  (ERel 0, loc)),
                            loc)),
                     loc)
                end
              | _ => raise Match
    in
        iq
    end

fun cacheWrap (query, i, urlifiedRel0, eqs) =
    case query of
        (EQuery {state = typ, ...}, _) =>
        let
            val () = ffiInfo := {index = i, params = length eqs} :: !ffiInfo
            val loc = ErrorMsg.dummySpan
            (* We ensure before this step that all arguments aren't effectful.
               by turning them into local variables as needed. *)
            val args = map (fn (_, e) => (e, stringTyp)) eqs
            val argsInc = map (fn (e, typ) => (incRels 1 e, typ)) args
            val check = ffiAppCache ("check", i, args)
            val store = ffiAppCache ("store", i, (urlifiedRel0, stringTyp) :: argsInc)
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
        fun doExp queryInfo =
         fn e' as ELet (v, t,
                        queryExp' as (EQuery {query = origQueryText,
                                              initial, body, state, tables, exps}, queryLoc),
                        letBody) =>
            let
                val loc = ErrorMsg.dummySpan
                val chunks = chunkify origQueryText
                fun strcat (e1, e2) = (EStrcat (e1, e2), loc)
                val (newQueryText, newVariables) =
                    (* Important that this is foldr (to oppose foldl below). *)
                    List.foldr
                        (fn (chunk, (qText, newVars)) =>
                            case chunk of
                                Exp (e as (EPrim _, _)) => (strcat (e, qText), newVars)
                              | Exp (e as (ERel _, _)) => (strcat (e, qText), newVars)
                              | Exp (e as (ENamed _, _)) => (strcat (e, qText), newVars)
                              (* Head of newVars has lowest index. *)
                              | Exp e =>
                                let
                                    val n = length newVars
                                in
                                    (* This is the (n + 1)th new variable, so
                                       there are already n new variables bound,
                                       so we increment indices by n. *)
                                    (strcat ((ERel (~(n+1)), loc), qText), incRels n e :: newVars)
                                end
                              | String s => (strcat (stringExp s, qText), newVars))
                        (stringExp "", [])
                        chunks
                fun wrapLets e' =
                    (* Important that this is foldl (to oppose foldr above). *)
                    List.foldl (fn (v, e') => ELet ("sqlArgz", stringTyp, v, (e', loc))) e' newVariables
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
                (* val () = Print.preface ("sqlcache> ", (MonoPrint.p_exp MonoEnv.empty queryText)); *)
                fun bind x f = Option.mapPartial f x
                fun guard b x = if b then x else NONE
                (* DEBUG: set first boolean argument to true to turn on printing. *)
                fun safe bound = not o effectful true (effectfulMap file) false bound
                val attempt =
                    (* Ziv misses Haskell's do notation.... *)
                    guard (safe 0 queryText andalso safe 0 initial andalso safe 2 body) (
                    bind (parse query queryText) (fn queryParsed =>
                    bind (indexOfName v) (fn i =>
                    bind (equalitiesQuery queryParsed) (fn eqs =>
                    bind (IM.find (!urlifiedRel0s, i)) (fn urlifiedRel0 =>
                    SOME (wrapLets (ELet (v, t,
                                          cacheWrap (queryExp, i, urlifiedRel0, eqs),
                                          incRelsBound 1 (length newVariables) letBody)),
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
        fileMapfold (fn exp => fn state => doExp state exp) file SIMM.empty
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
        val file' = addFlushing (addChecking file)
        val () = Sql.sqlcacheMode := false
    in
         file'
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
