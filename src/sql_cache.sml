structure SqlCache = struct

open Sql
open Mono

structure IS = IntBinarySet
structure IM = IntBinaryMap
structure StringKey = struct type ord_key = string val compare = String.compare end
structure SS = BinarySetFn (StringKey)
structure SM = BinaryMapFn (StringKey)
structure SIMM = MultimapFn (structure KeyMap = SM structure ValSet = IS)

val ffiIndices : int list ref = ref []
val rs : int list ref = ref []
val ws : int list ref = ref []

val rec tablesRead =
 fn Query1 {From=tablePairs, ...} => SS.fromList (map #1 tablePairs)
  | Union (q1,q2) => SS.union (tablesRead q1, tablesRead q2)

val tableWritten =
 fn Insert (tab, _) => tab
  | Delete (tab, _) => tab
  | Update (tab, _, _) => tab

fun tablesInExp' exp' =
    let
        val nothing = {read = SS.empty, written = SS.empty}
    in
        case exp' of
            EQuery {query=e, ...} =>
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
            let val {read = r, written = w} = tablesInExp' exp'
            in {read = SS.union (r, read), written = SS.union (w, written)} end
    in
        MonoUtil.Exp.fold {typ = #2, exp = addTables}
                          {read = SS.empty, written = SS.empty}
    end

fun intExp (n, loc) = (EPrim (Prim.Int (Int64.fromInt n)), loc)
fun intTyp loc = (TFfi ("Basis", "int"), loc)
fun boolPat (b, loc) = (PCon (Enum,
                              PConFfi {mod = "Basis", datatyp = "bool", arg = NONE,
                                       con = if b then "True" else "False"},
                              NONE),
                        loc)
fun boolTyp loc = (TFfi ("Basis", "int"), loc)

fun ffiAppExp (module, func, index, loc) =
    (EFfiApp (module, func ^ Int.toString index, []), loc)

fun sequence (befores, center, afters, loc) =
    List.foldr (fn (exp, seq) => (ESeq (exp, seq), loc))
               (List.foldl (fn (exp, seq) => (ESeq (seq, exp), loc))
                           center
                           afters)
               befores

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

fun addCacheCheck (index, exp) =
    let
        fun f (body as (_, loc)) =
            let
                val check = ffiAppExp ("Cache", "check", index, loc)
                val store = ffiAppExp ("Cache", "store", index, loc)
            in
                antiguardUnit (check, sequence ([], body, [store], loc), loc)
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
                sequence (mapFfi "flush" flushes, body, mapFfi "ready" flushes, loc)
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
          | _ => (file, init) (* Should never happen. *)
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
        rs := IS.listItems readers;
        ws := IS.listItems writers;
        ffiIndices := IS.listItems readers;
        addCacheFlushing (fileWithChecks, tablesToIndices, writers)
    end

end
