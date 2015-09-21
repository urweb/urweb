(* TODO: better name than "fm"? *)
structure MonoFm : MONO_FM = struct

open Mono

type vr = string * int * typ * exp * string

datatype foo_kind =
         Attr
       | Url

structure IM = IntBinaryMap

structure M = BinaryMapFn(struct
                          type ord_key = foo_kind
                          fun compare x =
                              case x of
                                  (Attr, Attr) => EQUAL
                                | (Attr, _) => LESS
                                | (_, Attr) => GREATER

                                | (Url, Url) => EQUAL
                          end)

structure TM = BinaryMapFn(struct
                           type ord_key = typ
                           val compare = MonoUtil.Typ.compare
                           end)

type t = {
     count : int,
     map : int IM.map M.map,
     listMap : int TM.map M.map,
     decls : vr list
}

val nextPvar = ref 0

fun empty count = {
    count = count,
    map = M.empty,
    listMap = M.empty,
    decls = []
}

fun chooseNext count =
    let
        val n = !nextPvar
    in
        if count < n then
            (count, count+1)
        else
            (nextPvar := n + 1;
             (n, n+1))
    end

fun enter ({count, map, listMap, ...} : t) = {count = count, map = map, listMap = listMap, decls = []}
fun freshName {count, map, listMap, decls} =
    let
        val (next, count) = chooseNext count
    in
        (next, {count = count , map = map, listMap = listMap, decls = decls})
    end
fun decls ({decls, ...} : t) =
    case decls of
        [] => []
      | _ => [(DValRec decls, ErrorMsg.dummySpan)]

fun lookup (t as {count, map, listMap, decls}) k n thunk =
    let
        val im = Option.getOpt (M.find (map, k), IM.empty)
    in
        case IM.find (im, n) of
            NONE =>
            let
                val n' = count
                val (d, {count, map, listMap, decls}) =
                    thunk count {count = count + 1,
                                 map = M.insert (map, k, IM.insert (im, n, n')),
                                 listMap = listMap,
                                 decls = decls}
            in
                ({count = count,
                  map = map,
                  listMap = listMap,
                  decls = d :: decls}, n')
            end
          | SOME n' => (t, n')
    end

fun lookupList (t as {count, map, listMap, decls}) k tp thunk =
    let
        val tm = Option.getOpt (M.find (listMap, k), TM.empty)
    in
        case TM.find (tm, tp) of
            NONE =>
            let
                val n' = count
                val (d, {count, map, listMap, decls}) =
                    thunk count {count = count + 1,
                                 map = map,
                                 listMap = M.insert (listMap, k, TM.insert (tm, tp, n')),
                                 decls = decls}
            in
                ({count = count,
                  map = map,
                  listMap = listMap,
                  decls = d :: decls}, n')
            end
          | SOME n' => (t, n')
    end

val postMonoize : t ref = ref (empty 0)

end
