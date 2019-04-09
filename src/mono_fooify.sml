structure MonoFooify :> MONO_FOOIFY = struct

open Mono

datatype foo_kind =
         Attr
       | Url

val nextPvar = ref 0
val pvarDefs = ref ([] : (string * int * (string * int * typ option) list) list)

structure Fm = struct

type vr = string * int * typ * exp * string

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

end

fun fk2s fk =
    case fk of
        Attr => "attr"
      | Url => "url"

fun capitalize s =
    if s = "" then
        s
    else
        str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

structure E = ErrorMsg

exception TypeMismatch of Fm.t * E.span
exception CantPass of Fm.t * typ
exception DontKnow of Fm.t * typ

val dummyExp = (EPrim (Prim.Int 0), E.dummySpan)

fun fooifyExpWithExceptions fk lookupENamed lookupDatatype =
    let
        fun fooify fm (e, tAll as (t, loc)) =
            case #1 e of
                EClosure (fnam, [(ERecord [], _)]) =>
                let
                    val (_, s) = lookupENamed fnam
                in
                    ((EPrim (Prim.String (Prim.Normal, Settings.getUrlPrefix () ^ s)), loc), fm)
                end
              | EClosure (fnam, args) =>
                let
                    val (ft, s) = lookupENamed fnam
                    fun attrify (args, ft, e, fm) =
                        case (args, ft) of
                            ([], _) => (e, fm)
                          | (arg :: args, (TFun (t, ft), _)) =>
                            let
                                val (arg', fm) = fooify fm (arg, t)
                            in
                                attrify (args, ft,
                                         (EStrcat (e,
                                                      (EStrcat ((EPrim (Prim.String (Prim.Normal, "/")), loc),
                                                                   arg'), loc)), loc),
                                         fm)
                            end
                          | _ => raise TypeMismatch (fm, loc)
                in
                    attrify (args, ft, (EPrim (Prim.String (Prim.Normal, Settings.getUrlPrefix () ^ s)), loc), fm)
                end
              | _ =>
                case t of
                    TFfi ("Basis", "unit") => ((EPrim (Prim.String (Prim.Normal, "_")), loc), fm)
                  | TFfi (m, x) => (if Settings.mayClientToServer (m, x)
                                    then ((EFfiApp (m, fk2s fk ^ "ify" ^ capitalize x, [(e, tAll)]), loc), fm)
                                    else raise CantPass (fm, tAll))

                  | TRecord [] => ((EPrim (Prim.String (Prim.Normal, "_")), loc), fm)
                  | TRecord ((x, t) :: xts) =>
                    let
                        val (se, fm) = fooify fm ((EField (e, x), loc), t)
                    in
                        foldl (fn ((x, t), (se, fm)) =>
                                  let
                                      val (se', fm) = fooify fm ((EField (e, x), loc), t)
                                  in
                                      ((EStrcat (se,
                                                    (EStrcat ((EPrim (Prim.String (Prim.Normal, "/")), loc),
                                                                 se'), loc)), loc),
                                       fm)
                                  end) (se, fm) xts
                    end

                  | TDatatype (i, ref (dk, _)) =>
                    let
                        fun makeDecl n fm =
                            let
                                val (x, xncs) =
                                    case ListUtil.search (fn (x, i', xncs) =>
                                                             if i' = i then
                                                                 SOME (x, xncs)
                                                             else
                                                                 NONE) (!pvarDefs) of
                                        NONE => lookupDatatype i
                                      | SOME v => v

                                val (branches, fm) =
                                    ListUtil.foldlMap
                                        (fn ((x, n, to), fm) =>
                                            case to of
                                                NONE =>
                                                (((PCon (dk, PConVar n, NONE), loc),
                                                  (EPrim (Prim.String (Prim.Normal, x)), loc)),
                                                 fm)
                                              | SOME t =>
                                                let
                                                    val (arg, fm) = fooify fm ((ERel 0, loc), t)
                                                in
                                                    (((PCon (dk, PConVar n, SOME (PVar ("a", t), loc)), loc),
                                                      (EStrcat ((EPrim (Prim.String (Prim.Normal, x ^ "/")), loc),
                                                                   arg), loc)),
                                                     fm)
                                                end)
                                        fm xncs

                                val dom = tAll
                                val ran = (TFfi ("Basis", "string"), loc)
                            in
                                ((fk2s fk ^ "ify_" ^ x,
                                  n,
                                  (TFun (dom, ran), loc),
                                  (EAbs ("x",
                                            dom,
                                            ran,
                                            (ECase ((ERel 0, loc),
                                                       branches,
                                                       {disc = dom,
                                                        result = ran}), loc)), loc),
                                  ""),
                                 fm)
                            end

                        val (fm, n) = Fm.lookup fm fk i makeDecl
                    in
                        ((EApp ((ENamed n, loc), e), loc), fm)
                    end

                  | TOption t =>
                    let
                        val (body, fm) = fooify fm ((ERel 0, loc), t)
                    in
                        ((ECase (e,
                                    [((PNone t, loc),
                                      (EPrim (Prim.String (Prim.Normal, "None")), loc)),

                                     ((PSome (t, (PVar ("x", t), loc)), loc),
                                      (EStrcat ((EPrim (Prim.String (Prim.Normal, "Some/")), loc),
                                                   body), loc))],
                                    {disc = tAll,
                                     result = (TFfi ("Basis", "string"), loc)}), loc),
                         fm)
                    end

                  | TList t =>
                    let
                        fun makeDecl n fm =
                            let
                                val rt = (TRecord [("1", t), ("2", (TList t, loc))], loc)
                                val (arg, fm) = fooify fm ((ERel 0, loc), rt)

                                val branches = [((PNone rt, loc),
                                                 (EPrim (Prim.String (Prim.Normal, "Nil")), loc)),
                                                ((PSome (rt, (PVar ("a", rt), loc)), loc),
                                                 (EStrcat ((EPrim (Prim.String (Prim.Normal, "Cons/")), loc),
                                                              arg), loc))]

                                val dom = tAll
                                val ran = (TFfi ("Basis", "string"), loc)
                            in
                                ((fk2s fk ^ "ify_list",
                                  n,
                                  (TFun (dom, ran), loc),
                                  (EAbs ("x",
                                            dom,
                                            ran,
                                            (ECase ((ERel 0, loc),
                                                       branches,
                                                       {disc = dom,
                                                        result = ran}), loc)), loc),
                                  ""),
                                 fm)
                            end

                        val (fm, n) = Fm.lookupList fm fk t makeDecl
                    in
                        ((EApp ((ENamed n, loc), e), loc), fm)
                    end

                  | _ => raise DontKnow (fm, tAll)
    in
        fooify
    end

fun fooifyExp fk lookupENamed lookupDatatype fm exp =
    fooifyExpWithExceptions fk lookupENamed lookupDatatype fm exp
    handle TypeMismatch (fm, loc) =>
           (E.errorAt loc "Type mismatch encoding attribute";
            (dummyExp, fm))
         | CantPass (fm, typ as (_, loc)) =>
           (E.errorAt loc "MonoFooify: can't pass type from client to server";
            Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty typ)];
            (dummyExp, fm))
         | DontKnow (fm, typ as (_, loc)) =>
           (E.errorAt loc "Don't know how to encode attribute/URL type";
            Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty typ)];
            (dummyExp, fm))

(* Has to be set at the end of [Monoize]. *)
val canonicalFm = ref (Fm.empty 0 : Fm.t)

fun urlify env expTyp =
    let
        val (exp, fm) =
            fooifyExpWithExceptions
                Url
                (fn n =>
                    let
                        val (_, t, _, s) = MonoEnv.lookupENamed env n
                    in
                        (t, s)
                    end)
                (fn n => MonoEnv.lookupDatatype env n)
                (!canonicalFm)
                expTyp
    in
        canonicalFm := fm;
        SOME exp
    end
    handle TypeMismatch _ => NONE
         | CantPass _ => NONE
         | DontKnow _ => NONE

fun getNewFmDecls () =
    let
        val fm = !canonicalFm
    in
        canonicalFm := Fm.enter fm;
        Fm.decls fm
    end

end
