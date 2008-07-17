(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

structure Corify :> CORIFY = struct

structure EM = ErrorMsg
structure L = Expl
structure L' = Core

structure IM = IntBinaryMap
structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

local
    val count = ref 0
in

fun reset v = count := v

fun alloc () =
    let
        val r = !count
    in
        count := r + 1;
        r
end

end

structure St : sig
    type t

    val empty : t

    val enter : t -> t
    val leave : t -> {outer : t, inner : t}
    val ffi : string -> L'.con SM.map -> t

    datatype core_con =
             CNormal of int
           | CFfi of string
    val bindCon : t -> string -> int -> t * int
    val lookupConById : t -> int -> int option
    val lookupConByName : t -> string -> core_con

    datatype core_val =
             ENormal of int
           | EFfi of string * L'.con
    val bindVal : t -> string -> int -> t * int
    val lookupValById : t -> int -> int option
    val lookupValByName : t -> string -> core_val

    val bindStr : t -> string -> int -> t -> t
    val lookupStrById : t -> int -> t
    val lookupStrByName : string * t -> t

    val bindFunctor : t -> string -> int -> int -> L.str -> t
    val lookupFunctorById : t -> int -> int * L.str
    val lookupFunctorByName : string * t -> int * L.str
end = struct

datatype flattening =
         FNormal of {cons : int SM.map,
                     vals : int SM.map,
                     strs : flattening SM.map,
                     funs : (int * L.str) SM.map}
       | FFfi of {mod : string,
                  vals : L'.con SM.map}
                           
type t = {
     cons : int IM.map,
     vals : int IM.map,
     strs : flattening IM.map,
     funs : (int * L.str) IM.map,
     current : flattening,
     nested : flattening list
}

val empty = {
    cons = IM.empty,
    vals = IM.empty,
    strs = IM.empty,
    funs = IM.empty,
    current = FNormal { cons = SM.empty, vals = SM.empty, strs = SM.empty, funs = SM.empty },
    nested = []
}

datatype core_con =
         CNormal of int
       | CFfi of string

datatype core_val =
         ENormal of int
       | EFfi of string * L'.con

fun bindCon {cons, vals, strs, funs, current, nested} s n =
    let
        val n' = alloc ()

        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {cons, vals, strs, funs} =>
                FNormal {cons = SM.insert (cons, s, n'),
                         vals = vals,
                         strs = strs,
                         funs = funs}
    in
        ({cons = IM.insert (cons, n, n'),
          vals = vals,
          strs = strs,
          funs = funs,
          current = current,
          nested = nested},
         n')
    end

fun lookupConById ({cons, ...} : t) n = IM.find (cons, n)

fun lookupConByName ({current, ...} : t) x =
    case current of
        FFfi {mod = m, ...} => CFfi m
      | FNormal {cons, ...} =>
        case SM.find (cons, x) of
            NONE => raise Fail "Corify.St.lookupConByName"
          | SOME n => CNormal n

fun bindVal {cons, vals, strs, funs, current, nested} s n =
    let
        val n' = alloc ()

        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {cons, vals, strs, funs} =>
                FNormal {cons = cons,
                         vals = SM.insert (vals, s, n'),
                         strs = strs,
                         funs = funs}
    in
        ({cons = cons,
          vals = IM.insert (vals, n, n'),
          strs = strs,
          funs = funs,
          current = current,
          nested = nested},
         n')
    end

fun lookupValById ({vals, ...} : t) n = IM.find (vals, n)

fun lookupValByName ({current, ...} : t) x =
    case current of
        FFfi {mod = m, vals, ...} =>
        (case SM.find (vals, x) of
             NONE => raise Fail "Corify.St.lookupValByName: no type for FFI val"
           | SOME t => EFfi (m, t))
      | FNormal {vals, ...} =>
        case SM.find (vals, x) of
            NONE => raise Fail "Corify.St.lookupValByName"
          | SOME n => ENormal n

fun enter {cons, vals, strs, funs, current, nested} =
    {cons = cons,
     vals = vals,
     strs = strs,
     funs = funs,
     current = FNormal {cons = SM.empty,
                        vals = SM.empty,
                        strs = SM.empty,
                        funs = SM.empty},
     nested = current :: nested}

fun dummy f = {cons = IM.empty,
               vals = IM.empty,
               strs = IM.empty,
               funs = IM.empty,
               current = f,
               nested = []}

fun leave {cons, vals, strs, funs, current, nested = m1 :: rest} =
        {outer = {cons = cons,
                  vals = vals,
                  strs = strs,
                  funs = funs,
                  current = m1,
                  nested = rest},
         inner = dummy current}
  | leave _ = raise Fail "Corify.St.leave"

fun ffi m vals = dummy (FFfi {mod = m, vals = vals})

fun bindStr ({cons, vals, strs, funs,
              current = FNormal {cons = mcons, vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
            x n ({current = f, ...} : t) =
    {cons = cons,
     vals = vals,
     strs = IM.insert (strs, n, f),
     funs = funs,
     current = FNormal {cons = mcons,
                        vals = mvals,
                        strs = SM.insert (mstrs, x, f),
                        funs = mfuns},
     nested = nested}
  | bindStr _ _ _ _ = raise Fail "Corify.St.bindStr"

fun lookupStrById ({strs, ...} : t) n =
    case IM.find (strs, n) of
        NONE => raise Fail "Corify.St.lookupStrById"
      | SOME f => dummy f

fun lookupStrByName (m, {current = FNormal {strs, ...}, ...} : t) =
    (case SM.find (strs, m) of
         NONE => raise Fail "Corify.St.lookupStrByName"
       | SOME f => dummy f)
  | lookupStrByName _ = raise Fail "Corify.St.lookupStrByName"

fun bindFunctor ({cons, vals, strs, funs,
                  current = FNormal {cons = mcons, vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
                x n na str =
    {cons = cons,
     vals = vals,
     strs = strs,
     funs = IM.insert (funs, n, (na, str)),
     current = FNormal {cons = mcons,
                        vals = mvals,
                        strs = mstrs,
                        funs = SM.insert (mfuns, x, (na, str))},
     nested = nested}
  | bindFunctor _ _ _ _ _ = raise Fail "Corify.St.bindFunctor"

fun lookupFunctorById ({funs, ...} : t) n =
    case IM.find (funs, n) of
        NONE => raise Fail "Corify.St.lookupFunctorById"
      | SOME v => v

fun lookupFunctorByName (m, {current = FNormal {funs, ...}, ...} : t) =
    (case SM.find (funs, m) of
         NONE => raise Fail "Corify.St.lookupFunctorByName"
       | SOME v => v)
  | lookupFunctorByName _ = raise Fail "Corify.St.lookupFunctorByName"

end


fun corifyKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (corifyKind k1, corifyKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (corifyKind k), loc)
      | L.KUnit => (L'.KUnit, loc)

fun corifyCon st (c, loc) =
    case c of
        L.TFun (t1, t2) => (L'.TFun (corifyCon st t1, corifyCon st t2), loc)
      | L.TCFun (x, k, t) => (L'.TCFun (x, corifyKind k, corifyCon st t), loc)
      | L.TRecord c => (L'.TRecord (corifyCon st c), loc)

      | L.CRel n => (L'.CRel n, loc)
      | L.CNamed n =>
        (case St.lookupConById st n of
             NONE => (L'.CNamed n, loc)
           | SOME n => (L'.CNamed n, loc))
      | L.CModProj (m, ms, x) =>
        let
            val st = St.lookupStrById st m
            val st = foldl St.lookupStrByName st ms
        in
            case St.lookupConByName st x of
                St.CNormal n => (L'.CNamed n, loc)
              | St.CFfi m => (L'.CFfi (m, x), loc)
        end

      | L.CApp (c1, c2) => (L'.CApp (corifyCon st c1, corifyCon st c2), loc)
      | L.CAbs (x, k, c) => (L'.CAbs (x, corifyKind k, corifyCon st c), loc)

      | L.CName s => (L'.CName s, loc)

      | L.CRecord (k, xcs) =>
        (L'.CRecord (corifyKind k, map (fn (c1, c2) => (corifyCon st c1, corifyCon st c2)) xcs), loc)
      | L.CConcat (c1, c2) => (L'.CConcat (corifyCon st c1, corifyCon st c2), loc)
      | L.CFold (k1, k2) => (L'.CFold (corifyKind k1, corifyKind k2), loc)
      | L.CUnit => (L'.CUnit, loc)

fun corifyExp st (e, loc) =
    case e of
        L.EPrim p => (L'.EPrim p, loc)
      | L.ERel n => (L'.ERel n, loc)
      | L.ENamed n =>
        (case St.lookupValById st n of
             NONE => (L'.ENamed n, loc)
           | SOME n => (L'.ENamed n, loc))
      | L.EModProj (m, ms, x) =>
        let
            val st = St.lookupStrById st m
            val st = foldl St.lookupStrByName st ms
        in
            case St.lookupValByName st x of
                St.ENormal n => (L'.ENamed n, loc)
              | St.EFfi (m, t) =>
                case t of
                    (L'.TFun (dom as (L'.TRecord (L'.CRecord (_, []), _), _), ran), _) =>
                    (L'.EAbs ("arg", dom, ran, (L'.EFfiApp (m, x, []), loc)), loc)
                  | t as (L'.TFun _, _) =>
                    let
                        fun getArgs (all as (t, _), args) =
                            case t of
                                L'.TFun (dom, ran) => getArgs (ran, dom :: args)
                              | _ => (all, rev args)
                                     
                        val (result, args) = getArgs (t, [])

                        val (actuals, _) = foldr (fn (_, (actuals, n)) =>
                                                     ((L'.ERel n, loc) :: actuals,
                                                      n + 1)) ([], 0) args
                        val app = (L'.EFfiApp (m, x, actuals), loc)
                        val (abs, _, _) = foldr (fn (t, (abs, ran, n)) =>
                                                    ((L'.EAbs ("arg" ^ Int.toString n,
                                                               t,
                                                               ran,
                                                               abs), loc),
                                                     (L'.TFun (t, ran), loc),
                                                     n - 1)) (app, result, length args - 1) args
                    in
                        abs
                    end
                  | _ => (L'.EFfi (m, x), loc)
        end
      | L.EApp (e1, e2) => (L'.EApp (corifyExp st e1, corifyExp st e2), loc)
      | L.EAbs (x, dom, ran, e1) => (L'.EAbs (x, corifyCon st dom, corifyCon st ran, corifyExp st e1), loc)
      | L.ECApp (e1, c) => (L'.ECApp (corifyExp st e1, corifyCon st c), loc)
      | L.ECAbs (x, k, e1) => (L'.ECAbs (x, corifyKind k, corifyExp st e1), loc)

      | L.ERecord xes => (L'.ERecord (map (fn (c, e, t) =>
                                              (corifyCon st c, corifyExp st e, corifyCon st t)) xes), loc)
      | L.EField (e1, c, {field, rest}) => (L'.EField (corifyExp st e1, corifyCon st c,
                                                       {field = corifyCon st field, rest = corifyCon st rest}), loc)
      | L.EFold k => (L'.EFold (corifyKind k), loc)
      | L.EWrite e => (L'.EWrite (corifyExp st e), loc)

fun corifyDecl ((d, loc : EM.span), st) =
    case d of
        L.DCon (x, n, k, c) =>
        let
            val (st, n) = St.bindCon st x n
        in
            ([(L'.DCon (x, n, corifyKind k, corifyCon st c), loc)], st)
        end
      | L.DVal (x, n, t, e) =>
        let
            val (st, n) = St.bindVal st x n
            val s =
                if String.isPrefix "wrap_" x then
                    String.extract (x, 5, NONE)
                else
                    x
        in
            ([(L'.DVal (x, n, corifyCon st t, corifyExp st e, s), loc)], st)
        end
      | L.DValRec vis =>
        let
            val (vis, st) = ListUtil.foldlMap
                            (fn ((x, n, t, e), st) =>
                                let
                                    val (st, n) = St.bindVal st x n
                                    val s =
                                        if String.isPrefix "wrap_" x then
                                            String.extract (x, 5, NONE)
                                        else
                                            x
                                in
                                    ((x, n, corifyCon st t, corifyExp st e, s), st)
                                end)
                            st vis
        in
            ([(L'.DValRec vis, loc)], st)
        end
      | L.DSgn _ => ([], st)

      | L.DStr (x, n, _, (L.StrFun (_, na, _, _, str), _)) =>
        ([], St.bindFunctor st x n na str)

      | L.DStr (x, n, _, str) =>
        let
            val (ds, {inner, outer}) = corifyStr (str, st)
            val st = St.bindStr outer x n inner
        in
            (ds, st)
        end

      | L.DFfiStr (m, n, (sgn, _)) =>
        (case sgn of
             L.SgnConst sgis =>
             let
                 val (ds, cmap, st) =
                     foldl (fn ((sgi, _), (ds, cmap, st)) =>
                               case sgi of
                                   L.SgiConAbs (x, n, k) =>
                                   let
                                       val (st, n') = St.bindCon st x n
                                   in
                                       ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                        cmap,
                                        st)
                                   end
                                 | L.SgiCon (x, n, k, _) =>
                                   let
                                       val (st, n') = St.bindCon st x n
                                   in
                                       ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                        cmap,
                                        st)
                                   end

                                 | L.SgiVal (x, _, c) =>
                                   (ds,
                                    SM.insert (cmap, x, corifyCon st c),
                                    st)
                                 | _ => (ds, cmap, st)) ([], SM.empty, st) sgis

                 val st = St.bindStr st m n (St.ffi m cmap)
             in
                 (rev ds, st)
             end
           | _ => raise Fail "Non-const signature for FFI structure")

      | L.DExport (en, sgn, str) =>
        (case #1 sgn of
             L.SgnConst sgis =>
             let
                 fun pathify (str, _) =
                     case str of
                         L.StrVar m => SOME (m, [])
                       | L.StrProj (str, s) =>
                         Option.map (fn (m, ms) => (m, ms @ [s])) (pathify str)
                       | _ => NONE
             in
                 case pathify str of
                     NONE => (ErrorMsg.errorAt loc "Structure is too fancy to export";
                              ([], st))
                   | SOME (m, ms) =>
                     let
                         fun wrapSgi ((sgi, _), (wds, eds))  =
                             case sgi of
                                 L.SgiVal (s, _, t as (L.TFun (dom, ran), _)) =>
                                 (case (#1 dom, #1 ran) of
                                      (L.TRecord _,
                                       L.CApp ((L.CModProj (_, [], "xml"), _),
                                               (L.CRecord (_, [((L.CName "Html", _),
                                                                _)]), _))) =>
                                      let
                                          val ran = (L.TRecord (L.CRecord ((L.KType, loc), []), loc), loc)
                                          val e = (L.EModProj (m, ms, s), loc)
                                          val e = (L.EAbs ("vs", dom, ran,
                                                           (L.EWrite (L.EApp (e, (L.ERel 0, loc)), loc), loc)), loc)
                                      in
                                          ((L.DVal ("wrap_" ^ s, 0,
                                                    (L.TFun (dom, ran), loc),
                                                    e), loc) :: wds,
                                           (fn st =>
                                               case #1 (corifyExp st (L.EModProj (en, [], "wrap_" ^ s), loc)) of
                                                   L'.ENamed n => (L'.DExport n, loc)
                                                 | _ => raise Fail "Corify: Value to export didn't corify properly")
                                           :: eds)
                                      end
                                    | _ => (wds, eds))
                               | _ => (wds, eds)

                         val (wds, eds) = foldl wrapSgi ([], []) sgis
                         val wrapper = (L.StrConst wds, loc)
                         val (ds, {inner, outer}) = corifyStr (wrapper, st)
                         val st = St.bindStr outer "wrapper" en inner
                         
                         val ds = ds @ map (fn f => f st) eds
                     in
                         (ds, st)
                     end
             end
           | _ => raise Fail "Non-const signature for 'export'")
                 

and corifyStr ((str, _), st) =
    case str of
        L.StrConst ds =>
        let
            val st = St.enter st
            val (ds, st) = ListUtil.foldlMapConcat corifyDecl st ds
        in
            (ds, St.leave st)
        end
      | L.StrVar n => ([], {inner = St.lookupStrById st n, outer = st})
      | L.StrProj (str, x) =>
        let
            val (ds, {inner, outer}) = corifyStr (str, st)
        in
            (ds, {inner = St.lookupStrByName (x, inner), outer = outer})
        end
      | L.StrFun _ => raise Fail "Corify of nested functor definition"
      | L.StrApp (str1, str2) =>
        let
            fun unwind' (str, _) =
                case str of
                    L.StrVar n => St.lookupStrById st n
                  | L.StrProj (str, x) => St.lookupStrByName (x, unwind' str)
                  | _ => raise Fail "Corify of fancy functor application [1]"

            fun unwind (str, _) =
                case str of
                    L.StrVar n => St.lookupFunctorById st n
                  | L.StrProj (str, x) => St.lookupFunctorByName (x, unwind' str)
                  | _ => raise Fail "Corify of fancy functor application [2]"

            val (na, body) = unwind str1

            val (ds1, {inner, outer}) = corifyStr (str2, st)
            val (ds2, sts) = corifyStr (body, St.bindStr outer "ARG" na inner)
        in
            (ds1 @ ds2, sts)
        end

fun maxName ds = foldl (fn ((d, _), n) =>
                           case d of
                               L.DCon (_, n', _, _) => Int.max (n, n')
                             | L.DVal (_, n', _, _) => Int.max (n, n')
                             | L.DValRec vis => foldl (fn ((_, n', _, _), n) => Int.max (n, n)) n vis
                             | L.DSgn (_, n', _) => Int.max (n, n')
                             | L.DStr (_, n', _, str) => Int.max (n, Int.max (n', maxNameStr str))
                             | L.DFfiStr (_, n', _) => Int.max (n, n')
                             | L.DExport _ => n)
                 0 ds

and maxNameStr (str, _) =
    case str of
        L.StrConst ds => maxName ds
      | L.StrVar n => n
      | L.StrProj (str, _) => maxNameStr str
      | L.StrFun (_, _, _, _, str) => maxNameStr str
      | L.StrApp (str1, str2) => Int.max (maxNameStr str1, maxNameStr str2)

fun corify ds =
    let
        val () = reset (maxName ds + 1)
        val (ds, _) = ListUtil.foldlMapConcat corifyDecl St.empty ds
    in
        ds
    end

end
