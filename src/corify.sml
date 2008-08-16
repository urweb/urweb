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

    val debug : t -> unit

    val enter : t -> t
    val leave : t -> {outer : t, inner : t}
    val ffi : string -> L'.con SM.map -> (string * string list * L'.con option * L'.datatype_kind) SM.map -> t

    datatype core_con =
             CNormal of int
           | CFfi of string
    val bindCon : t -> string -> int -> t * int
    val lookupConById : t -> int -> int option
    val lookupConByName : t -> string -> core_con

    val bindConstructor : t -> string -> int -> L'.patCon -> t
    val lookupConstructorByNameOpt : t -> string -> L'.patCon option
    val lookupConstructorByName : t -> string -> L'.patCon
    val lookupConstructorById : t -> int -> L'.patCon

     datatype core_val =
              ENormal of int
            | EFfi of string * L'.con
     val bindVal : t -> string -> int -> t * int
     val bindConstructorVal : t -> string -> int -> t
     val lookupValById : t -> int -> int option
     val lookupValByName : t -> string -> core_val

     val bindStr : t -> string -> int -> t -> t
     val lookupStrById : t -> int -> t
     val lookupStrByName : string * t -> t

     val bindFunctor : t -> string -> int -> string -> int -> L.str -> t
     val lookupFunctorById : t -> int -> string * int * L.str
     val lookupFunctorByName : string * t -> string * int * L.str
 end = struct

 datatype flattening =
          FNormal of {cons : int SM.map,
                      constructors : L'.patCon SM.map,
                      vals : int SM.map,
                      strs : flattening SM.map,
                      funs : (string * int * L.str) SM.map}
        | FFfi of {mod : string,
                   vals : L'.con SM.map,
                   constructors : (string * string list * L'.con option * L'.datatype_kind) SM.map}

 type t = {
      cons : int IM.map,
      constructors : L'.patCon IM.map,
      vals : int IM.map,
      strs : flattening IM.map,
      funs : (string * int * L.str) IM.map,
      current : flattening,
      nested : flattening list
 }

 val empty = {
     cons = IM.empty,
     constructors = IM.empty,
     vals = IM.empty,
     strs = IM.empty,
     funs = IM.empty,
     current = FNormal { cons = SM.empty, constructors = SM.empty, vals = SM.empty, strs = SM.empty, funs = SM.empty },
     nested = []
 }

 fun debug ({current = FNormal {cons, constructors, vals, strs, funs}, ...} : t) =
     print ("cons: " ^ Int.toString (SM.numItems cons) ^ "; "
            ^ "constructors: " ^ Int.toString (SM.numItems constructors) ^ "; "
            ^ "vals: " ^ Int.toString (SM.numItems vals) ^ "; "
            ^ "strs: " ^ Int.toString (SM.numItems strs) ^ "; "
            ^ "funs: " ^ Int.toString (SM.numItems funs) ^ "\n")
   | debug _ = print "Not normal!\n"

 datatype core_con =
          CNormal of int
        | CFfi of string

 datatype core_val =
          ENormal of int
        | EFfi of string * L'.con

 fun bindCon {cons, constructors, vals, strs, funs, current, nested} s n =
     let
         val n' = alloc ()

         val current =
             case current of
                 FFfi _ => raise Fail "Binding inside FFfi"
               | FNormal {cons, constructors, vals, strs, funs} =>
                 FNormal {cons = SM.insert (cons, s, n'),
                          constructors = constructors,
                          vals = vals,
                          strs = strs,
                          funs = funs}
     in
         ({cons = IM.insert (cons, n, n'),
           constructors = constructors,
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

 fun bindVal {cons, constructors, vals, strs, funs, current, nested} s n =
     let
         val n' = alloc ()

         val current =
             case current of
                 FFfi _ => raise Fail "Binding inside FFfi"
               | FNormal {cons, constructors, vals, strs, funs} =>
                 FNormal {cons = cons,
                          constructors = constructors,
                          vals = SM.insert (vals, s, n'),
                          strs = strs,
                          funs = funs}
     in
         ({cons = cons,
           constructors = constructors,
           vals = IM.insert (vals, n, n'),
           strs = strs,
           funs = funs,
           current = current,
           nested = nested},
          n')
     end

 fun bindConstructorVal {cons, constructors, vals, strs, funs, current, nested} s n =
     let
         val current =
             case current of
                 FFfi _ => raise Fail "Binding inside FFfi"
               | FNormal {cons, constructors, vals, strs, funs} =>
                 FNormal {cons = cons,
                          constructors = constructors,
                          vals = SM.insert (vals, s, n),
                          strs = strs,
                          funs = funs}
     in
         {cons = cons,
          constructors = constructors,
          vals = IM.insert (vals, n, n),
          strs = strs,
          funs = funs,
          current = current,
          nested = nested}
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

 fun bindConstructor {cons, constructors, vals, strs, funs, current, nested} s n n' =
     let
         val current =
             case current of
                 FFfi _ => raise Fail "Binding inside FFfi"
               | FNormal {cons, constructors, vals, strs, funs} =>
                 FNormal {cons = cons,
                          constructors = SM.insert (constructors, s, n'),
                          vals = vals,
                          strs = strs,
                          funs = funs}
     in
         {cons = cons,
          constructors = IM.insert (constructors, n, n'),
          vals = vals,
          strs = strs,
          funs = funs,
          current = current,
          nested = nested}
     end

 fun lookupConstructorById ({constructors, ...} : t) n =
     case IM.find (constructors, n) of
         NONE => raise Fail "Corify.St.lookupConstructorById"
       | SOME x => x

 fun lookupConstructorByNameOpt ({current, ...} : t) x =
     case current of
         FFfi {mod = m, constructors, ...} =>
         (case SM.find (constructors, x) of
              NONE => NONE
            | SOME (n, xs, to, dk) => SOME (L'.PConFfi {mod = m, datatyp = n, params = xs, con = x, arg = to, kind = dk}))
       | FNormal {constructors, ...} =>
         case SM.find (constructors, x) of
             NONE => NONE
           | SOME n => SOME n

 fun lookupConstructorByName ({current, ...} : t) x =
     case current of
         FFfi {mod = m, constructors, ...} =>
         (case SM.find (constructors, x) of
              NONE => raise Fail "Corify.St.lookupConstructorByName [1]"
            | SOME (n, xs, to, dk) => L'.PConFfi {mod = m, datatyp = n, params = xs, con = x, arg = to, kind = dk})
       | FNormal {constructors, ...} =>
         case SM.find (constructors, x) of
             NONE => raise Fail "Corify.St.lookupConstructorByName [2]"
           | SOME n => n

 fun enter {cons, constructors, vals, strs, funs, current, nested} =
     {cons = cons,
      constructors = constructors,
      vals = vals,
      strs = strs,
      funs = funs,
      current = FNormal {cons = SM.empty,
                         constructors = SM.empty,
                         vals = SM.empty,
                         strs = SM.empty,
                         funs = SM.empty},
      nested = current :: nested}

 fun dummy f = {cons = IM.empty,
                constructors = IM.empty,
                vals = IM.empty,
                strs = IM.empty,
                funs = IM.empty,
                current = f,
                nested = []}

 fun leave {cons, constructors, vals, strs, funs, current, nested = m1 :: rest} =
         {outer = {cons = cons,
                   constructors = constructors,
                   vals = vals,
                   strs = strs,
                   funs = funs,
                   current = m1,
                   nested = rest},
          inner = dummy current}
   | leave _ = raise Fail "Corify.St.leave"

 fun ffi m vals constructors = dummy (FFfi {mod = m, vals = vals, constructors = constructors})

 fun bindStr ({cons, constructors, vals, strs, funs,
               current = FNormal {cons = mcons, constructors = mconstructors,
                                  vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
             x n ({current = f, ...} : t) =
     {cons = cons,
      constructors = constructors,
      vals = vals,
      strs = IM.insert (strs, n, f),
      funs = funs,
      current = FNormal {cons = mcons,
                         constructors = mconstructors,
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

 fun bindFunctor ({cons, constructors, vals, strs, funs,
                   current = FNormal {cons = mcons, constructors = mconstructors,
                                      vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
                 x n xa na str =
     {cons = cons,
      constructors = constructors,
      vals = vals,
      strs = strs,
      funs = IM.insert (funs, n, (xa, na, str)),
      current = FNormal {cons = mcons,
                         constructors = mconstructors,
                         vals = mvals,
                         strs = mstrs,
                         funs = SM.insert (mfuns, x, (xa, na, str))},
      nested = nested}
   | bindFunctor _ _ _ _ _ _ = raise Fail "Corify.St.bindFunctor"

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
       | L.KTuple _ => raise Fail "Corify KTuple"

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

       | L.CTuple _ => raise Fail "Corify CTuple"
       | L.CProj _ => raise Fail "Corify CProj"

 fun corifyPatCon st pc =
     case pc of
         L.PConVar n => St.lookupConstructorById st n
       | L.PConProj (m1, ms, x) =>
         let
             val st = St.lookupStrById st m1
             val st = foldl St.lookupStrByName st ms
         in
             St.lookupConstructorByName st x
         end

 fun corifyPat st (p, loc) =
     case p of
         L.PWild => (L'.PWild, loc)
       | L.PVar (x, t) => (L'.PVar (x, corifyCon st t), loc)
       | L.PPrim p => (L'.PPrim p, loc)
       | L.PCon (dk, pc, ts, po) => (L'.PCon (dk, corifyPatCon st pc, map (corifyCon st) ts,
                                              Option.map (corifyPat st) po), loc)
       | L.PRecord xps => (L'.PRecord (map (fn (x, p, t) => (x, corifyPat st p, corifyCon st t)) xps), loc)

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
             case St.lookupConstructorByNameOpt st x of
                 SOME (pc as L'.PConFfi {mod = m, datatyp, params, arg, kind, ...}) =>
                 let
                     val args = ListUtil.mapi (fn (i, _) => (L'.CRel i, loc)) params
                     val e = case arg of
                                 NONE => (L'.ECon (kind, pc, args, NONE), loc)
                               | SOME dom => (L'.EAbs ("x", dom, (L'.CFfi (m, datatyp), loc),
                                                       (L'.ECon (kind, pc, args, SOME (L'.ERel 0, loc)), loc)), loc)

                     val k = (L'.KType, loc)
                 in
                     foldr (fn (x, e) => (L'.ECAbs (x, k, e), loc)) e params
                 end
               | _ =>
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
       | L.ECut (e1, c, {field, rest}) => (L'.ECut (corifyExp st e1, corifyCon st c,
                                                    {field = corifyCon st field, rest = corifyCon st rest}), loc)
       | L.EFold k => (L'.EFold (corifyKind k), loc)

       | L.ECase (e, pes, {disc, result}) =>
         (L'.ECase (corifyExp st e,
                    map (fn (p, e) => (corifyPat st p, corifyExp st e)) pes,
                    {disc = corifyCon st disc, result = corifyCon st result}),
          loc)

       | L.EWrite e => (L'.EWrite (corifyExp st e), loc)

 fun corifyDecl ((d, loc : EM.span), st) =
     case d of
         L.DCon (x, n, k, c) =>
         let
             val (st, n) = St.bindCon st x n
         in
             ([(L'.DCon (x, n, corifyKind k, corifyCon st c), loc)], st)
         end
       | L.DDatatype (x, n, xs, xncs) =>
         let
             val (st, n) = St.bindCon st x n
             val (xncs, st) = ListUtil.foldlMap (fn ((x, n, co), st) =>
                                                    let
                                                        val st = St.bindConstructor st x n (L'.PConVar n)
                                                        val st = St.bindConstructorVal st x n
                                                        val co = Option.map (corifyCon st) co
                                                    in
                                                        ((x, n, co), st)
                                                    end) st xncs

             val dk = ElabUtil.classifyDatatype xncs
             val t = (L'.CNamed n, loc)
             val nxs = length xs - 1
             val t = ListUtil.foldli (fn (i, _, t) => (L'.CApp (t, (L'.CRel (nxs - i), loc)), loc)) t xs
             val k = (L'.KType, loc)
             val dcons = map (fn (x, n, to) =>
                                 let
                                     val args = ListUtil.mapi (fn (i, _) => (L'.CRel (nxs - i), loc)) xs
                                     val (e, t) =
                                         case to of
                                             NONE => ((L'.ECon (dk, L'.PConVar n, args, NONE), loc), t)
                                           | SOME t' => ((L'.EAbs ("x", t', t,
                                                                   (L'.ECon (dk, L'.PConVar n, args,
                                                                             SOME (L'.ERel 0, loc)),
                                                                    loc)),
                                                          loc),
                                                         (L'.TFun (t', t), loc))

                                     val t = foldr (fn (x, t) => (L'.TCFun (x, k, t), loc)) t xs
                                     val e = foldr (fn (x, e) => (L'.ECAbs (x, k, e), loc)) e xs
                                 in
                                     (L'.DVal (x, n, t, e, ""), loc)
                                 end) xncs
         in
             ((L'.DDatatype (x, n, xs, xncs), loc) :: dcons, st)
         end
       | L.DDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
         let
             val (st, n) = St.bindCon st x n
             val c = corifyCon st (L.CModProj (m1, ms, s), loc)

             val m = foldl (fn (x, m) => (L.StrProj (m, x), loc)) (L.StrVar m1, loc) ms
             val (_, {inner, ...}) = corifyStr (m, st)

             val (xncs, st) = ListUtil.foldlMap (fn ((x, n, co), st) =>
                                                    let
                                                        val n' = St.lookupConstructorByName inner x
                                                        val st = St.bindConstructor st x n n'
                                                        val (st, n) = St.bindVal st x n
                                                        val co = Option.map (corifyCon st) co
                                                    in
                                                        ((x, n, co), st)
                                                    end) st xncs

             val nxs = length xs - 1
             val c = ListUtil.foldli (fn (i, _, c) => (L'.CApp (c, (L'.CRel (nxs - i), loc)), loc)) c xs
             val k = (L'.KType, loc)
             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs

             val cds = map (fn (x, n, co) =>
                               let
                                   val t = case co of
                                               NONE => c
                                             | SOME t' => (L'.TFun (t', c), loc)
                                   val e = corifyExp st (L.EModProj (m1, ms, x), loc)

                                   val t = foldr (fn (x, t) => (L'.TCFun (x, k, t), loc)) t xs
                               in
                                   (L'.DVal (x, n, t, e, x), loc)
                               end) xncs
         in
             ((L'.DCon (x, n, k', c), loc) :: cds, st)
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
                                     in
                                         ((x, n, t, e), st)
                                     end)
                                 st vis

             val vis = map
                           (fn (x, n, t, e) =>
                               let
                                   val s =
                                       if String.isPrefix "wrap_" x then
                                           String.extract (x, 5, NONE)
                                       else
                                           x
                               in
                                   (x, n, corifyCon st t, corifyExp st e, s)
                               end)
                           vis
         in
             ([(L'.DValRec vis, loc)], st)
         end
       | L.DSgn _ => ([], st)

       | L.DStr (x, n, _, (L.StrFun (xa, na, _, _, str), _)) =>
         ([], St.bindFunctor st x n xa na str)

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
                  val (ds, cmap, conmap, st) =
                      foldl (fn ((sgi, _), (ds, cmap, conmap, st)) =>
                                case sgi of
                                    L.SgiConAbs (x, n, k) =>
                                    let
                                        val (st, n') = St.bindCon st x n
                                    in
                                        ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                         cmap,
                                         conmap,
                                         st)
                                    end
                                  | L.SgiCon (x, n, k, _) =>
                                    let
                                        val (st, n') = St.bindCon st x n
                                    in
                                        ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                         cmap,
                                         conmap,
                                         st)
                                    end

                                  | L.SgiDatatype (x, n, xs, xnts) =>
                                    let
                                        val k = (L'.KType, loc)
                                        val dk = ElabUtil.classifyDatatype xnts
                                        val (st, n') = St.bindCon st x n
                                        val (xnts, (ds', st, cmap, conmap)) =
                                            ListUtil.foldlMap
                                                (fn ((x', n, to), (ds', st, cmap, conmap)) =>
                                                    let
                                                        val dt = (L'.CNamed n', loc)
                                                        val args = ListUtil.mapi (fn (i, _) => (L'.CRel i, loc)) xs

                                                        val to = Option.map (corifyCon st) to

                                                        val pc = L'.PConFfi {mod = m,
                                                                             datatyp = x,
                                                                             params = xs,
                                                                             con = x',
                                                                             arg = to,
                                                                             kind = dk}

                                                        fun wrapT t =
                                                            foldr (fn (x, t) => (L'.TCFun (x, k, t), loc)) t xs
                                                        fun wrapE e =
                                                            foldr (fn (x, e) => (L'.ECAbs (x, k, e), loc)) e xs

                                                        val (cmap, d) =
                                                            case to of
                                                                NONE => (SM.insert (cmap, x', wrapT dt),
                                                                         (L'.DVal (x', n, wrapT dt,
                                                                                   wrapE
                                                                                       (L'.ECon (dk, pc, args, NONE),
                                                                                        loc),
                                                                                   ""), loc))
                                                              | SOME t =>
                                                                let
                                                                    val tf = (L'.TFun (t, dt), loc)
                                                                    val e = wrapE (L'.EAbs ("x", t, tf,
                                                                                            (L'.ECon (dk, pc, args,
                                                                                                      SOME (L'.ERel 0,
                                                                                                            loc)),
                                                                                             loc)), loc)
                                                                    val d = (L'.DVal (x', n, wrapT tf,
                                                                                      e, ""), loc)
                                                               in
                                                                   (SM.insert (cmap, x', wrapT tf), d)
                                                               end

                                                       val st = St.bindConstructor st x' n pc

                                                       val conmap = SM.insert (conmap, x', (x, xs, to, dk))
                                                   in
                                                       ((x', n, to),
                                                        (d :: ds', st, cmap, conmap))
                                                   end) ([], st, cmap, conmap) xnts
                                   in
                                       (ds' @ (L'.DDatatype (x, n', xs, xnts), loc) :: ds,
                                        cmap,
                                        conmap,
                                        st)
                                   end

                                 | L.SgiVal (x, _, c) =>
                                   (ds,
                                    SM.insert (cmap, x, corifyCon st c),
                                    conmap,
                                    st)
                                 | _ => (ds, cmap, conmap, st)) ([], SM.empty, SM.empty, st) sgis

                 val st = St.bindStr st m n (St.ffi m cmap conmap)
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
                                      (L.TRecord (L.CRecord (_, []), _),
                                       L.CApp
                                           ((L.CApp
                                                 ((L.CApp ((L.CModProj (_, [], "xml"), _),
                                                           (L.CRecord (_, [((L.CName "Html", _),
                                                                            _)]), _)), _), _), _), _)) =>
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
                                                   L'.ENamed n => (L'.DExport (L'.Link, n), loc)
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

            val (xa, na, body) = unwind str1

            val (ds1, {inner = inner', outer}) = corifyStr (str2, st)
            val (ds2, {inner, outer}) = corifyStr (body, St.bindStr outer xa na inner')
        in
            (ds1 @ ds2, {inner = St.bindStr inner xa na inner', outer = outer})
        end

fun maxName ds = foldl (fn ((d, _), n) =>
                           case d of
                               L.DCon (_, n', _, _) => Int.max (n, n')
                             | L.DDatatype (_, n', _, _) => Int.max (n, n')
                             | L.DDatatypeImp (_, n', _, _, _, _, _) => Int.max (n, n')
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
