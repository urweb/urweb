(* Copyright (c) 2008-2012, Adam Chlipala
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

fun doRestify k (mods, s) =
    let
        val s = if String.isPrefix "wrap_" s then
                    String.extract (s, 5, NONE)
                else
                    s
        val s = String.concatWith "/" (rev (s :: mods))
        val s = String.implode (List.filter (fn ch => ch <> #"$") (String.explode s))
    in
        Settings.rewrite k s
    end

val relify = CharVector.map (fn #"/" => #"_"
                              | ch => ch)

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

fun getCounter () = !count
fun setCounter n = count := n

end

structure St : sig
    type t

    val empty : t

    val debug : t -> unit

    val name : t -> string list

    val enter : t * string list -> t
    val leave : t -> {outer : t, inner : t}
    val ffi : string -> L'.con SM.map -> (string * string list * L'.con option * L'.datatype_kind) SM.map -> t

    val basisIs : t * int -> t
    val lookupBasis : t -> int option

    datatype core_con =
             CNormal of int
           | CFfi of string
    val bindCon : t -> string -> int -> t * int
    val lookupConById : t -> int -> int option
    val lookupConByName : t -> string -> core_con

    val bindConstructor : t -> string -> int -> t * int
    val bindConstructorAs : t -> string -> int -> L'.patCon -> t
    val lookupConstructorByNameOpt : t -> string -> L'.patCon option
    val lookupConstructorByName : t -> string -> L'.patCon
    val lookupConstructorById : t -> int -> L'.patCon
    val lookupConstructorByIdOpt : t -> int -> L'.patCon option
                                            
    datatype core_val =
             ENormal of int
           | EFfi of string * L'.con
    val bindVal : t -> string -> int -> t * int
    val bindConstructorVal : t -> string -> int -> int -> t
    val lookupValById : t -> int -> int option
    val lookupValByName : t -> string -> core_val

    val bindStr : t -> string -> int -> t -> t
    val lookupStrById : t -> int -> t
    val lookupStrByIdOpt : t -> int -> t option
    val lookupStrByName : string * t -> t
    val lookupStrByNameOpt : string * t -> t option

    val bindFunctor : t -> string -> int -> string -> int -> L.str -> t
    val lookupFunctorById : t -> int -> string * int * L.str
    val lookupFunctorByIdOpt : t -> int -> (string * int * L.str) option
    val lookupFunctorByName : string * t -> string * int * L.str
end = struct

datatype flattening =
         FNormal of {name : string list,
                     cons : int SM.map,
                     constructors : L'.patCon SM.map,
                     vals : int SM.map,
                     strs : flattening SM.map,
                     funs : (string * int * L.str) SM.map}
       | FFfi of {mod : string,
                  vals : L'.con SM.map,
                  constructors : (string * string list * L'.con option * L'.datatype_kind) SM.map}

type t = {
     basis : int option,
     cons : int IM.map,
     constructors : L'.patCon IM.map,
     vals : int IM.map,
     strs : flattening IM.map,
     funs : (string * int * L.str) IM.map,
     current : flattening,
     nested : flattening list
}

val empty = {
    basis = NONE,
    cons = IM.empty,
    constructors = IM.empty,
    vals = IM.empty,
    strs = IM.empty,
    funs = IM.empty,
    current = FNormal { name = [], cons = SM.empty, constructors = SM.empty,
                        vals = SM.empty, strs = SM.empty, funs = SM.empty },
    nested = []
}

fun debug ({current = FNormal {cons, constructors, vals, strs, funs, ...}, ...} : t) =
    print ("cons: " ^ Int.toString (SM.numItems cons) ^ "; "
           ^ "constructors: " ^ Int.toString (SM.numItems constructors) ^ "; "
           ^ "vals: " ^ Int.toString (SM.numItems vals) ^ "; "
           ^ "strs: " ^ Int.toString (SM.numItems strs) ^ "; "
           ^ "funs: " ^ Int.toString (SM.numItems funs) ^ "\n")
  | debug _ = print "Not normal!\n"

fun name ({current = FNormal {name, ...}, ...} : t) = name
  | name {current = FFfi {mod = name, ...}, ...} = [name]

fun basisIs ({cons, constructors, vals, strs, funs, current, nested, ...} : t, basis) =
    {basis = SOME basis,
     cons = cons,
     constructors = constructors,
     vals = vals,
     strs = strs,
     funs = funs,
     current = current,
     nested = nested}

fun lookupBasis ({basis, ...} : t) = basis

datatype core_con =
         CNormal of int
       | CFfi of string

datatype core_val =
         ENormal of int
       | EFfi of string * L'.con

fun bindCon {basis, cons, constructors, vals, strs, funs, current, nested} s n =
    let
        val n' = alloc ()

        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {name, cons, constructors, vals, strs, funs} =>
                FNormal {name = name,
                         cons = SM.insert (cons, s, n'),
                         constructors = constructors,
                         vals = vals,
                         strs = strs,
                         funs = funs}
    in
        ({basis = basis,
          cons = IM.insert (cons, n, n'),
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
            NONE => raise Fail ("Corify.St.lookupConByName " ^ x)
          | SOME n => CNormal n

fun bindVal {basis, cons, constructors, vals, strs, funs, current, nested} s n =
    let
        val n' = alloc ()

        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {name, cons, constructors, vals, strs, funs} =>
                FNormal {name = name,
                         cons = cons,
                         constructors = constructors,
                         vals = SM.insert (vals, s, n'),
                         strs = strs,
                         funs = funs}
    in
        ({basis = basis,
          cons = cons,
          constructors = constructors,
          vals = IM.insert (vals, n, n'),
          strs = strs,
          funs = funs,
          current = current,
          nested = nested},
         n')
    end

fun bindConstructorVal {basis, cons, constructors, vals, strs, funs, current, nested} s n n' =
    let
        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {name, cons, constructors, vals, strs, funs} =>
                FNormal {name = name,
                         cons = cons,
                         constructors = constructors,
                         vals = SM.insert (vals, s, n'),
                         strs = strs,
                         funs = funs}
    in
        {basis = basis,
         cons = cons,
         constructors = constructors,
         vals = IM.insert (vals, n, n'),
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
             NONE => raise Fail ("Corify.St.lookupValByName: no type for FFI val " ^ x)
           | SOME t => EFfi (m, t))
      | FNormal {name, vals, ...} =>
        case SM.find (vals, x) of
            NONE => raise Fail ("Corify.St.lookupValByName " ^ String.concatWith "." (rev name) ^ "." ^ x)
          | SOME n => ENormal n

fun bindConstructorAs {basis, cons, constructors, vals, strs, funs, current, nested} s n c' =
    let
        val current =
            case current of
                FFfi _ => raise Fail "Binding inside FFfi"
              | FNormal {name, cons, constructors, vals, strs, funs} =>
                FNormal {name = name,
                         cons = cons,
                         constructors = SM.insert (constructors, s, c'),
                         vals = vals,
                         strs = strs,
                         funs = funs}
    in
        {basis = basis,
         cons = cons,
         constructors = IM.insert (constructors, n, c'),
         vals = vals,
         strs = strs,
         funs = funs,
         current = current,
         nested = nested}
    end

fun bindConstructor st s n =
    let
        val n' = alloc ()
        val c' = L'.PConVar n'
    in
        (bindConstructorAs st s n c', n')
    end

fun lookupConstructorById ({constructors, ...} : t) n =
    case IM.find (constructors, n) of
        NONE => raise Fail "Corify.St.lookupConstructorById"
      | SOME x => x

fun lookupConstructorByIdOpt ({constructors, ...} : t) n =
    IM.find (constructors, n)

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

fun enter ({basis, cons, constructors, vals, strs, funs, current, nested}, name) =
    {basis = basis,
     cons = cons,
     constructors = constructors,
     vals = vals,
     strs = strs,
     funs = funs,
     current = FNormal {name = name,
                        cons = SM.empty,
                        constructors = SM.empty,
                        vals = SM.empty,
                        strs = SM.empty,
                        funs = SM.empty},
     nested = current :: nested}

fun dummy (b, f) = {basis = b,
                    cons = IM.empty,
                    constructors = IM.empty,
                    vals = IM.empty,
                    strs = IM.empty,
                    funs = IM.empty,
                    current = f,
                    nested = []}

fun leave {basis, cons, constructors, vals, strs, funs, current, nested = m1 :: rest} =
    {outer = {basis = basis,
              cons = cons,
              constructors = constructors,
              vals = vals,
              strs = strs,
              funs = funs,
              current = m1,
              nested = rest},
     inner = dummy (basis, current)}
  | leave _ = raise Fail "Corify.St.leave"

fun ffi m vals constructors = dummy (NONE, FFfi {mod = m, vals = vals, constructors = constructors})

fun bindStr ({basis, cons, constructors, vals, strs, funs,
              current = FNormal {name, cons = mcons, constructors = mconstructors,
                                 vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
            x n ({current = f, ...} : t) =
    {basis = basis,
     cons = cons,
     constructors = constructors,
     vals = vals,
     strs = IM.insert (strs, n, f),
     funs = funs,
     current = FNormal {name = name,
                        cons = mcons,
                        constructors = mconstructors,
                        vals = mvals,
                        strs = SM.insert (mstrs, x, f),
                        funs = mfuns},
     nested = nested}
  | bindStr _ _ _ _ = raise Fail "Corify.St.bindStr"

fun lookupStrById ({basis, strs, ...} : t) n =
    case IM.find (strs, n) of
        NONE => raise Fail ("Corify.St.lookupStrById(" ^ Int.toString n ^ ")")
      | SOME f => dummy (basis, f)

fun lookupStrByIdOpt ({basis, strs, ...} : t) n =
    case IM.find (strs, n) of
        NONE => NONE
      | SOME f => SOME (dummy (basis, f))

fun lookupStrByName (m, {basis, current = FNormal {strs, ...}, ...} : t) =
    (case SM.find (strs, m) of
         NONE => raise Fail "Corify.St.lookupStrByName [1]"
       | SOME f => dummy (basis, f))
  | lookupStrByName _ = raise Fail "Corify.St.lookupStrByName [2]"

fun lookupStrByNameOpt (m, {basis, current = FNormal {strs, ...}, ...} : t) =
    (case SM.find (strs, m) of
         NONE => NONE
       | SOME f => SOME (dummy (basis, f)))
  | lookupStrByNameOpt _ = NONE

fun bindFunctor ({basis, cons, constructors, vals, strs, funs,
                  current = FNormal {name, cons = mcons, constructors = mconstructors,
                                     vals = mvals, strs = mstrs, funs = mfuns}, nested} : t)
                x n xa na str =
    {basis = basis,
     cons = cons,
     constructors = constructors,
     vals = vals,
     strs = strs,
     funs = IM.insert (funs, n, (xa, na, str)),
     current = FNormal {name = name,
                        cons = mcons,
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

fun lookupFunctorByIdOpt ({funs, ...} : t) n =
    IM.find (funs, n)

fun lookupFunctorByName (m, {current = FNormal {funs, ...}, ...} : t) =
    (case SM.find (funs, m) of
         NONE => raise Fail ("Corify.St.lookupFunctorByName " ^ m ^ "[1]")
       | SOME v => v)
  | lookupFunctorByName _ = raise Fail "Corify.St.lookupFunctorByName [2]"

end


fun corifyKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (corifyKind k1, corifyKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (corifyKind k), loc)
      | L.KUnit => (L'.KUnit, loc)
      | L.KTuple ks => (L'.KTuple (map corifyKind ks), loc)

      | L.KRel n => (L'.KRel n, loc)
      | L.KFun (x, k) => (L'.KFun (x, corifyKind k), loc)

fun corifyCon st (c, loc) =
    case c of
        L.TFun (t1, t2) => (L'.TFun (corifyCon st t1, corifyCon st t2), loc)
      | L.TCFun (x, k, t) => (L'.TCFun (x, corifyKind k, corifyCon st t), loc)
      | L.TKFun (x, t) => (L'.TKFun (x, corifyCon st t), loc)
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
              | St.CFfi m => 
                if (m, x) = ("Basis", "unit") then 
                    (L'.TRecord (L'.CRecord ((L'.KType, loc), []), loc), loc)
                else
                    (L'.CFfi (m, x), loc)
        end

      | L.CApp (c1, c2) => (L'.CApp (corifyCon st c1, corifyCon st c2), loc)
      | L.CAbs (x, k, c) => (L'.CAbs (x, corifyKind k, corifyCon st c), loc)

      | L.CKApp (c1, k) => (L'.CKApp (corifyCon st c1, corifyKind k), loc)
      | L.CKAbs (x, c) => (L'.CKAbs (x, corifyCon st c), loc)

      | L.CName s => (L'.CName s, loc)

      | L.CRecord (k, xcs) =>
        (L'.CRecord (corifyKind k, map (fn (c1, c2) => (corifyCon st c1, corifyCon st c2)) xcs), loc)
      | L.CConcat (c1, c2) => (L'.CConcat (corifyCon st c1, corifyCon st c2), loc)
      | L.CMap (k1, k2) => (L'.CMap (corifyKind k1, corifyKind k2), loc)
      | L.CUnit => (L'.CUnit, loc)

      | L.CTuple cs => (L'.CTuple (map (corifyCon st) cs), loc)
      | L.CProj (c, n) => (L'.CProj (corifyCon st c, n), loc)

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
        L.PVar (x, t) => (L'.PVar (x, corifyCon st t), loc)
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
                        (L'.CApp ((L'.CFfi ("Basis", "transaction"), _), dom), _) =>
                        (L'.EAbs ("arg", dom, (L'.TRecord (L'.CRecord ((L'.KType, loc), []), loc), loc),
                                  (L'.EFfiApp (m, x, []), loc)), loc)
                      | t as (L'.TFun _, _) =>
                        let
                            fun getArgs (all as (t, _), args) =
                                case t of
                                    L'.TFun (dom, ran) => getArgs (ran, dom :: args)
                                  | _ => (all, rev args)

                            val (result, args) = getArgs (t, [])
                            val (isTransaction, result) =
                                case result of
                                    (L'.CApp ((L'.CFfi ("Basis", "transaction"), _),
                                              result), _) => (true, result)
                                  | _ => (false, result)

                            fun makeApp n =
                                let
                                    val (actuals, _) = foldr (fn (t, (actuals, n)) =>
                                                                 (((L'.ERel n, loc), t) :: actuals,
                                                                  n + 1)) ([], n) args
                                in
                                    (L'.EFfiApp (m, x, actuals), loc)
                                end
                            val unit = (L'.TRecord (L'.CRecord ((L'.KType, loc), []), loc), loc)
                            val (result, app) =
                                if isTransaction then
                                    ((L'.TFun (unit, result), loc),
                                     (L'.EAbs ("_",
                                               unit,
                                               result,
                                               makeApp 1), loc))
                                else
                                    (result, makeApp 0)
                                     
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
      | L.EKApp (e1, k) => (L'.EKApp (corifyExp st e1, corifyKind k), loc)
      | L.EKAbs (x, e1) => (L'.EKAbs (x, corifyExp st e1), loc)

      | L.ERecord xes => (L'.ERecord (map (fn (c, e, t) =>
                                              (corifyCon st c, corifyExp st e, corifyCon st t)) xes), loc)
      | L.EField (e1, c, {field, rest}) => (L'.EField (corifyExp st e1, corifyCon st c,
                                                       {field = corifyCon st field, rest = corifyCon st rest}), loc)
      | L.EConcat (e1, c1, e2, c2) => (L'.EConcat (corifyExp st e1, corifyCon st c1, corifyExp st e2,
                                                   corifyCon st c2), loc)
      | L.ECut (e1, c, {field, rest}) => (L'.ECut (corifyExp st e1, corifyCon st c,
                                                   {field = corifyCon st field, rest = corifyCon st rest}), loc)
      | L.ECutMulti (e1, c, {rest}) => (L'.ECutMulti (corifyExp st e1, corifyCon st c,
                                                      {rest = corifyCon st rest}), loc)

      | L.ECase (e, pes, {disc, result}) =>
        (L'.ECase (corifyExp st e,
                   map (fn (p, e) => (corifyPat st p, corifyExp st e)) pes,
                   {disc = corifyCon st disc, result = corifyCon st result}),
         loc)

      | L.EWrite e => (L'.EWrite (corifyExp st e), loc)

      | L.ELet (x, t, e1, e2) => (L'.ELet (x, corifyCon st t, corifyExp st e1, corifyExp st e2), loc)

fun isTransactional (c, _) =
    case c of
        L'.TFun (_, c) => isTransactional c
      | L'.CApp ((L'.CFfi ("Basis", "transaction"), _), _) => true
      | _ => false

fun corifyDecl mods (all as (d, loc : EM.span), st) =
    case d of
        L.DCon (x, n, k, c) =>
        let
            val (st, n) = St.bindCon st x n
        in
            ([(L'.DCon (x, n, corifyKind k, corifyCon st c), loc)], st)
        end
      | L.DDatatype dts =>
        let
            val (dts, st) = ListUtil.foldlMap (fn ((x, n, xs, xncs), st) =>
                                                  let
                                                      val (st, n) = St.bindCon st x n
                                                  in
                                                      ((x, n, xs, xncs), st)
                                                  end)
                                              st dts

            val (dts, (st, dcons)) =
                ListUtil.foldlMap
                    (fn ((x, n, xs, xncs), (st, dcons)) =>
                        let
                            val (xncs, st) = ListUtil.foldlMap
                                                 (fn ((x, n, co), st) =>
                                                     let
                                                         val (st, n') = St.bindConstructor st x n
                                                         val st = St.bindConstructorVal st x n n'
                                                         val co = Option.map (corifyCon st) co
                                                     in
                                                         ((x, n', co), st)
                                                     end) st xncs

                            val dk = ElabUtil.classifyDatatype xncs
                            val t = (L'.CNamed n, loc)
                            val nxs = length xs - 1
                            val t = ListUtil.foldli
                                        (fn (i, _, t) => (L'.CApp (t, (L'.CRel (nxs - i), loc)), loc)) t xs
                            val k = (L'.KType, loc)
                            val dcons' = map (fn (x, n, to) =>
                                                 let
                                                     val args = ListUtil.mapi
                                                                    (fn (i, _) => (L'.CRel (nxs - i), loc)) xs
                                                     val (e, t) =
                                                         case to of
                                                             NONE => ((L'.ECon (dk, L'.PConVar n, args, NONE),
                                                                       loc), t)
                                                           | SOME t' => ((L'.EAbs ("x", t', t,
                                                                                   (L'.ECon (dk, L'.PConVar n,
                                                                                             args,
                                                                                             SOME (L'.ERel 0,
                                                                                                   loc)),
                                                                                    loc)),
                                                                          loc),
                                                                         (L'.TFun (t', t), loc))
                                                                        
                                                     val t = foldr (fn (x, t) => (L'.TCFun (x, k, t), loc)) t xs
                                                     val e = foldr (fn (x, e) => (L'.ECAbs (x, k, e), loc)) e xs
                                                 in
                                                     (L'.DVal (x, n, t, e, ""), loc)
                                                 end) xncs
                        in
                            ((x, n, xs, xncs), (st, dcons' @ dcons))
                        end)
                (st, []) dts
        in
            ((L'.DDatatype dts, loc) :: dcons, st)
        end
      | L.DDatatypeImp (x, n, m1, ms, s, xs, xncs) =>
        let
            val (st, n) = St.bindCon st x n
            val c = corifyCon st (L.CModProj (m1, ms, s), loc)

            val m = foldl (fn (x, m) => (L.StrProj (m, x), loc)) (L.StrVar m1, loc) ms
            val (_, {inner, ...}) = corifyStr mods (m, st)

            val (xncs, st) = ListUtil.foldlMap (fn ((x, n, co), st) =>
                                                   let
                                                       val n' = St.lookupConstructorByName inner x
                                                       val st = St.bindConstructorAs st x n n'
                                                       val (st, n) = St.bindVal st x n
                                                       val co = Option.map (corifyCon st) co
                                                   in
                                                       ((x, n, co), st)
                                                   end) st xncs

            val nxs = length xs - 1
            val cBase = c
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
            ((L'.DCon (x, n, k', cBase), loc) :: cds, st)
        end
      | L.DVal (x, n, t, e as (L.ENamed n', _)) =>
        let
            val st =
                case St.lookupConstructorByIdOpt st n' of
                    SOME pc => St.bindConstructorAs st x n pc
                  | _ => st

            val (st, n) = St.bindVal st x n
            val s = doRestify Settings.Url (mods, x)
        in
            ([(L'.DVal (x, n, corifyCon st t, corifyExp st e, s), loc)], st)
        end
      | L.DVal (x, n, t, e) =>
        let
            val (st, n) = St.bindVal st x n
            val s = doRestify Settings.Url (mods, x)
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
                                  val s = doRestify Settings.Url (mods, x)
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

      | L.DStr (x, n, _, (L.StrProj (str, x'), _)) =>
        let
            val (ds, {inner, outer}) = corifyStr mods (str, st)

            val st = case St.lookupStrByNameOpt (x', inner) of
                SOME st' => St.bindStr st x n st'
              | NONE =>
                let
                    val (x', n', str') = St.lookupFunctorByName (x', inner)
                in
                    St.bindFunctor st x n x' n' str'
                end
        in
            ([], st)
        end

      | L.DStr (x, n, _, (L.StrVar n', _)) =>
        (case St.lookupFunctorByIdOpt st n' of
             SOME (arg, dom, body) => ([], St.bindFunctor st x n arg dom body)
           | NONE => ([], St.bindStr st x n (St.lookupStrById st n')))

      | L.DStr (x, n, _, str) =>
        let
            val mods' =
                if x = "anon" then
                    mods
                else
                    x :: mods

            val (ds, {inner, outer}) = corifyStr mods' (str, st)
            val st = St.bindStr outer x n inner
        in
            (ds, st)
        end

      | L.DFfiStr (m, n, (sgn, _)) =>
        (case sgn of
             L.SgnConst sgis =>
             let
                 val (ds, cmap, conmap, st, _) =
                     foldl (fn ((sgi, _), (ds, cmap, conmap, st, trans)) =>
                               case sgi of
                                   L.SgiConAbs (x, n, k) =>
                                   let
                                       val (st, n') = St.bindCon st x n

                                       val trans =
                                           if x = "transaction" then
                                               SOME n
                                           else
                                               trans
                                   in
                                       ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                        cmap,
                                        conmap,
                                        st,
                                        trans)
                                   end
                                 | L.SgiCon (x, n, k, _) =>
                                   let
                                       val (st, n') = St.bindCon st x n
                                   in
                                       ((L'.DCon (x, n', corifyKind k, (L'.CFfi (m, x), loc)), loc) :: ds,
                                        cmap,
                                        conmap,
                                        st,
                                        trans)
                                   end

                                 | L.SgiDatatype dts =>
                                   let
                                       val k = (L'.KType, loc)

                                       val (dts, (ds', st, cmap, conmap)) =
                                           ListUtil.foldlMap
                                               (fn ((x, n, xs, xnts), (ds', st, cmap, conmap)) =>
                                                   let
                                                       val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc))
                                                                      k xs

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
                                                                           foldr (fn (x, t) => (L'.TCFun (x, k, t), loc))
                                                                                 t xs
                                                                       fun wrapE e =
                                                                           foldr (fn (x, e) => (L'.ECAbs (x, k, e), loc))
                                                                                 e xs

                                                                       val (cmap, d) =
                                                                           case to of
                                                                               NONE => (SM.insert (cmap, x', wrapT dt),
                                                                                        (L'.DVal (x', n, wrapT dt,
                                                                                                  wrapE
                                                                                                      (L'.ECon (dk, pc,
                                                                                                                args,
                                                                                                                NONE),
                                                                                                       loc),
                                                                                                  ""), loc))
                                                                             | SOME t =>
                                                                               let
                                                                                   val tf = (L'.TFun (t, dt), loc)
                                                                                   val e = wrapE
                                                                                               (L'.EAbs ("x", t, tf,
                                                                                                         (L'.ECon (dk,
                                                                                                                   pc,
                                                                                                                   args,
                                                                                                                   SOME
                                                                                                                       (L'.ERel 0,
                                                                                                                        loc)),
                                                                                                          loc)), loc)
                                                                                   val d = (L'.DVal (x', n, wrapT tf,
                                                                                                     e, ""), loc)
                                                                               in
                                                                                   (SM.insert (cmap, x', wrapT tf), d)
                                                                               end

                                                                       val st = St.bindConstructorAs st x' n pc
                                                                                
                                                                       val conmap = SM.insert (conmap, x',
                                                                                               (x, xs, to, dk))
                                                                   in
                                                                       ((x', n, to),
                                                                        (d :: ds', st, cmap, conmap))
                                                                   end) (ds', st, cmap, conmap) xnts

                                                       val d = (L'.DCon (x, n', k', (L'.CFfi (m, x), loc)), loc)
                                                   in
                                                       ((x, n', xs, xnts), (d :: ds', st, cmap, conmap))
                                                   end)
                                           ([], st, cmap, conmap) dts
                                   in
                                       (List.revAppend (ds', ds),
                                        cmap,
                                        conmap,
                                        st,
                                        trans)
                                   end

                                 | L.SgiVal (x, _, c) =>
                                   let
                                       val c =
                                           case trans of
                                               NONE => corifyCon st c
                                             | SOME trans =>
                                               let
                                                   fun transactify (all as (c, loc)) =
                                                       case c of
                                                           L.TFun (dom, ran) =>
                                                           (L'.TFun (corifyCon st dom, transactify ran), loc)
                                                         | L.CApp ((L.CNamed trans', _), t) =>
                                                           if trans' = trans then
                                                               (L'.CApp ((L'.CFfi (m, "transaction"), loc),
                                                                        corifyCon st t), loc)
                                                           else
                                                               corifyCon st all
                                                         | _ => corifyCon st all
                                               in
                                                   transactify c
                                               end
                                   in
                                       if isTransactional c then
                                           let
                                               val ffi = (m, x)
                                           in
                                               if Settings.isBenignEffectful ffi then
                                                   ()
                                               else
                                                   Settings.addEffectful ffi
                                           end
                                       else
                                           ();
                                       (ds,
                                        SM.insert (cmap, x, c),
                                        conmap,
                                        st,
                                        trans)
                                   end
                                 | _ => (ds, cmap, conmap, st, trans))
                                                           ([], SM.empty, SM.empty, st, NONE) sgis

                 val st = St.bindStr st m n (St.ffi m cmap conmap)
             in
                 (rev ds, if m = "Basis" then St.basisIs (st, n) else st)
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
                         val basis_n = case St.lookupBasis st of
                                           NONE => raise Fail "Corify: Don't know number of Basis"
                                         | SOME n => n

                         fun wrapSgi ((sgi, _), (wds, eds))  =
                             case sgi of
                                 L.SgiVal (s, _, t) =>
                                 let
                                     fun getPage (t, args) =
                                         case #1 t of
                                             L.CApp ((L.CModProj (basis, [], "transaction"), _),
                                                     t' as
                                                        (L.CApp
                                                             ((L.CApp
                                                                   ((L.CApp ((L.CModProj (basis', [], "xml"), _),
                                                                             (L.CRecord (_, [((L.CName "Html", _),
                                                                                              _)]), _)), _), _),
                                                               _), _), _)) =>
                                             if basis = basis_n andalso basis' = basis_n then
                                                 SOME (t', rev args)
                                             else
                                                 NONE
                                           | L.TFun (dom, ran) => getPage (ran, dom :: args)
                                           | _ => NONE
                                 in
                                     case getPage (t, []) of
                                         NONE => (wds, eds)
                                       | SOME (ran', args) =>
                                         let
                                             val ran = (L.TRecord (L.CRecord ((L.KType, loc), []), loc), loc)
                                             val ranT = (L.CApp ((L.CModProj (basis_n, [], "transaction"), loc),
                                                                 ran), loc)
                                             val e = (L.EModProj (m, ms, s), loc)

                                             val ef = (L.EModProj (basis_n, [], "bind"), loc)
                                             val ef = (L.ECApp (ef, (L.CModProj (basis_n, [], "transaction"), loc)), loc)
                                             val ef = (L.ECApp (ef, ran'), loc)
                                             val ef = (L.ECApp (ef, ran), loc)
                                             val ef = (L.EApp (ef, (L.EModProj (basis_n, [], "transaction_monad"), loc)),
                                                       loc)
                                             val ea = ListUtil.foldri (fn (i, _, ea) =>
                                                                          (L.EApp (ea, (L.ERel i, loc)), loc)) e args
                                             val ef = (L.EApp (ef, ea), loc)

                                             val eat = (L.CApp ((L.CModProj (basis_n, [], "transaction"), loc),
                                                                ran), loc)
                                             val ea = (L.EAbs ("p", ran', eat,
                                                               (L.EWrite (L.ERel 0, loc), loc)), loc)

                                             val (e, tf) = ListUtil.foldri (fn (i, t, (e, tf)) =>
                                                                               ((L.EAbs ("x" ^ Int.toString i,
                                                                                         t, tf, e), loc),
                                                                                (L.TFun (t, tf), loc)))
                                                           ((L.EApp (ef, ea), loc), ranT) args

                                             val expKind = if List.exists (fn t =>
                                                                              case corifyCon st t of
                                                                                  (L'.CFfi ("Basis", "postBody"), _) => true
                                                                                | _ => false) args then
                                                               L'.Extern L'.ReadCookieWrite
                                                           else
                                                               L'.Link L'.ReadCookieWrite
                                         in
                                             ((L.DVal ("wrap_" ^ s, 0, tf, e), loc) :: wds,
                                              (fn st =>
                                                  case #1 (corifyExp st (L.EModProj (en, [], "wrap_" ^ s), loc)) of
                                                      L'.ENamed n => (L'.DExport (expKind, n, false), loc)
                                                    | _ => raise Fail "Corify: Value to export didn't corify properly")
                                              :: eds)
                                         end
                                 end
                               | _ => (wds, eds)

                         val (wds, eds) = foldl wrapSgi ([], []) sgis
                         val wrapper = (L.StrConst wds, loc)
                         val mst = St.lookupStrById st m
                         val mst = foldl St.lookupStrByName mst ms
                         val (ds, {inner, outer}) = corifyStr (St.name mst) (wrapper, st)
                         val st = St.bindStr outer "wrapper" en inner

                         val ds = ds @ map (fn f => f st) eds
                     in
                         (ds, st)
                     end
             end
           | _ => raise Fail "Non-const signature for 'export'")

      | L.DTable (_, x, n, c, pe, pc, ce, cc) =>
        let
            val (st, n) = St.bindVal st x n
            val s = relify (doRestify Settings.Table (mods, x))
        in
            ([(L'.DTable (x, n, corifyCon st c, s,
                          corifyExp st pe, corifyCon st pc,
                          corifyExp st ce, corifyCon st cc), loc)], st)
        end
      | L.DSequence (_, x, n) =>
        let
            val (st, n) = St.bindVal st x n
            val s = relify (doRestify Settings.Sequence (mods, x))
        in
            ([(L'.DSequence (x, n, s), loc)], st)
        end
      | L.DView (_, x, n, e, c) =>
        let
            val (st, n) = St.bindVal st x n
            val s = relify (doRestify Settings.View (mods, x))
        in
            ([(L'.DView (x, n, s, corifyExp st e, corifyCon st c), loc)], st)
        end
      | L.DIndex (e1, e2) => ([(L'.DIndex (corifyExp st e1, corifyExp st e2), loc)], st)

      | L.DDatabase s => ([(L'.DDatabase s, loc)], st)

      | L.DCookie (_, x, n, c) =>
        let
            val (st, n) = St.bindVal st x n
            val s = doRestify Settings.Cookie (mods, x)
        in
            ([(L'.DCookie (x, n, corifyCon st c, s), loc)], st)
        end
      | L.DStyle (_, x, n) =>
        let
            val (st, n) = St.bindVal st x n
            val s = relify (doRestify Settings.Style (mods, x))
        in
            ([(L'.DStyle (x, n, s), loc)], st)
        end

      | L.DTask (e1, e2) =>
        ([(L'.DTask (corifyExp st e1, corifyExp st e2), loc)], st)

      | L.DPolicy e1 =>
        ([(L'.DPolicy (corifyExp st e1), loc)], st)

      | L.DOnError (m, ms, x) =>
        let
            val st = St.lookupStrById st m
            val st = foldl St.lookupStrByName st ms
        in
            case St.lookupValByName st x of
                St.ENormal n => ([(L'.DOnError n, loc)], st)
              | _ => (ErrorMsg.errorAt loc "Wrong type of identifier for 'onError'";
                      ([], st))
        end

      | L.DFfi (x, n, modes, t) =>
        let
            val m = case St.name st of
                        [m] => m
                      | _ => (ErrorMsg.errorAt loc "Used 'ffi' declaration beneath module top level";
                              "")

            val name = (m, x)

            val (st, n) = St.bindVal st x n
            val s = doRestify Settings.Url (mods, x)

            val t' = corifyCon st t

            fun numArgs (t : L'.con) =
                case #1 t of
                    L'.TFun (_, ran) => 1 + numArgs ran
                  | _ => 0

            fun makeArgs (i, t : L'.con, acc) =
                case #1 t of
                    L'.TFun (dom, ran) => makeArgs (i-1, ran, ((L'.ERel i, loc), dom) :: acc)
                  | _ => rev acc

            fun wrapAbs (i, t : L'.con, tTrans, e) =
                case (#1 t, #1 tTrans) of
                    (L'.TFun (dom, ran), L'.TFun (_, ran')) => (L'.EAbs ("x" ^ Int.toString i, dom, ran, wrapAbs (i+1, ran, ran', e)), loc)
                  | _ => e

            fun getRan (t : L'.con) =
                case #1 t of
                    L'.TFun (_, ran) => getRan ran
                  | _ => t

            fun addLastBit (t : L'.con) =
                case #1 t of
                    L'.TFun (dom, ran) => (L'.TFun (dom, addLastBit ran), #2 t)
                  | _ => (L'.TFun ((L'.TRecord (L'.CRecord ((L'.KType, loc), []), loc), loc), t), loc)

            val isTrans = isTransactional t'
            val e = (L'.EFfiApp (m, x, makeArgs (numArgs t' -
                                               (if isTrans then
                                                    0
                                                else
                                                    1), t', [])), loc)
            val (e, tTrans) = if isTrans then
                                  ((L'.EAbs ("_", (L'.TRecord (L'.CRecord ((L'.KType, loc), []), loc), loc), getRan t', e), loc), addLastBit t')
                              else
                                  (e, t')
            val e = wrapAbs (0, t', tTrans, e)
        in
            app (fn Source.Effectful => Settings.addEffectful name
                  | Source.BenignEffectful => Settings.addBenignEffectful name
                  | Source.ClientOnly => Settings.addClientOnly name
                  | Source.ServerOnly => Settings.addServerOnly name
                  | Source.JsFunc s => Settings.addJsFunc (name, s)) modes;

            if List.exists (fn Source.JsFunc _ => true | _ => false) modes then
                ()
            else
                Settings.addJsFunc (name, #2 name);

            if isTrans andalso not (Settings.isBenignEffectful name) then
                Settings.addEffectful name
            else
                ();

            ([(L'.DVal (x, n, t', e, s), loc)], st)
        end

and corifyStr mods ((str, loc), st) =
    case str of
        L.StrConst ds =>
        let
            val st = St.enter (st, mods)
            val (ds, st) = ListUtil.foldlMapConcat (corifyDecl mods) st ds
        in
            (ds, St.leave st)
        end
      | L.StrVar n => ([], {inner = St.lookupStrById st n, outer = st})
      | L.StrProj (str, x) =>
        let
            val (ds, {inner, outer}) = corifyStr mods (str, st)
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

            (* An important step to make sure that nested functors
             * "close under their environments": *)
            val (next, body') = ExplRename.rename {NextId = getCounter (),
                                                   FormalName = xa,
                                                   FormalId = na,
                                                   Body = body}

            (*val () = Print.prefaces ("RENAME " ^ ErrorMsg.spanToString loc)
                     [("FROM", ExplPrint.p_str ExplEnv.empty body),
                      ("TO", ExplPrint.p_str ExplEnv.empty body')]*)
            val body = body'

            val () = setCounter next

            val (ds1, {inner = inner', outer}) = corifyStr mods (str2, st)

            val (ds2, {inner, outer}) = corifyStr mods (body, St.bindStr outer xa na inner')
        in
            (ds1 @ ds2, {inner = St.bindStr inner xa na inner', outer = outer})
        end

fun maxName ds = foldl (fn ((d, _), n) =>
                           case d of
                               L.DCon (_, n', _, _) => Int.max (n, n')
                             | L.DDatatype dts => foldl (fn ((_, n', _, _), n) => Int.max (n, n')) n dts
                             | L.DDatatypeImp (_, n', _, _, _, _, _) => Int.max (n, n')
                             | L.DVal (_, n', _, _) => Int.max (n, n')
                             | L.DValRec vis => foldl (fn ((_, n', _, _), n) => Int.max (n, n)) n vis
                             | L.DSgn (_, n', _) => Int.max (n, n')
                             | L.DStr (_, n', _, str) => Int.max (n, Int.max (n', maxNameStr str))
                             | L.DFfiStr (_, n', _) => Int.max (n, n')
                             | L.DExport _ => n
                             | L.DTable (_, _, n', _, _, _, _, _) => Int.max (n, n')
                             | L.DSequence (_, _, n') => Int.max (n, n')
                             | L.DView (_, _, n', _, _) => Int.max (n, n')
                             | L.DIndex _ => n
                             | L.DDatabase _ => n
                             | L.DCookie (_, _, n', _) => Int.max (n, n')
                             | L.DStyle (_, _, n') => Int.max (n, n')
                             | L.DTask _ => n
                             | L.DPolicy _ => n
                             | L.DOnError _ => n
                             | L.DFfi (_, n', _, _) => Int.max (n, n'))
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

        val (ds, _) = ListUtil.foldlMapConcat (corifyDecl []) St.empty ds
    in
        ds
    end

end
