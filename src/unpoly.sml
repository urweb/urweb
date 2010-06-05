(* Copyright (c) 2008-2010, Adam Chlipala
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

(* Simplify a Core program by repeating polymorphic function definitions *)

structure Unpoly :> UNPOLY = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

structure IS = IntBinarySet
structure IM = IntBinaryMap


(** The actual specialization *)

val liftConInCon = E.liftConInCon
val subConInCon = E.subConInCon

val liftConInExp = E.liftConInExp
val subConInExp = E.subConInExp

val isOpen = U.Con.existsB {kind = fn _ => false,
                            con = fn (n, c) =>
                                     case c of
                                         CRel n' => n' >= n
                                       | _ => false,
                            bind = fn (n, b) =>
                                      case b of
                                          U.Con.RelC _ => n + 1
                                        | _ => n} 0

fun unpolyNamed (xn, rep) =
    U.Exp.map {kind = fn k => k,
               con = fn c => c,
               exp = fn e =>
                        case e of
                            ECApp (e', _) =>
                            let
                                fun isTheOne (e, _) =
                                    case e of
                                        ENamed xn' => xn' = xn
                                      | ECApp (e, _) => isTheOne e
                                      | _ => false
                            in
                                if isTheOne e' then
                                    rep
                                else
                                    e
                            end
                          | _ => e}

structure M = BinaryMapFn(struct
                          type ord_key = con list
                          val compare = Order.joinL U.Con.compare
                          end)

type func = {
     kinds : kind list,
     defs : (string * int * con * exp * string) list,
     replacements : int M.map
}

type state = {
     funcs : func IM.map,
     decls : decl list,
     nextName : int
}

fun kind (k, st) = (k, st)

fun con (c, st) = (c, st)

fun exp (e, st : state) =
    case e of
        ECApp _ =>
        let
            fun unravel (e, cargs) =
                case e of
                    ECApp ((e, _), c) => unravel (e, c :: cargs)
                  | ENamed n => SOME (n, rev cargs)
                  | _ => NONE
        in
            case unravel (e, []) of
                NONE => (e, st)
              | SOME (n, cargs) =>
                if List.exists isOpen cargs then
                    (e, st)
                else
                    case IM.find (#funcs st, n) of
                        NONE => (e, st)
                      | SOME {kinds = ks, defs = vis, replacements} =>
                        let
                            val cargs = map ReduceLocal.reduceCon cargs
                        in
                            case M.find (replacements, cargs) of
                                SOME n => (ENamed n, st)
                              | NONE =>
                                let
                                    val old_vis = vis
                                    val (vis, (thisName, nextName)) =
                                        ListUtil.foldlMap
                                            (fn ((x, n', t, e, s), (thisName, nextName)) =>
                                                ((x, nextName, n', t, e, s),
                                                 (if n' = n then nextName else thisName,
                                                  nextName + 1)))
                                            (0, #nextName st) vis

                                    fun specialize (x, n, n_old, t, e, s) =
                                        let
                                            fun trim (t, e, cargs) =
                                                case (t, e, cargs) of
                                                    ((TCFun (_, _, t), _),
                                                     (ECAbs (_, _, e), _),
                                                     carg :: cargs) =>
                                                    let
                                                        val t = subConInCon (length cargs, carg) t
                                                        val e = subConInExp (length cargs, carg) e
                                                    in
                                                        trim (t, e, cargs)
                                                    end
                                                  | (_, _, []) => SOME (t, e)
                                                  | _ => NONE
                                        in
                                            (*Print.prefaces "specialize"
                                                             [("n", Print.PD.string (Int.toString n)),
                                                              ("nold", Print.PD.string (Int.toString n_old)),
                                                              ("t", CorePrint.p_con CoreEnv.empty t),
                                                              ("e", CorePrint.p_exp CoreEnv.empty e),
                                                              ("|cargs|", Print.PD.string (Int.toString (length cargs)))];*)
                                            Option.map (fn (t, e) => (x, n, n_old, t, e, s))
                                                       (trim (t, e, cargs))
                                        end

                                    val vis = List.map specialize vis
                                in
                                    if List.exists (not o Option.isSome) vis orelse length cargs > length ks then
                                        (e, st)
                                    else
                                        let
                                            val vis = List.mapPartial (fn x => x) vis

                                            val vis = map (fn (x, n, n_old, t, e, s) =>
                                                              (x ^ "_unpoly", n, n_old, t, e, s)) vis
                                            val vis' = map (fn (x, n, _, t, e, s) =>
                                                               (x, n, t, e, s)) vis

                                            val funcs = foldl (fn ((_, n, n_old, _, _, _), funcs) =>
                                                                  let
                                                                      val replacements = case IM.find (funcs, n_old) of
                                                                                             NONE => M.empty
                                                                                           | SOME {replacements = r,
                                                                                                   ...} => r
                                                                  in
                                                                      IM.insert (funcs, n_old,
                                                                                 {kinds = ks,
                                                                                  defs = old_vis,
                                                                                  replacements = M.insert (replacements,
                                                                                                           cargs,
                                                                                                           n)})
                                                                  end) (#funcs st) vis

                                            val ks' = List.drop (ks, length cargs)

                                            val st = {funcs = foldl (fn (vi, funcs) =>
                                                                        IM.insert (funcs, #2 vi,
                                                                                   {kinds = ks',
                                                                                    defs = vis',
                                                                                    replacements = M.empty}))
                                                                    funcs vis',
                                                      decls = #decls st,
                                                      nextName = nextName}

                                            val (vis', st) = ListUtil.foldlMap (fn ((x, n, t, e, s), st) =>
                                                                                   let
                                                                                       val (e, st) = polyExp (e, st)
                                                                                   in
                                                                                       ((x, n, t, e, s), st)
                                                                                   end)
                                                                               st vis'
                                        in
                                            (ENamed thisName,
                                             {funcs = #funcs st,
                                              decls = (DValRec vis', ErrorMsg.dummySpan) :: #decls st,
                                              nextName = #nextName st})
                                        end
                                end
                        end
        end
      | _ => (e, st)

and polyExp (x, st) = U.Exp.foldMap {kind = kind, con = con, exp = exp} st x

fun decl (d, st : state) =
    let
        fun unravel (e, cargs) =
            case e of
                (ECAbs (_, k, e), _) =>
                unravel (e, k :: cargs)
              | _ => rev cargs
    in
        case d of
            DVal (vi as (x, n, t, e, s)) =>
            let
                val cargs = unravel (e, [])

                val ns = IS.singleton n
            in
                (d, {funcs = IM.insert (#funcs st, n, {kinds = cargs,
                                                       defs = [vi],
                                                       replacements = M.empty}),
                     decls = #decls st,
                     nextName = #nextName st})
            end
          | DValRec (vis as ((x, n, t, e, s) :: rest)) =>
            let
                val cargs = unravel (e, [])

                fun unravel (e, cargs) =
                    case (e, cargs) of
                        ((ECAbs (_, k, e), _), k' :: cargs) =>
                        U.Kind.compare (k, k') = EQUAL
                        andalso unravel (e, cargs)
                      | (_, []) => true
                      | _ => false
                             
                fun deAbs (e, cargs) =
                    case (e, cargs) of
                        ((ECAbs (_, _, e), _), _ :: cargs) => deAbs (e, cargs)
                      | (_, []) => e
                      | _ => raise Fail "Unpoly: deAbs"

            in
                if List.exists (fn vi => not (unravel (#4 vi, cargs))) rest then
                    (d, st)
                else
                    let
                        val ns = IS.addList (IS.empty, map #2 vis)
                        val nargs = length cargs

                        (** Verifying lack of polymorphic recursion *)

                        fun kind _ = false
                        fun con _ = false

                        fun exp (cn, e) =
                            case e of
                                orig as ECApp (e, c) =>
                                let
                                    fun isIrregular (e, pos) =
                                        case #1 e of
                                            ENamed n =>
                                            IS.member (ns, n)
                                            andalso
                                            (case #1 c of
                                                 CRel i => i <> nargs - pos + cn
                                               | _ => true)
                                          | ECApp (e, _) => isIrregular (e, pos + 1)
                                          | _ => false
                                in
                                    isIrregular (e, 1)
                                end
                              | _ => false

                        fun bind (cn, b) =
                            case b of
                                U.Exp.RelC _ => cn+1
                              | _ => cn

                        val irregular = U.Exp.existsB {kind = kind, con = con, exp = exp, bind = bind} 0
                    in
                        if List.exists (fn x => irregular (deAbs (#4 x, cargs))) vis then
                            (d, st)
                        else
                            (d, {funcs = foldl (fn (vi, funcs) =>
                                                   IM.insert (funcs, #2 vi, {kinds = cargs,
                                                                             defs = vis,
                                                                             replacements = M.empty}))
                                               (#funcs st) vis,
                                 decls = #decls st,
                                 nextName = #nextName st})
                    end
            end

          | _ => (d, st)
    end

val polyDecl = U.Decl.foldMap {kind = kind, con = con, exp = exp, decl = decl}

fun unpoly file =
    let
        fun doDecl (d : decl, st : state) =
            let
                val (d, st) = polyDecl st d
            in
                (rev (d :: #decls st),
                 {funcs = #funcs st,
                  decls = [],
                  nextName = #nextName st})
            end

        val (ds, _) = ListUtil.foldlMapConcat doDecl
                      {funcs = IM.empty,
                       decls = [],
                       nextName = U.File.maxName file + 1} file
    in
        ds
    end

end
