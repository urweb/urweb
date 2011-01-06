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

structure ESpecialize :> ESPECIALIZE = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

type skey = exp

structure K = struct
type ord_key = con list * exp list
fun compare ((cs1, es1), (cs2, es2)) = Order.join (Order.joinL U.Con.compare (cs1, cs2),
                                                   fn () => Order.joinL U.Exp.compare (es1, es2))
end

structure KM = BinaryMapFn(K)
structure IM = IntBinaryMap
structure IS = IntBinarySet

val freeVars = U.Exp.foldB {kind = fn (_, _, xs) => xs,
                            con = fn (_, _, xs) => xs,
                            exp = fn (bound, e, xs) =>
                                     case e of
                                         ERel x =>
                                         if x >= bound then
                                             IS.add (xs, x - bound)
                                         else
                                             xs
                                       | _ => xs,
                            bind = fn (bound, b) =>
                                      case b of
                                          U.Exp.RelE _ => bound + 1
                                        | _ => bound}
                           0 IS.empty

fun isPolyT (t, _) =
    case t of
        TFun (_, ran) => isPolyT ran
      | TCFun _ => true
      | TKFun _ => true
      | _ => false

fun isPoly (d, _) =
    case d of
        DVal (_, _, t, _, _) => isPolyT t
      | DValRec vis => List.exists (isPolyT o #3) vis
      | _ => false

fun positionOf (v : int, ls) =
    let
        fun pof (pos, ls) =
            case ls of
                [] => raise Fail "Defunc.positionOf"
              | v' :: ls' =>
                if v = v' then
                    pos
                else
                    pof (pos + 1, ls')
    in
        pof (0, ls)
    end

fun squish fvs =
    U.Exp.mapB {kind = fn _ => fn k => k,
                con = fn _ => fn c => c,
                exp = fn bound => fn e =>
                                     case e of
                                         ERel x =>
                                         if x >= bound then
                                             ERel (positionOf (x - bound, fvs) + bound)
                                         else
                                             e
                                       | _ => e,
                bind = fn (bound, b) =>
                          case b of
                              U.Exp.RelE _ => bound + 1
                            | _ => bound}
               0

type func = {
     name : string,
     args : int KM.map,
     body : exp,
     typ : con,
     tag : string
}

type state = {
     maxName : int,
     funcs : func IM.map,
     decls : (string * int * con * exp * string) list,
     specialized : IS.set
}

fun default (_, x, st) = (x, st)

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val mayNotSpec = ref SS.empty

val functionInside = U.Con.exists {kind = fn _ => false,
                                   con = fn TFun _ => true
                                          | CFfi ("Basis", "transaction") => true
                                          | CFfi ("Basis", "eq") => true
                                          | CFfi ("Basis", "num") => true
                                          | CFfi ("Basis", "ord") => true
                                          | CFfi ("Basis", "show") => true
                                          | CFfi ("Basis", "read") => true
                                          | CFfi ("Basis", "sql_injectable_prim") => true
                                          | CFfi ("Basis", "sql_injectable") => true
                                          | _ => false}

fun specialize' (funcs, specialized) file =
    let
        fun bind (env, b) =
            case b of
                U.Decl.RelE xt => xt :: env
              | _ => env

        fun exp (env, e as (_, loc), st : state) =
            let
                (*val () = Print.prefaces "exp" [("e", CorePrint.p_exp CoreEnv.empty
                                                                     (e, ErrorMsg.dummySpan))]*)

                fun getApp (e, _) =
                    case e of
                        ENamed f => SOME (f, [])
                      | EApp (e1, e2) =>
                        (case getApp e1 of
                             NONE => NONE
                           | SOME (f, xs) => SOME (f, xs @ [e2]))
                      | _ => NONE

                val getApp = fn e => case getApp e of
                                         v as SOME (_, _ :: _) => v
                                       | _ => NONE

                fun default () =
                    case #1 e of
                        EPrim _ => (e, st)
                      | ERel _ => (e, st)
                      | ENamed _ => (e, st)
                      | ECon (_, _, _, NONE) => (e, st)
                      | ECon (dk, pc, cs, SOME e) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((ECon (dk, pc, cs, SOME e), loc), st)
                        end
                      | EFfi _ => (e, st)
                      | EFfiApp (m, x, es) =>
                        let
                            val (es, st) = ListUtil.foldlMap (fn (e, st) => exp (env, e, st)) st es
                        in
                            ((EFfiApp (m, x, es), loc), st)
                        end
                      | EApp (e1, e2) =>
                        let
                            val (e1, st) = exp (env, e1, st)
                            val (e2, st) = exp (env, e2, st)
                        in
                            ((EApp (e1, e2), loc), st)
                        end
                      | EAbs (x, d, r, e) =>
                        let
                            val (e, st) = exp ((x, d) :: env, e, st)
                        in
                            ((EAbs (x, d, r, e), loc), st)
                        end
                      | ECApp (e, c) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((ECApp (e, c), loc), st)
                        end
                      | ECAbs _ => (e, st)
                      | EKAbs _ => (e, st)
                      | EKApp (e, k) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((EKApp (e, k), loc), st)
                        end
                      | ERecord fs =>
                        let
                            val (fs, st) = ListUtil.foldlMap (fn ((c1, e, c2), st) =>
                                                                 let
                                                                     val (e, st) = exp (env, e, st)
                                                                 in
                                                                     ((c1, e, c2), st)
                                                                 end) st fs
                        in
                            ((ERecord fs, loc), st)
                        end
                      | EField (e, c, cs) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((EField (e, c, cs), loc), st)
                        end
                      | EConcat (e1, c1, e2, c2) =>
                        let
                            val (e1, st) = exp (env, e1, st)
                            val (e2, st) = exp (env, e2, st)
                        in
                            ((EConcat (e1, c1, e2, c2), loc), st)
                        end
                      | ECut (e, c, cs) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((ECut (e, c, cs), loc), st)
                        end
                      | ECutMulti (e, c, cs) =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((ECutMulti (e, c, cs), loc), st)
                        end

                      | ECase (e, pes, cs) =>
                        let
                            val (e, st) = exp (env, e, st)
                            val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                                  let
                                                                      val (e, st) = exp (E.patBindsL p @ env, e, st)
                                                                  in
                                                                      ((p, e), st)
                                                                  end) st pes
                        in
                            ((ECase (e, pes, cs), loc), st)
                        end

                      | EWrite e =>
                        let
                            val (e, st) = exp (env, e, st)
                        in
                            ((EWrite e, loc), st)
                        end
                      | EClosure (n, es) =>
                        let
                            val (es, st) = ListUtil.foldlMap (fn (e, st) => exp (env, e, st)) st es
                        in
                            ((EClosure (n, es), loc), st)
                        end
                      | ELet (x, t, e1, e2) =>
                        let
                            val (e1, st) = exp (env, e1, st)
                            val (e2, st) = exp ((x, t) :: env, e2, st)
                        in
                            ((ELet (x, t, e1, e2), loc), st)
                        end
                      | EServerCall (n, es, t) =>
                        let
                            val (es, st) = ListUtil.foldlMap (fn (e, st) => exp (env, e, st)) st es
                        in
                            ((EServerCall (n, es, t), loc), st)
                        end
            in
                case getApp e of
                    NONE => default ()
                  | SOME (f, xs) =>
                    case IM.find (#funcs st, f) of
                        NONE => ((*print ("No find: " ^ Int.toString f ^ "\n");*) default ())
                      | SOME {name, args, body, typ, tag} =>
                        let
                            val (xs, st) = ListUtil.foldlMap (fn (e, st) => exp (env, e, st)) st xs

                            (*val () = Print.prefaces "Consider" [("e", CorePrint.p_exp CoreEnv.empty
                                                                                      (e, ErrorMsg.dummySpan))]*)

                            val loc = ErrorMsg.dummySpan

                            fun findSplit av (xs, typ, fxs, fvs, fin) =
                                case (#1 typ, xs) of
                                    (TFun (dom, ran), e :: xs') =>
                                    let
                                        val av = case #1 e of
                                                     ERel _ => av
                                                   | _ => false
                                    in
                                        if functionInside dom orelse (av andalso case #1 e of
                                                                                     ERel _ => true
                                                                                   | _ => false) then
                                            findSplit av (xs',
                                                          ran,
                                                          e :: fxs,
                                                          IS.union (fvs, freeVars e),
                                                          fin orelse functionInside dom)
                                        else
                                            (rev fxs, xs, fvs, fin)
                                    end
                                  | _ => (rev fxs, xs, fvs, fin)

                            val (fxs, xs, fvs, fin) = findSplit true (xs, typ, [], IS.empty, false)

                            fun valueish (all as (e, _)) =
                                case e of
                                    EPrim _ => true
                                  | ERel _ => true
                                  | ENamed _ => true
                                  | ECon (_, _, _, NONE) => true
                                  | ECon (_, _, _, SOME e) => valueish e
                                  | EFfi (_, _) => true
                                  | EAbs _ => true
                                  | ECAbs _ => true
                                  | EKAbs _ => true
                                  | ECApp (e, _) => valueish e
                                  | EKApp (e, _) => valueish e
                                  | EApp (e1, e2) => valueish e1 andalso valueish e2
                                  | ERecord xes => List.all (valueish o #2) xes
                                  | EField (e, _, _) => valueish e
                                  | _ => false

                            val vts = map (fn n => #2 (List.nth (env, n))) (IS.listItems fvs)
                            val fxs' = map (squish (IS.listItems fvs)) fxs

                            val p_bool = Print.PD.string o Bool.toString
                        in
                            (*Print.prefaces "Func" [("name", Print.PD.string name),
                                                   ("e", CorePrint.p_exp CoreEnv.empty e),
                                                   ("fxs'", Print.p_list (CorePrint.p_exp CoreEnv.empty) fxs')];*)
                            if not fin
                               orelse List.all (fn (ERel _, _) => true
                                                 | _ => false) fxs'
                               orelse List.exists (not o valueish) fxs'
                               orelse (IS.numItems fvs >= length fxs
                                       andalso IS.exists (fn n => functionInside (#2 (List.nth (env, n)))) fvs) then
                                ((*Print.prefaces "No" [("name", Print.PD.string name),
                                                      ("f", Print.PD.string (Int.toString f)),
                                                      ("fxs'",
                                                       Print.p_list (CorePrint.p_exp CoreEnv.empty) fxs'),
                                                      ("b1", p_bool (not fin)),
                                                      ("b2", p_bool (List.all (fn (ERel _, _) => true
                                                                                | _ => false) fxs')),
                                                      ("b3", p_bool (List.exists (not o valueish) fxs')),
                                                      ("b4", p_bool (IS.numItems fvs >= length fxs
                                                                     andalso IS.exists (fn n => functionInside (#2 (List.nth (env, n)))) fvs))];*)
                                 default ())
                            else
                                case (KM.find (args, (vts, fxs')),
                                      SS.member (!mayNotSpec, name) (*orelse IS.member (#specialized st, f)*)) of
                                    (SOME f', _) =>
                                    let
                                        val e = (ENamed f', loc)
                                        val e = IS.foldr (fn (arg, e) => (EApp (e, (ERel arg, loc)), loc))
                                                         e fvs
                                        val e = foldl (fn (arg, e) => (EApp (e, arg), loc))
                                                      e xs
                                    in
                                        (*Print.prefaces "Brand new (reuse)"
                                                       [("e'", CorePrint.p_exp CoreEnv.empty e)];*)
                                        (e, st)
                                    end
                                  | (_, true) => ((*Print.prefaces ("No!(" ^ name ^ ")")
                                                                 [("fxs'",
                                                                   Print.p_list (CorePrint.p_exp CoreEnv.empty) fxs')];*)
                                                  default ())
                                  | (NONE, false) =>
                                    let
                                        (*val () = Print.prefaces "New one"
                                                 [("f", Print.PD.string (Int.toString f)),
                                                  ("mns", Print.p_list Print.PD.string
                                                                       (SS.listItems (!mayNotSpec)))]*)

                                        (*val () = Print.prefaces ("Yes(" ^ name ^ ")")
                                                                [("fxs'",
                                                                  Print.p_list (CorePrint.p_exp CoreEnv.empty) fxs')]*)

                                        fun subBody (body, typ, fxs') =
                                            case (#1 body, #1 typ, fxs') of
                                                (_, _, []) => SOME (body, typ)
                                              | (EAbs (_, _, _, body'), TFun (_, typ'), x :: fxs'') =>
                                                let
                                                    val body'' = E.subExpInExp (0, x) body'
                                                in
                                                    subBody (body'',
                                                             typ',
                                                             fxs'')
                                                end
                                              | _ => NONE
                                    in
                                        case subBody (body, typ, fxs') of
                                            NONE => default ()
                                          | SOME (body', typ') =>
                                            let
                                                val f' = #maxName st
                                                val args = KM.insert (args, (vts, fxs'), f')
                                                val funcs = IM.insert (#funcs st, f, {name = name,
                                                                                      args = args,
                                                                                      body = body,
                                                                                      typ = typ,
                                                                                      tag = tag})

                                                val st = {
                                                    maxName = f' + 1,
                                                    funcs = funcs,
                                                    decls = #decls st,
                                                    specialized = IS.add (#specialized st, f')
                                                }

                                                (*val () = Print.prefaces "specExp"
                                                                        [("f", CorePrint.p_exp env (ENamed f, loc)),
                                                                         ("f'", CorePrint.p_exp env (ENamed f', loc)),
                                                                         ("xs", Print.p_list (CorePrint.p_exp env) xs),
                                                                         ("fxs'", Print.p_list
                                                                                      (CorePrint.p_exp E.empty) fxs'),
                                                                         ("e", CorePrint.p_exp env (e, loc))]*)
                                                val (body', typ') = IS.foldl (fn (n, (body', typ')) =>
                                                                                 let
                                                                                     val (x, xt) = List.nth (env, n)
                                                                                 in
                                                                                     ((EAbs (x, xt, typ', body'),
                                                                                       loc),
                                                                                      (TFun (xt, typ'), loc))
                                                                                 end)
                                                                             (body', typ') fvs
                                                val mns = !mayNotSpec
                                                (*val () = mayNotSpec := SS.add (mns, name)*)
                                                (*val () = print ("NEW: " ^ name ^ "__" ^ Int.toString f' ^ "\n");*)
                                                val body' = ReduceLocal.reduceExp body'
                                                (*val () = Print.preface ("PRE", CorePrint.p_exp CoreEnv.empty body')*)
                                                val (body', st) = exp (env, body', st)
                                                val () = mayNotSpec := mns

                                                val e' = (ENamed f', loc)
                                                val e' = IS.foldr (fn (arg, e) => (EApp (e, (ERel arg, loc)), loc))
                                                                  e' fvs
                                                val e' = foldl (fn (arg, e) => (EApp (e, arg), loc))
                                                               e' xs

                                                (*val () = Print.prefaces "Brand new"
                                                                        [("e'", CorePrint.p_exp CoreEnv.empty e'),
                                                                         ("e", CorePrint.p_exp CoreEnv.empty e),
                                                                         ("body'", CorePrint.p_exp CoreEnv.empty body')]*)
                                            in
                                                (e',
                                                 {maxName = #maxName st,
                                                  funcs = #funcs st,
                                                  decls = (name, f', typ', body', tag) :: #decls st,
                                                  specialized = #specialized st})
                                            end
                                    end
                        end
            end

        fun doDecl (d, (st : state, changed)) =
            let
                (*val befor = Time.now ()*)

                val funcs = #funcs st
                val funcs = 
                    case #1 d of
                        DValRec vis =>
                        foldl (fn ((x, n, c, e, tag), funcs) =>
                                  IM.insert (funcs, n, {name = x,
                                                        args = KM.empty,
                                                        body = e,
                                                        typ = c,
                                                        tag = tag}))
                              funcs vis
                      | _ => funcs

                val st = {maxName = #maxName st,
                          funcs = funcs,
                          decls = [],
                          specialized = #specialized st}

                (*val () = Print.prefaces "decl" [("d", CorePrint.p_decl CoreEnv.empty d)]*)

                val () = mayNotSpec := SS.empty

                val (d', st) =
                    if isPoly d then
                        (d, st)
                    else
                        case #1 d of
                            DVal (x, n, t, e, s) =>
                            let
                                (*val () = Print.preface ("Visiting", Print.box [Print.PD.string (x ^ "__" ^ Int.toString n),
                                                                                                Print.space,
                                                                                                Print.PD.string ":",
                                                                                                Print.space,
                                                                                                CorePrint.p_con CoreEnv.empty t])*)

                                val (e, st) = exp ([], e, st)
                            in
                                ((DVal (x, n, t, e, s), #2 d), st)
                            end
                          | DValRec vis =>
                            let
                                (*val () = Print.preface ("Visiting", Print.p_list (fn vi =>
                                                                                     Print.box [Print.PD.string (#1 vi ^ "__"
                                                                                                                 ^ Int.toString
                                                                                                                       (#2 vi)),
                                                                                                Print.space,
                                                                                                Print.PD.string ":",
                                                                                                Print.space,
                                                                                                CorePrint.p_con CoreEnv.empty (#3 vi)])
                                                                                 vis)*)

                                val (vis, st) = ListUtil.foldlMap (fn ((x, n, t, e, s), st) =>
                                                                      let
                                                                          val () = mayNotSpec := SS.empty
                                                                          val (e, st) = exp ([], e, st)
                                                                      in
                                                                          ((x, n, t, e, s), st)
                                                                      end) st vis
                            in
                                ((DValRec vis, #2 d), st)
                            end
                          | DTable (s, n, t, s1, e1, t1, e2, t2) =>
                            let
                                val (e1, st) = exp ([], e1, st)
                                val (e2, st) = exp ([], e2, st)
                            in
                                ((DTable (s, n, t, s1, e1, t2, e2, t2), #2 d), st)
                            end
                          | DView (x, n, s, e, t) =>
                            let
                                val (e, st) = exp ([], e, st)
                            in
                                ((DView (x, n, s, e, t), #2 d), st)
                            end
                          | DTask (e1, e2) =>
                            let
                                val (e1, st) = exp ([], e1, st)
                                val (e2, st) = exp ([], e2, st)
                            in
                                ((DTask (e1, e2), #2 d), st)
                            end
                          | _ => (d, st)

                val () = mayNotSpec := SS.empty

                (*val () = print "/decl\n"*)

                val funcs = #funcs st
                val funcs =
                    case #1 d of
                        DVal (x, n, c, e as (EAbs _, _), tag) =>
                        IM.insert (funcs, n, {name = x,
                                              args = KM.empty,
                                              body = e,
                                              typ = c,
                                              tag = tag})
                      | DVal (_, n, _, (ENamed n', _), _) =>
                        (case IM.find (funcs, n') of
                             NONE => funcs
                           | SOME v => IM.insert (funcs, n, v))
                      | _ => funcs

                val (changed, ds) =
                    case #decls st of
                        [] => (changed, [d'])
                      | vis =>
                        (true, case d' of
                                   (DValRec vis', _) => [(DValRec (vis @ vis'), ErrorMsg.dummySpan)]
                                 | _ => [(DValRec vis, ErrorMsg.dummySpan), d'])
            in
                (*Print.prefaces "doDecl" [("d", CorePrint.p_decl E.empty d),
                                         ("d'", CorePrint.p_decl E.empty d')];*)
                (ds, ({maxName = #maxName st,
                       funcs = funcs,
                       decls = [],
                       specialized = #specialized st}, changed))
            end

        (*val () = Print.preface ("RESET", CorePrint.p_file CoreEnv.empty file)*)
        val (ds, (st, changed)) = ListUtil.foldlMapConcat doDecl
                                                            ({maxName = U.File.maxName file + 1,
                                                              funcs = funcs,
                                                              decls = [],
                                                              specialized = specialized},
                                                             false)
                                                            file
    in
        (*print ("Changed = " ^ Bool.toString changed ^ "\n");*)
        (changed, ds, #funcs st, #specialized st)
    end

fun specializeL (funcs, specialized) file =
    let
        val file = ReduceLocal.reduce file
        (*val file = ReduceLocal.reduce file*)
        val (changed, file, funcs, specialized) = specialize' (funcs, specialized) file
        (*val file = ReduceLocal.reduce file
        val file = CoreUntangle.untangle file
        val file = Shake.shake file*)
    in
        (*print "Round over\n";*)
        if changed then
            let
                (*val file = ReduceLocal.reduce file*)
                (*val () = Print.prefaces "Pre-untangle" [("file", CorePrint.p_file CoreEnv.empty file)]*)
                val file = CoreUntangle.untangle file
                (*val () = Print.prefaces "Post-untangle" [("file", CorePrint.p_file CoreEnv.empty file)]*)
                val file = Shake.shake file
            in
                (*print "Again!\n";*)
                (*Print.prefaces "Again" [("file", CorePrint.p_file CoreEnv.empty file)];*)
                specializeL (funcs, specialized) file
            end
        else
            file
    end

val specialize = specializeL (IM.empty, IS.empty)

end
