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

(* Simplify a Mono program algebraically *)

structure MonoReduce :> MONO_REDUCE = struct

open Mono

structure E = MonoEnv
structure U = MonoUtil

structure IM = IntBinaryMap
structure IS = IntBinarySet


fun simpleTypeImpure tsyms =
    U.Typ.exists (fn TFun _ => true
                   | TDatatype (n, _) => IS.member (tsyms, n)
                   | _ => false)

fun simpleImpure (tsyms, syms) =
    U.Exp.existsB {typ = fn _ => false,
                   exp = fn (env, e) =>
                            case e of
                                EWrite _ => true
                              | EQuery _ => true
                              | EDml _ => true
                              | ENextval _ => true
                              | ESetval _ => true
                              | EFfiApp (m, x, _) => Settings.isEffectful (m, x) orelse Settings.isBenignEffectful (m, x)
                              | EServerCall _ => true
                              | ERecv _ => true
                              | ESleep _ => true
                              | ENamed n => IS.member (syms, n)
                              | ERel n =>
                                let
                                    val (_, t, _) = E.lookupERel env n
                                in
                                    simpleTypeImpure tsyms t
                                end
                              | _ => false,
                   bind = fn (env, b) =>
                             case b of
                                 U.Exp.RelE (x, t) => E.pushERel env x t NONE
                               | _ => env}

fun impure (e, _) =
    case e of
        EWrite _ => true
      | EQuery _ => true
      | EDml _ => true
      | ENextval _ => true
      | ESetval _ => true
      | EUnurlify (e, _, _) => impure e
      | EAbs _ => false

      | EPrim _ => false
      | ERel _ => false
      | ENamed _ => false
      | ECon (_, _, eo) => (case eo of NONE => false | SOME e => impure e)
      | ENone _ => false
      | ESome (_, e) => impure e
      | EFfi _ => false
      | EFfiApp (m, x, _) => Settings.isEffectful (m, x) orelse Settings.isBenignEffectful (m, x)
      | EApp ((EFfi _, _), _) => false
      | EApp _ => true

      | EUnop (_, e) => impure e
      | EBinop (_, _, e1, e2) => impure e1 orelse impure e2

      | ERecord xes => List.exists (fn (_, e, _) => impure e) xes
      | EField (e, _) => impure e

      | ECase (e, pes, _) => impure e orelse List.exists (fn (_, e) => impure e) pes

      | EError _ => true
      | EReturnBlob {blob = e1, mimeType = e2, ...} => impure e1 orelse impure e2
      | ERedirect (e, _) => impure e

      | EStrcat (e1, e2) => impure e1 orelse impure e2

      | ESeq (e1, e2) => impure e1 orelse impure e2
      | ELet (_, _, e1, e2) => impure e1 orelse impure e2

      | EClosure (_, es) => List.exists impure es
      | EJavaScript (_, e) => impure e
      | ESignalReturn e => impure e
      | ESignalBind (e1, e2) => impure e1 orelse impure e2
      | ESignalSource e => impure e
      | EServerCall _ => true
      | ERecv _ => true
      | ESleep _ => true
      | ESpawn _ => true

val liftExpInExp = Monoize.liftExpInExp

fun multiLift n e =
    case n of
        0 => e
      | _ => multiLift (n - 1) (liftExpInExp 0 e)

val subExpInExp' =
    U.Exp.mapB {typ = fn t => t,
                exp = fn (xn, rep) => fn e =>
                                  case e of
                                      ERel xn' =>
                                      (case Int.compare (xn', xn) of
                                           EQUAL => #1 rep
                                         | GREATER=> ERel (xn' - 1)
                                         | LESS => e)
                                    | _ => e,
                bind = fn ((xn, rep), U.Exp.RelE _) => (xn+1, liftExpInExp 0 rep)
                        | (ctx, _) => ctx}

fun subExpInExp (n, e1) e2 =
    let
        val r = subExpInExp' (n, e1) e2
    in
        (*Print.prefaces "subExpInExp" [("e1", MonoPrint.p_exp MonoEnv.empty e1),
                                      ("e2", MonoPrint.p_exp MonoEnv.empty e2),
                                      ("r", MonoPrint.p_exp MonoEnv.empty r)];*)
        r
    end

fun typ c = c

val swapExpVars =
    U.Exp.mapB {typ = fn t => t,
                exp = fn lower => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn = lower then
                                             ERel (lower + 1)
                                         else if xn = lower + 1 then
                                             ERel lower
                                         else
                                             e
                                       | _ => e,
                bind = fn (lower, U.Exp.RelE _) => lower+1
                        | (lower, _) => lower}

val swapExpVarsPat =
    U.Exp.mapB {typ = fn t => t,
                exp = fn (lower, len) => fn e =>
                                     case e of
                                         ERel xn =>
                                         if xn = lower then
                                             ERel (lower + len)
                                         else if xn >= lower + 1 andalso xn < lower + 1 + len then
                                             ERel (xn - 1)
                                         else
                                             e
                                       | _ => e,
                bind = fn ((lower, len), U.Exp.RelE _) => (lower+1, len)
                        | (st, _) => st}

datatype result = Yes of exp list | No | Maybe

fun match (env, p : pat, e : exp) =
    case (#1 p, #1 e) of
        (PWild, _) => Yes env
      | (PVar (x, t), _) => Yes (e :: env)

      | (PPrim (Prim.String s), EStrcat ((EPrim (Prim.String s'), _), _)) =>
        if String.isPrefix s' s then
            Maybe
        else
            No

      | (PPrim (Prim.String s), EStrcat (_, (EPrim (Prim.String s'), _))) =>
        if String.isSuffix s' s then
            Maybe
        else
            No

      | (PPrim p, EPrim p') =>
        if Prim.equal (p, p') then
            Yes env
        else
            No

      | (PCon (_, PConVar n1, NONE), ECon (_, PConVar n2, NONE)) =>
        if n1 = n2 then
            Yes env
        else
            No

      | (PCon (_, PConVar n1, SOME p), ECon (_, PConVar n2, SOME e)) =>
        if n1 = n2 then
            match (env, p, e)
        else
            No

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, NONE), ECon (_, PConFfi {mod = m2, con = con2, ...}, NONE)) =>
        if m1 = m2 andalso con1 = con2 then
            Yes env
        else
            No

      | (PCon (_, PConFfi {mod = m1, con = con1, ...}, SOME ep), ECon (_, PConFfi {mod = m2, con = con2, ...}, SOME e)) =>
        if m1 = m2 andalso con1 = con2 then
            match (env, p, e)
        else
            No

      | (PRecord xps, ERecord xes) =>
        let
            fun consider (xps, env) =
                case xps of
                    [] => Yes env
                  | (x, p, _) :: rest =>
                    case List.find (fn (x', _, _) => x' = x) xes of
                        NONE => No
                      | SOME (_, e, _) =>
                        case match (env, p, e) of
                            No => No
                          | Maybe => Maybe
                          | Yes env => consider (rest, env)
        in
            consider (xps, env)
        end

      | (PNone _, ENone _) => Yes env
      | (PNone _, ESome _) => No
      | (PSome (_, p), ESome (_, e)) => match (env, p, e)
      | (PSome _, ENone _) => No

      | _ => Maybe

datatype event =
         WritePage
       | ReadDb
       | WriteDb
       | ReadCookie
       | WriteCookie
       | UseRel
       | Unsure
       | Abort

fun p_event e =
    let
        open Print.PD
    in
        case e of
            WritePage => string "WritePage"
          | ReadDb => string "ReadDb"
          | WriteDb => string "WriteDb"
          | ReadCookie => string "ReadCookie"
          | WriteCookie => string "WriteCookie"
          | UseRel => string "UseRel"
          | Unsure => string "Unsure"
          | Abort => string "Abort"
    end

val p_events = Print.p_list p_event

fun patBinds (p, _) =
    case p of
        PWild => 0
      | PVar _ => 1
      | PPrim _ => 0
      | PCon (_, _, NONE) => 0
      | PCon (_, _, SOME p) => patBinds p
      | PRecord xpts => foldl (fn ((_, p, _), n) => n + patBinds p) 0 xpts
      | PNone _ => 0
      | PSome (_, p) => patBinds p

val countFree = U.Exp.foldB {typ = fn (_, n) => n,
                             exp = fn (x, e, n) =>
                                      case e of
                                          ERel x' => if x = x' then n + 1 else n
                                        | _ => n,
                             bind = fn (n, b) =>
                                       case b of
                                           U.Exp.RelE _ => n + 1
                                     | _ => n}

val freeInAbs = U.Exp.existsB {typ = fn _ => false,
                               exp = fn (n, e) =>
                                        case e of
                                            EAbs (_, _, _, b) => countFree n 0 b > 0
                                          | EJavaScript (_, b) => countFree n 0 b > 0
                                          | _ => false,
                               bind = fn (n, b) =>
                                         case b of
                                             U.Exp.RelE _ => n + 1
                                           | _ => n} 0

fun reduce file =
    let
        val (timpures, impures, absCounts) =
            foldl (fn ((d, _), (timpures, impures, absCounts)) =>
                      let
                          fun countAbs (e, _) =
                              case e of
                                  EAbs (_, _, _, e) => 1 + countAbs e
                                | _ => 0
                      in
                          case d of
                              DDatatype dts =>
                              (if List.exists (fn (_, _, cs) =>
                                                  List.exists (fn (_, _, NONE) => false
                                                                | (_, _, SOME t) => simpleTypeImpure timpures t) cs)
                                              dts then
                                   IS.addList (timpures, map #2 dts)
                               else
                                   timpures,
                               impures,
                               absCounts)
                            | DVal (_, n, _, e, _) =>
                              (timpures,
                               if simpleImpure (timpures, impures) E.empty e then
                                   IS.add (impures, n)
                               else
                                   impures,
                               IM.insert (absCounts, n, countAbs e))
                            | DValRec vis =>
                              (timpures,
                               if List.exists (fn (_, _, _, e, _) => simpleImpure (timpures, impures) E.empty e) vis then
                                   foldl (fn ((_, n, _, _, _), impures) =>
                                             IS.add (impures, n)) impures vis
                               else
                                   impures,
                               foldl (fn ((x, n, _, e, _), absCounts) =>
                                         IM.insert (absCounts, n, countAbs e))
                                     absCounts vis)
                            | _ => (timpures, impures, absCounts)
                      end)
                  (IS.empty, IS.empty, IM.empty) file

        val uses = U.File.fold {typ = fn (_, m) => m,
                                exp = fn (e, m) =>
                                         case e of
                                             ENamed n => IM.insert (m, n, 1 + Option.getOpt (IM.find (m, n), 0))
                                           | _ => m,
                                decl = fn (_, m) => m}
                               IM.empty file

        val size = U.Exp.fold {typ = fn (_, n) => n,
                               exp = fn (_, n) => n + 1} 0

        val functionInside' = U.Typ.exists (fn c => case c of
                                                        TFun _ => true
                                                      | _ => false)
                              
        fun functionInside t =
            case #1 t of
                TFun (t1, t2) => functionInside' t1 orelse functionInside t2
              | _ => functionInside' t

        fun mayInline (n, e, t, s) =
            case IM.find (uses, n) of
                NONE => false
              | SOME count => count <= 1
                              orelse size e <= Settings.getMonoInline ()
                              orelse functionInside t
                              orelse Settings.checkAlwaysInline s

        fun summarize d (e, _) =
            let
                val s =
                    case e of
                        EPrim _ => []
                      | ERel n => if n = d then [UseRel] else []
                      | ENamed _ => []
                      | ECon (_, _, NONE) => []
                      | ECon (_, _, SOME e) => summarize d e
                      | ENone _ => []
                      | ESome (_, e) => summarize d e
                      | EFfi _ => []
                      | EFfiApp ("Basis", "get_cookie", [e]) =>
                        summarize d e @ [ReadCookie]
                      | EFfiApp ("Basis", "set_cookie", es) =>
                        List.concat (map (summarize d) es) @ [WriteCookie]
                      | EFfiApp ("Basis", "clear_cookie", es) =>
                        List.concat (map (summarize d) es) @ [WriteCookie]
                      | EFfiApp (m, x, es) =>
                        if Settings.isEffectful (m, x) orelse Settings.isBenignEffectful (m, x) then
                            List.concat (map (summarize d) es) @ [if m = "Basis" andalso String.isSuffix "_w" x then
                                                                      WritePage
                                                                  else
                                                                      Unsure]
                        else
                            List.concat (map (summarize d) es)
                      | EApp ((EFfi _, _), e) => summarize d e
                      | EApp _ =>
                        let
                            fun unravel (e, passed, ls) =
                                case e of
                                    ENamed n =>
                                    let
                                        val ls = rev ls
                                    in
                                        if IS.member (impures, n) then
                                            case IM.find (absCounts, n) of
                                                NONE => [Unsure]
                                              | SOME len =>
                                                if passed < len then
                                                    ls
                                                else
                                                    ls @ [Unsure]
                                        else
                                            ls
                                    end
                                  | ERel n => List.revAppend (ls,
                                                              if n = d then
                                                                  [UseRel, Unsure]
                                                              else
                                                                  [Unsure])
                                  | EApp (f, x) =>
                                    unravel (#1 f, passed + 1, List.revAppend (summarize d x,
                                                                               ls))
                                  | EError _ => [Abort]
                                  | _ => [Unsure]
                        in
                            unravel (e, 0, [])
                        end

                      | EAbs _ => []

                      | EUnop (_, e) => summarize d e
                      | EBinop (_, _, e1, e2) => summarize d e1 @ summarize d e2

                      | ERecord xets => List.concat (map (summarize d o #2) xets)
                      | EField (e, _) => summarize d e

                      | ECase (e, pes, _) =>
                        let
                            val lss = map (fn (p, e) => summarize (d + patBinds p) e) pes

                            fun splitRel ls acc =
                                case ls of
                                    [] => (acc, false, ls)
                                  | UseRel :: ls  => (acc, true, ls)
                                  | v :: ls => splitRel ls (v :: acc)

                            val (pre, used, post) = foldl (fn (ls, (pre, used, post)) =>
                                                              let
                                                                  val (pre', used', post') = splitRel ls []
                                                              in
                                                                  (pre' @ pre, used' orelse used, post' @ post)
                                                              end)
                                                    ([], false, []) lss
                        in
                            summarize d e
                            @ pre
                            @ (if used then [UseRel] else [])
                            @ post
                        end
                      | EStrcat (e1, e2) => summarize d e1 @ summarize d e2

                      | EError (e, _) => summarize d e @ [Abort]
                      | EReturnBlob {blob = e1, mimeType = e2, ...} => summarize d e1 @ summarize d e2 @ [Abort]
                      | ERedirect (e, _) => summarize d e @ [Abort]

                      | EWrite e => summarize d e @ [WritePage]
                                    
                      | ESeq (e1, e2) => summarize d e1 @ summarize d e2
                      | ELet (_, _, e1, e2) => summarize d e1 @ summarize (d + 1) e2

                      | EClosure (_, es) => List.concat (map (summarize d) es)

                      | EQuery {query, body, initial, ...} =>
                        List.concat [summarize d query,
                                     summarize d initial,
                                     [ReadDb],
                                     summarize (d + 2) body]

                      | EDml (e, _) => summarize d e @ [WriteDb]
                      | ENextval e => summarize d e @ [WriteDb]
                      | ESetval (e1, e2) => summarize d e1 @ summarize d e2 @ [WriteDb]
                      | EUnurlify (e, _, _) => summarize d e
                      | EJavaScript (_, e) => summarize d e
                      | ESignalReturn e => summarize d e
                      | ESignalBind (e1, e2) => summarize d e1 @ summarize d e2
                      | ESignalSource e => summarize d e

                      | EServerCall (e, _, _) => summarize d e @ [Unsure]
                      | ERecv (e, _) => summarize d e @ [Unsure]
                      | ESleep e => summarize d e @ [Unsure]
                      | ESpawn e => summarize d e @ [Unsure]
            in
                (*Print.prefaces "Summarize"
                               [("e", MonoPrint.p_exp MonoEnv.empty (e, ErrorMsg.dummySpan)),
                                ("d", Print.PD.string (Int.toString d)),
                                ("s", p_events s)];*)
                s
            end

        val impure = fn env => fn e =>
                        simpleImpure (timpures, impures) env e andalso impure e
                        andalso not (List.null (summarize ~1 e))

        fun exp env e =
            let
                (*val () = Print.prefaces "exp" [("e", MonoPrint.p_exp env (e, ErrorMsg.dummySpan))]*)

                fun doLet (x, t, e', b) =
                    let
                        fun doSub () =
                            let
                                val r = subExpInExp (0, e') b
                            in
                                (*Print.prefaces "doSub" [("e'", MonoPrint.p_exp env e'),
                                                        ("b", MonoPrint.p_exp (E.pushERel env x t NONE) b),
                                                        ("r", MonoPrint.p_exp env r)];*)
                                #1 (reduceExp env r)
                            end

                        fun trySub () =
                            ((*Print.prefaces "trySub"
                                            [("e", MonoPrint.p_exp env (e, ErrorMsg.dummySpan))];*)
                             case t of
                                 (TFfi ("Basis", "string"), _) => doSub ()
                               | (TSignal _, _) => e
                               | _ =>
                                 case e' of
                                     (ECase _, _) => e
                                   | _ => doSub ())
                    in
                        if impure env e' then
                            let
                                val effs_e' = summarize 0 e'
                                val effs_e' = List.filter (fn x => x <> UseRel) effs_e'
                                val effs_b = summarize 0 b

                                (*val () = Print.fprefaces outf "Try"
                                                        [(*("e", MonoPrint.p_exp env (e, ErrorMsg.dummySpan)),*)
                                                         ("e'", MonoPrint.p_exp env e'),
                                                         ("b", MonoPrint.p_exp (E.pushERel env x t NONE) b),
                                                         ("e'_eff", p_events effs_e'),
                                                         ("b_eff", p_events effs_b)]*)

                                fun does eff = List.exists (fn eff' => eff' = eff) effs_e'
                                val writesPage = does WritePage
                                val readsDb = does ReadDb
                                val writesDb = does WriteDb
                                val readsCookie = does ReadCookie
                                val writesCookie = does ReadCookie

                                fun verifyUnused eff =
                                    case eff of
                                        UseRel => false
                                      | _ => true

                                fun verifyCompatible effs =
                                    case effs of
                                        [] => false
                                      | eff :: effs =>
                                        case eff of
                                            Unsure => false
                                          | UseRel => List.all verifyUnused effs
                                          | WritePage => not writesPage andalso verifyCompatible effs
                                          | ReadDb => not writesDb andalso verifyCompatible effs
                                          | WriteDb => not writesDb andalso not readsDb andalso verifyCompatible effs
                                          | ReadCookie => not writesCookie andalso verifyCompatible effs
                                          | WriteCookie => not writesCookie andalso not readsCookie
                                                           andalso verifyCompatible effs
                                          | Abort => true
                            in
                                (*Print.prefaces "verifyCompatible"
                                                 [("e'", MonoPrint.p_exp env e'),
                                                  ("b", MonoPrint.p_exp (E.pushERel env x t NONE) b),
                                                  ("effs_e'", Print.p_list p_event effs_e'),
                                                  ("effs_b", Print.p_list p_event effs_b)];*)
                                if (List.null effs_e'
                                    orelse (List.all (fn eff => eff <> Unsure) effs_e'
                                            andalso verifyCompatible effs_b)
                                    orelse (case effs_b of
                                                UseRel :: effs => List.all verifyUnused effs
                                              | _ => false))
                                   andalso countFree 0 0 b = 1
                                   andalso not (freeInAbs b) then
                                    trySub ()
                                else
                                    e
                            end
                        else
                            trySub ()
                    end

                val r =
                    case e of
                        ERel n =>
                        (case E.lookupERel env n of
                             (_, _, SOME e') => #1 e'
                           | _ => e)
                      | ENamed n =>
                        (case E.lookupENamed env n of
                             (_, _, SOME e', _) => ((*Print.prefaces "Switch" [("n", Print.PD.string (Int.toString n)),
                                                                               ("e'", MonoPrint.p_exp env e')];*)
                                                    #1 e')
                           | _ => e)

                      | EApp ((EAbs (x, t, _, e1), loc), e2) =>
                        ((*Print.prefaces "Considering" [("e1", MonoPrint.p_exp (E.pushERel env x t NONE) e1),
                                                       ("e2", MonoPrint.p_exp env e2),
                                                       ("sub", MonoPrint.p_exp env (reduceExp env (subExpInExp (0, e2) e1)))];*)
                         if impure env e2 orelse countFree 0 0 e1 > 1 then
                             #1 (reduceExp env (ELet (x, t, e2, e1), loc))
                         else
                             #1 (reduceExp env (subExpInExp (0, e2) e1)))

                      | ECase (e', pes, {disc, result}) =>
                        let
                            fun push () =
                                case result of
                                    (TFun (dom, result), loc) =>
                                    let
                                        fun safe (e, _) =
                                            case e of
                                                EAbs _ => true
                                              | EError _ => true
                                              | _ => false
                                    in
                                        if List.all (safe o #2) pes then
                                            EAbs ("y", dom, result,
                                                  (ECase (liftExpInExp 0 e',
                                                          map (fn (p, (EAbs (_, _, _, e), _)) =>
                                                                  (p, swapExpVarsPat (0, patBinds p) e)
                                                                | (p, (EError (e, (TFun (_, t), _)), loc)) =>
                                                                  (p, (EError (e, t), loc))
                                                                | _ => raise Fail "MonoReduce ECase") pes,
                                                          {disc = disc, result = result}), loc))
                                        else
                                            e
                                    end
                                  | _ => e

                            fun search pes =
                                case pes of
                                    [] => push ()
                                  | (p, body) :: pes =>
                                    case match ([], p, e') of
                                        No => search pes
                                      | Maybe => push ()
                                      | Yes subs =>
                                        let
                                            val (body, remaining) =
                                                foldl (fn (e, (body, remaining)) =>
                                                          (subExpInExp (0, multiLift remaining e) body, remaining - 1))
                                                      (body, length subs - 1) subs
                                            val r = reduceExp (E.patBinds env p) body
                                        in
                                            (*Print.preface ("subs", Print.p_list (MonoPrint.p_exp env) subs);*)
                                            (*Print.prefaces "ECase"
                                                           [("old", MonoPrint.p_exp env body),
                                                            ("body", MonoPrint.p_exp env body),
                                                            ("r", MonoPrint.p_exp env r)];*)
                                            #1 r
                                        end
                        in
                            if impure env e' then
                                e
                            else
                                search pes
                        end

                      | EField ((ERecord xes, _), x) =>
                        (case List.find (fn (x', _, _) => x' = x) xes of
                             SOME (_, e, _) => #1 e
                           | NONE => e)

                      | ELet (x1, t1, (ELet (x2, t2, e1, b1), loc), b2) =>
                        let
                            val e' = (ELet (x2, t2, e1,
                                            (ELet (x1, t1, b1,
                                                   liftExpInExp 1 b2), loc)), loc)
                        in
                            (*Print.prefaces "ELet commute" [("e", MonoPrint.p_exp env (e, loc)),
                                                             ("e'", MonoPrint.p_exp env e')];*)
                            #1 (reduceExp env e')
                        end
                      | EApp ((ELet (x, t, e, b), loc), e') =>
                        #1 (reduceExp env (ELet (x, t, e,
                                                 (EApp (b, liftExpInExp 0 e'), loc)), loc))

                      | ELet (x, t, e', b as (EAbs (x', t' as (TRecord [], _), ran, e''), loc)) =>
                        if impure env e' then
                            doLet (x, t, e', b)
                        else
                            EAbs (x', t', ran, reduceExp (E.pushERel env x' t' NONE)
                                                         (ELet (x, t, liftExpInExp 0 e', swapExpVars 0 e''), loc))

                      | ELet (x, t, e', b) => doLet (x, t, e', b)

                      | EStrcat ((EPrim (Prim.String s1), _), (EPrim (Prim.String s2), _)) =>
                        EPrim (Prim.String (s1 ^ s2))

                      | ESignalBind ((ESignalReturn e1, loc), e2) =>
                        #1 (reduceExp env (EApp (e2, e1), loc))

                      | _ => e
            in
                (*Print.prefaces "exp'" [("e", MonoPrint.p_exp env (e, ErrorMsg.dummySpan)),
                                       ("r", MonoPrint.p_exp env (r, ErrorMsg.dummySpan))];*)
                r
            end

        and bind (env, b) =
            case b of
                U.Decl.Datatype (x, n, xncs) => E.pushDatatype env x n xncs
              | U.Decl.RelE (x, t) => E.pushERel env x t NONE
              | U.Decl.NamedE (x, n, t, eo, s) =>
                let
                    val eo = case eo of
                                 NONE => NONE
                               | SOME e => if mayInline (n, e, t, s) then
                                               SOME e
                                           else
                                               NONE
                in
                    E.pushENamed env x n t (Option.map (reduceExp env) eo) s
                end

        and reduceExp env = U.Exp.mapB {typ = typ, exp = exp, bind = bind} env

        fun decl env d = ((*Print.preface ("d", MonoPrint.p_decl env (d, ErrorMsg.dummySpan));*)
                          d)
    in
        U.File.mapB {typ = typ, exp = exp, decl = decl, bind = bind} E.empty file
    end

end
