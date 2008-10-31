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

 structure Elaborate :> ELABORATE = struct

 structure P = Prim
 structure L = Source
 structure L' = Elab
 structure E = ElabEnv
 structure U = ElabUtil
 structure D = Disjoint

 open Print
 open ElabPrint
 open ElabErr

 structure IM = IntBinaryMap

 structure SK = struct
 type ord_key = string
 val compare = String.compare
 end

 structure SS = BinarySetFn(SK)
 structure SM = BinaryMapFn(SK)

 val basis_r = ref 0

 fun elabExplicitness e =
     case e of
         L.Explicit => L'.Explicit
       | L.Implicit => L'.Implicit

 fun occursKind r =
     U.Kind.exists (fn L'.KUnif (_, _, r') => r = r'
                     | _ => false)

 exception KUnify' of kunify_error

 fun unifyKinds' (k1All as (k1, _)) (k2All as (k2, _)) =
     let
         fun err f = raise KUnify' (f (k1All, k2All))
     in
         case (k1, k2) of
             (L'.KType, L'.KType) => ()
           | (L'.KUnit, L'.KUnit) => ()

           | (L'.KArrow (d1, r1), L'.KArrow (d2, r2)) =>
             (unifyKinds' d1 d2;
              unifyKinds' r1 r2)
           | (L'.KName, L'.KName) => ()
           | (L'.KRecord k1, L'.KRecord k2) => unifyKinds' k1 k2
           | (L'.KTuple ks1, L'.KTuple ks2) =>
             ((ListPair.appEq (fn (k1, k2) => unifyKinds' k1 k2) (ks1, ks2))
              handle ListPair.UnequalLengths => err KIncompatible)

           | (L'.KError, _) => ()
           | (_, L'.KError) => ()

           | (L'.KUnif (_, _, ref (SOME k1All)), _) => unifyKinds' k1All k2All
           | (_, L'.KUnif (_, _, ref (SOME k2All))) => unifyKinds' k1All k2All

           | (L'.KUnif (_, _, r1), L'.KUnif (_, _, r2)) =>
             if r1 = r2 then
                 ()
             else
                 r1 := SOME k2All

           | (L'.KUnif (_, _, r), _) =>
             if occursKind r k2All then
                 err KOccursCheckFailed
             else
                 r := SOME k2All
           | (_, L'.KUnif (_, _, r)) =>
             if occursKind r k1All then
                 err KOccursCheckFailed
             else
                 r := SOME k1All

           | _ => err KIncompatible
     end

 exception KUnify of L'.kind * L'.kind * kunify_error

 fun unifyKinds k1 k2 =
     unifyKinds' k1 k2
     handle KUnify' err => raise KUnify (k1, k2, err)

 fun checkKind env c k1 k2 =
     unifyKinds k1 k2
     handle KUnify (k1, k2, err) =>
            conError env (WrongKind (c, k1, k2, err))

 val dummy = ErrorMsg.dummySpan

 val ktype = (L'.KType, dummy)
 val kname = (L'.KName, dummy)
 val ktype_record = (L'.KRecord ktype, dummy)

 val cerror = (L'.CError, dummy)
 val kerror = (L'.KError, dummy)
 val eerror = (L'.EError, dummy)
 val sgnerror = (L'.SgnError, dummy)
 val strerror = (L'.StrError, dummy)

 val int = ref cerror
 val float = ref cerror
 val string = ref cerror
 val table = ref cerror

 local
     val count = ref 0
 in

 fun resetKunif () = count := 0

 fun kunif loc =
     let
         val n = !count
         val s = if n <= 26 then
                     str (chr (ord #"A" + n))
                 else
                     "U" ^ Int.toString (n - 26)
     in
         count := n + 1;
         (L'.KUnif (loc, s, ref NONE), dummy)
     end

 end

 local
     val count = ref 0
 in

 fun resetCunif () = count := 0

 fun cunif (loc, k) =
     let
         val n = !count
         val s = if n <= 26 then
                     str (chr (ord #"A" + n))
                 else
                     "U" ^ Int.toString (n - 26)
     in
         count := n + 1;
         (L'.CUnif (loc, k, s, ref NONE), dummy)
     end

 end

 fun elabKind (k, loc) =
     case k of
         L.KType => (L'.KType, loc)
       | L.KArrow (k1, k2) => (L'.KArrow (elabKind k1, elabKind k2), loc)
       | L.KName => (L'.KName, loc)
       | L.KRecord k => (L'.KRecord (elabKind k), loc)
       | L.KUnit => (L'.KUnit, loc)
       | L.KTuple ks => (L'.KTuple (map elabKind ks), loc)
       | L.KWild => kunif loc

 fun foldKind (dom, ran, loc)=
     (L'.KArrow ((L'.KArrow ((L'.KName, loc),
                             (L'.KArrow (dom,
                                         (L'.KArrow (ran, ran), loc)), loc)), loc),
                 (L'.KArrow (ran,
                             (L'.KArrow ((L'.KRecord dom, loc),
                                         ran), loc)), loc)), loc)

 fun hnormKind (kAll as (k, _)) =
     case k of
         L'.KUnif (_, _, ref (SOME k)) => hnormKind k
       | _ => kAll

 fun elabCon (env, denv) (c, loc) =
     case c of
         L.CAnnot (c, k) =>
         let
             val k' = elabKind k
             val (c', ck, gs) = elabCon (env, denv) c
         in
             checkKind env c' ck k';
             (c', k', gs)
         end

       | L.TFun (t1, t2) =>
         let
             val (t1', k1, gs1) = elabCon (env, denv) t1
             val (t2', k2, gs2) = elabCon (env, denv) t2
         in
             checkKind env t1' k1 ktype;
             checkKind env t2' k2 ktype;
             ((L'.TFun (t1', t2'), loc), ktype, gs1 @ gs2)
         end
       | L.TCFun (e, x, k, t) =>
         let
             val e' = elabExplicitness e
             val k' = elabKind k
             val env' = E.pushCRel env x k'
             val (t', tk, gs) = elabCon (env', D.enter denv) t
         in
             checkKind env t' tk ktype;
             ((L'.TCFun (e', x, k', t'), loc), ktype, gs)
         end
       | L.CDisjoint (c1, c2, c) =>
         let
             val (c1', k1, gs1) = elabCon (env, denv) c1
             val (c2', k2, gs2) = elabCon (env, denv) c2

             val ku1 = kunif loc
             val ku2 = kunif loc

             val (denv', gs3) = D.assert env denv (c1', c2')
             val (c', k, gs4) = elabCon (env, denv') c
         in
             checkKind env c1' k1 (L'.KRecord ku1, loc);
             checkKind env c2' k2 (L'.KRecord ku2, loc);

             ((L'.CDisjoint (L'.Instantiate, c1', c2', c'), loc), k, gs1 @ gs2 @ gs3 @ gs4)
         end
       | L.TRecord c =>
         let
             val (c', ck, gs) = elabCon (env, denv) c
             val k = (L'.KRecord ktype, loc)
         in
             checkKind env c' ck k;
             ((L'.TRecord c', loc), ktype, gs)
         end

       | L.CVar ([], s) =>
         (case E.lookupC env s of
              E.NotBound =>
              (conError env (UnboundCon (loc, s));
               (cerror, kerror, []))
            | E.Rel (n, k) =>
              ((L'.CRel n, loc), k, [])
            | E.Named (n, k) =>
              ((L'.CNamed n, loc), k, []))
       | L.CVar (m1 :: ms, s) =>
         (case E.lookupStr env m1 of
              NONE => (conError env (UnboundStrInCon (loc, m1));
                       (cerror, kerror, []))
            | SOME (n, sgn) =>
              let
                  val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                      case E.projectStr env {sgn = sgn, str = str, field = m} of
                                          NONE => (conError env (UnboundStrInCon (loc, m));
                                                   (strerror, sgnerror))
                                        | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                   ((L'.StrVar n, loc), sgn) ms

                  val k = case E.projectCon env {sgn = sgn, str = str, field = s} of
                              NONE => (conError env (UnboundCon (loc, s));
                                       kerror)
                            | SOME (k, _) => k
              in
                  ((L'.CModProj (n, ms, s), loc), k, [])
              end)

       | L.CApp (c1, c2) =>
         let
             val (c1', k1, gs1) = elabCon (env, denv) c1
             val (c2', k2, gs2) = elabCon (env, denv) c2
             val dom = kunif loc
             val ran = kunif loc
         in
             checkKind env c1' k1 (L'.KArrow (dom, ran), loc);
             checkKind env c2' k2 dom;
             ((L'.CApp (c1', c2'), loc), ran, gs1 @ gs2)
         end
       | L.CAbs (x, ko, t) =>
         let
             val k' = case ko of
                          NONE => kunif loc
                        | SOME k => elabKind k
             val env' = E.pushCRel env x k'
             val (t', tk, gs) = elabCon (env', D.enter denv) t
         in
             ((L'.CAbs (x, k', t'), loc),
              (L'.KArrow (k', tk), loc),
              gs)
         end

       | L.CName s =>
         ((L'.CName s, loc), kname, [])

       | L.CRecord xcs =>
         let
             val k = kunif loc

             val (xcs', gs) = ListUtil.foldlMap (fn ((x, c), gs) =>
                                                    let
                                                        val (x', xk, gs1) = elabCon (env, denv) x
                                                        val (c', ck, gs2) = elabCon (env, denv) c
                                                    in
                                                        checkKind env x' xk kname;
                                                        checkKind env c' ck k;
                                                        ((x', c'), gs1 @ gs2 @ gs)
                                                    end) [] xcs

             val rc = (L'.CRecord (k, xcs'), loc)
             (* Add duplicate field checking later. *)

             fun prove (xcs, ds) =
                 case xcs of
                     [] => ds
                   | xc :: rest =>
                     let
                         val r1 = (L'.CRecord (k, [xc]), loc)
                         val ds = foldl (fn (xc', ds) =>
                                            let
                                                val r2 = (L'.CRecord (k, [xc']), loc)
                                            in
                                                D.prove env denv (r1, r2, loc) @ ds
                                            end)
                                  ds rest
                     in
                         prove (rest, ds)
                     end
         in
             (rc, (L'.KRecord k, loc), prove (xcs', gs))
         end
       | L.CConcat (c1, c2) =>
         let
             val (c1', k1, gs1) = elabCon (env, denv) c1
             val (c2', k2, gs2) = elabCon (env, denv) c2
             val ku = kunif loc
             val k = (L'.KRecord ku, loc)
         in
             checkKind env c1' k1 k;
             checkKind env c2' k2 k;
             ((L'.CConcat (c1', c2'), loc), k,
              D.prove env denv (c1', c2', loc) @ gs1 @ gs2)
         end
       | L.CFold =>
         let
             val dom = kunif loc
             val ran = kunif loc
         in
             ((L'.CFold (dom, ran), loc),
              foldKind (dom, ran, loc),
              [])
         end

       | L.CUnit => ((L'.CUnit, loc), (L'.KUnit, loc), [])

       | L.CTuple cs =>
         let
             val (cs', ks, gs) = foldl (fn (c, (cs', ks, gs)) =>
                                           let
                                               val (c', k, gs') = elabCon (env, denv) c
                                           in
                                               (c' :: cs', k :: ks, gs' @ gs)
                                           end) ([], [], []) cs
         in
             ((L'.CTuple (rev cs'), loc), (L'.KTuple (rev ks), loc), gs)
         end
       | L.CProj (c, n) =>
         let
             val (c', k, gs) = elabCon (env, denv) c
         in
             case hnormKind k of
                 (L'.KTuple ks, _) =>
                 if n <= 0 orelse n > length ks then
                     (conError env (ProjBounds (c', n));
                      (cerror, kerror, []))
                 else
                     ((L'.CProj (c', n), loc), List.nth (ks, n - 1), gs)
               | k => (conError env (ProjMismatch (c', k));
                       (cerror, kerror, []))
         end

       | L.CWild k =>
         let
             val k' = elabKind k
         in
             (cunif (loc, k'), k', [])
         end

 fun kunifsRemain k =
     case k of
         L'.KUnif (_, _, ref NONE) => true
       | _ => false
 fun cunifsRemain c =
     case c of
         L'.CUnif (loc, _, _, ref NONE) => SOME loc
       | _ => NONE

 val kunifsInDecl = U.Decl.exists {kind = kunifsRemain,
                                   con = fn _ => false,
                                   exp = fn _ => false,
                                   sgn_item = fn _ => false,
                                   sgn = fn _ => false,
                                   str = fn _ => false,
                                   decl = fn _ => false}

 val cunifsInDecl = U.Decl.search {kind = fn _ => NONE,
                                   con = cunifsRemain,
                                   exp = fn _ => NONE,
                                   sgn_item = fn _ => NONE,
                                   sgn = fn _ => NONE,
                                   str = fn _ => NONE,
                                   decl = fn _ => NONE}

 fun occursCon r =
     U.Con.exists {kind = fn _ => false,
                   con = fn L'.CUnif (_, _, _, r') => r = r'
                          | _ => false}

 exception CUnify' of cunify_error

 exception SynUnif = E.SynUnif

 open ElabOps

 type record_summary = {
      fields : (L'.con * L'.con) list,
      unifs : (L'.con * L'.con option ref) list,
      others : L'.con list
 }

 fun summaryToCon {fields, unifs, others} =
     let
         val c = (L'.CRecord (ktype, []), dummy)
         val c = List.foldr (fn (c', c) => (L'.CConcat (c', c), dummy)) c others
         val c = List.foldr (fn ((c', _), c) => (L'.CConcat (c', c), dummy)) c unifs
     in
         (L'.CConcat ((L'.CRecord (ktype, fields), dummy), c), dummy)
     end

 fun p_summary env s = p_con env (summaryToCon s)

 exception CUnify of L'.con * L'.con * cunify_error

 fun kindof env (c, loc) =
     case c of
         L'.TFun _ => ktype
       | L'.TCFun _ => ktype
       | L'.TRecord _ => ktype

       | L'.CRel xn => #2 (E.lookupCRel env xn)
       | L'.CNamed xn => #2 (E.lookupCNamed env xn)
       | L'.CModProj (n, ms, x) =>
         let
             val (_, sgn) = E.lookupStrNamed env n
             val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                        case E.projectStr env {sgn = sgn, str = str, field = m} of
                                            NONE => raise Fail "kindof: Unknown substructure"
                                          | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                              ((L'.StrVar n, loc), sgn) ms
         in
             case E.projectCon env {sgn = sgn, str = str, field = x} of
                 NONE => raise Fail "kindof: Unknown con in structure"
               | SOME (k, _) => k
         end

       | L'.CApp (c, _) =>
         (case hnormKind (kindof env c) of
              (L'.KArrow (_, k), _) => k
            | (L'.KError, _) => kerror
            | k => raise CUnify' (CKindof (k, c, "arrow")))
       | L'.CAbs (x, k, c) => (L'.KArrow (k, kindof (E.pushCRel env x k) c), loc)
       | L'.CDisjoint (_, _, _, c) => kindof env c

       | L'.CName _ => kname

       | L'.CRecord (k, _) => (L'.KRecord k, loc)
       | L'.CConcat (c, _) => kindof env c
       | L'.CFold (dom, ran) => foldKind (dom, ran, loc)

       | L'.CUnit => (L'.KUnit, loc)

       | L'.CTuple cs => (L'.KTuple (map (kindof env) cs), loc)
       | L'.CProj (c, n) =>
         (case hnormKind (kindof env c) of
              (L'.KTuple ks, _) => List.nth (ks, n - 1)
            | k => raise CUnify' (CKindof (k, c, "tuple")))

       | L'.CError => kerror
       | L'.CUnif (_, k, _, _) => k

 val hnormCon = D.hnormCon

 datatype con_summary =
          Nil
        | Cons
        | Unknown

 fun compatible cs =
     case cs of
         (Unknown, _) => false
       | (_, Unknown) => false
       | (s1, s2) => s1 = s2

 fun summarizeCon (env, denv) c =
     let
         val (c, gs) = hnormCon (env, denv) c
     in
         case #1 c of
             L'.CRecord (_, []) => (Nil, gs)
           | L'.CRecord (_, _ :: _) => (Cons, gs)
           | L'.CConcat ((L'.CRecord (_, _ :: _), _), _) => (Cons, gs)
           | L'.CDisjoint (_, _, _, c) =>
             let
                 val (s, gs') = summarizeCon (env, denv) c
             in
                 (s, gs @ gs')
             end
           | _ => (Unknown, gs)
     end

 fun p_con_summary s =
     Print.PD.string (case s of
                          Nil => "Nil"
                        | Cons => "Cons"
                        | Unknown => "Unknown")

 exception SummaryFailure

 fun unifyRecordCons (env, denv) (c1, c2) =
     let
         fun rkindof c =
             case hnormKind (kindof env c) of
                 (L'.KRecord k, _) => k
               | (L'.KError, _) => kerror
               | k => raise CUnify' (CKindof (k, c, "record"))

         val k1 = rkindof c1
         val k2 = rkindof c2

         val (r1, gs1) = recordSummary (env, denv) c1
         val (r2, gs2) = recordSummary (env, denv) c2
     in
         unifyKinds k1 k2;
         unifySummaries (env, denv) (k1, r1, r2);
         gs1 @ gs2
     end

 and recordSummary (env, denv) c =
     let
         val (c, gs) = hnormCon (env, denv) c

         val (sum, gs') =
             case c of
                 (L'.CRecord (_, xcs), _) => ({fields = xcs, unifs = [], others = []}, [])
               | (L'.CConcat (c1, c2), _) =>
                 let
                     val (s1, gs1) = recordSummary (env, denv) c1
                     val (s2, gs2) = recordSummary (env, denv) c2
                 in
                     ({fields = #fields s1 @ #fields s2,
                       unifs = #unifs s1 @ #unifs s2,
                       others = #others s1 @ #others s2},
                      gs1 @ gs2)
                 end
               | (L'.CUnif (_, _, _, ref (SOME c)), _) => recordSummary (env, denv) c
               | c' as (L'.CUnif (_, _, _, r), _) => ({fields = [], unifs = [(c', r)], others = []}, [])
               | c' => ({fields = [], unifs = [], others = [c']}, [])
     in
         (sum, gs @ gs')
     end

 and consEq (env, denv) (c1, c2) =
     let
         val gs = unifyCons (env, denv) c1 c2
     in
         List.all (fn (loc, env, denv, c1, c2) =>
                      case D.prove env denv (c1, c2, loc) of
                          [] => true
                        | _ => false) gs
     end
     handle CUnify _ => false

 and consNeq env (c1, c2) =
     case (#1 (ElabOps.hnormCon env c1), #1 (ElabOps.hnormCon env c2)) of
         (L'.CName x1, L'.CName x2) => x1 <> x2
       | _ => false

 and unifySummaries (env, denv) (k, s1 : record_summary, s2 : record_summary) =
     let
         val loc = #2 k
         (*val () = eprefaces "Summaries" [("#1", p_summary env s1),
                                         ("#2", p_summary env s2)]*)

         fun eatMatching p (ls1, ls2) =
             let
                 fun em (ls1, ls2, passed1) =
                     case ls1 of
                         [] => (rev passed1, ls2)
                       | h1 :: t1 =>
                         let
                             fun search (ls2', passed2) =
                                 case ls2' of
                                     [] => em (t1, ls2, h1 :: passed1)
                                   | h2 :: t2 =>
                                     if p (h1, h2) then
                                         em (t1, List.revAppend (passed2, t2), passed1)
                                     else
                                         search (t2, h2 :: passed2)
                         in
                             search (ls2, [])
                         end
             in
                 em (ls1, ls2, [])
             end

         val (fs1, fs2) = eatMatching (fn ((x1, c1), (x2, c2)) =>
                                          not (consNeq env (x1, x2))
                                          andalso consEq (env, denv) (c1, c2)
                                          andalso consEq (env, denv) (x1, x2))
                                      (#fields s1, #fields s2)
         (*val () = eprefaces "Summaries2" [("#1", p_summary env {fields = fs1, unifs = #unifs s1, others = #others s1}),
                                          ("#2", p_summary env {fields = fs2, unifs = #unifs s2, others = #others s2})]*)
         val (unifs1, unifs2) = eatMatching (fn ((_, r1), (_, r2)) => r1 = r2) (#unifs s1, #unifs s2)
         val (others1, others2) = eatMatching (consEq (env, denv)) (#others s1, #others s2)
         (*val () = eprefaces "Summaries3" [("#1", p_summary env {fields = fs1, unifs = unifs1, others = others1}),
                                          ("#2", p_summary env {fields = fs2, unifs = unifs2, others = others2})]*)

         fun unifFields (fs, others, unifs) =
             case (fs, others, unifs) of
                 ([], [], _) => ([], [], unifs)
               | (_, _, []) => (fs, others, [])
               | (_, _, (_, r) :: rest) =>
                 let
                     val r' = ref NONE
                     val kr = (L'.KRecord k, dummy)
                     val cr' = (L'.CUnif (dummy, kr, "recd", r'), dummy)

                     val prefix = case (fs, others) of
                                      ([], other :: others) =>
                                      List.foldl (fn (other, c) =>
                                                     (L'.CConcat (c, other), dummy))
                                                 other others
                                    | (fs, []) =>
                                      (L'.CRecord (k, fs), dummy)
                                    | (fs, others) =>
                                      List.foldl (fn (other, c) =>
                                                     (L'.CConcat (c, other), dummy))
                                                 (L'.CRecord (k, fs), dummy) others
                 in
                     r := SOME (L'.CConcat (prefix, cr'), dummy);
                     ([], [], (cr', r') :: rest)
                 end

         val (fs1, others1, unifs2) = unifFields (fs1, others1, unifs2)
         val (fs2, others2, unifs1) = unifFields (fs2, others2, unifs1)

         (*val () = eprefaces "Summaries4" [("#1", p_summary env {fields = fs1, unifs = unifs1, others = others1}),
                                          ("#2", p_summary env {fields = fs2, unifs = unifs2, others = others2})]*)

         fun isGuessable (other, fs) =
             let
                 val gs = guessFold (env, denv) (other, (L'.CRecord (k, fs), loc), [], SummaryFailure)
             in
                 List.all (fn (loc, env, denv, c1, c2) =>
                              case D.prove env denv (c1, c2, loc) of
                                  [] => true
                                | _ => false) gs
             end
             handle SummaryFailure => false

         val (fs1, fs2, others1, others2) =
             case (fs1, fs2, others1, others2) of
                 ([], _, [other1], []) =>
                 if isGuessable (other1, fs2) then
                     ([], [], [], [])
                 else
                     (fs1, fs2, others1, others2)
               | (_, [], [], [other2]) =>
                 if isGuessable (other2, fs1) then
                     ([], [], [], [])
                 else
                     (fs1, fs2, others1, others2)
               | _ => (fs1, fs2, others1, others2)

         (*val () = eprefaces "Summaries5" [("#1", p_summary env {fields = fs1, unifs = unifs1, others = others1}),
                                            ("#2", p_summary env {fields = fs2, unifs = unifs2, others = others2})]*)

         val clear = case (fs1, others1, fs2, others2) of
                          ([], [], [], []) => true
                        | _ => false
         val empty = (L'.CRecord (k, []), dummy)

         fun unsummarize {fields, unifs, others} =
             let
                 val c = (L'.CRecord (k, fields), loc)

                 val c = foldl (fn ((c1, _), c2) => (L'.CConcat (c1, c2), loc))
                               c unifs
             in
                 foldl (fn (c1, c2) => (L'.CConcat (c1, c2), loc))
                       c others
             end

         fun pairOffUnifs (unifs1, unifs2) =
             case (unifs1, unifs2) of
                 ([], _) =>
                 if clear then
                     List.app (fn (_, r) => r := SOME empty) unifs2
                 else
                     raise CUnify' (CRecordFailure (unsummarize s1, unsummarize s2))
               | (_, []) =>
                 if clear then
                     List.app (fn (_, r) => r := SOME empty) unifs1
                 else
                     raise CUnify' (CRecordFailure (unsummarize s1, unsummarize s2))
               | ((c1, _) :: rest1, (_, r2) :: rest2) =>
                 (r2 := SOME c1;
                  pairOffUnifs (rest1, rest2))
     in
         pairOffUnifs (unifs1, unifs2)
         (*before eprefaces "Summaries'" [("#1", p_summary env s1),
                                       ("#2", p_summary env s2)]*)
     end

 and guessFold (env, denv) (c1, c2, gs, ex) =
     let
         val loc = #2 c1

         fun unfold (dom, ran, f, i, r, c) =
             let
                 val nm = cunif (loc, (L'.KName, loc))
                 val v =
                     case dom of
                         (L'.KUnit, _) => (L'.CUnit, loc)
                       | _ => cunif (loc, dom)
                 val rest = cunif (loc, (L'.KRecord dom, loc))
                 val acc = (L'.CFold (dom, ran), loc)
                 val acc = (L'.CApp (acc, f), loc)
                 val acc = (L'.CApp (acc, i), loc)
                 val acc = (L'.CApp (acc, rest), loc)

                 val (iS, gs3) = summarizeCon (env, denv) i

                 val app = (L'.CApp (f, nm), loc)
                 val app = (L'.CApp (app, v), loc)
                 val app = (L'.CApp (app, acc), loc)
                 val (appS, gs4) = summarizeCon (env, denv) app

                 val (cS, gs5) = summarizeCon (env, denv) c
             in
                 (*prefaces "Summaries" [("iS", p_con_summary iS),
                                         ("appS", p_con_summary appS),
                                         ("cS", p_con_summary cS)];*)

                 if compatible (iS, appS) then
                     raise ex
                 else if compatible (cS, iS) then
                     let
                         (*val () = prefaces "Same?" [("i", p_con env i),
                                                    ("c", p_con env c)]*)
                         val gs6 = unifyCons (env, denv) i c
                         (*val () = TextIO.print "Yes!\n"*)

                         val gs7 = unifyCons (env, denv) r (L'.CRecord (dom, []), loc)
                     in
                         gs @ gs3 @ gs5 @ gs6 @ gs7
                     end
                 else if compatible (cS, appS) then
                     let
                         (*val () = prefaces "Same?" [("app", p_con env app),
                                                      ("c", p_con env c),
                                                      ("app'", p_con env (#1 (hnormCon (env, denv) app)))]*)
                         val gs6 = unifyCons (env, denv) app c
                         (*val () = TextIO.print "Yes!\n"*)

                         val singleton = (L'.CRecord (dom, [(nm, v)]), loc)
                         val concat = (L'.CConcat (singleton, rest), loc)
                         (*val () = prefaces "Pre-crew" [("r", p_con env r),
                                                         ("concat", p_con env concat)]*)
                         val gs7 = unifyCons (env, denv) r concat
                     in
                         (*prefaces "The crew" [("nm", p_con env nm),
                                                ("v", p_con env v),
                                                ("rest", p_con env rest)];*)

                         gs @ gs3 @ gs4 @ gs5 @ gs6 @ gs7
                     end
                 else
                     raise ex
             end
             handle _ => raise ex
     in
         case (#1 c1, #1 c2) of
             (L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold (dom, ran), _), f), _), i), _), r), _) =>
             unfold (dom, ran, f, i, r, c2)
           | (_, L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold (dom, ran), _), f), _), i), _), r)) =>
             unfold (dom, ran, f, i, r, c1)
           | _ => raise ex
     end

 and unifyCons' (env, denv) c1 c2 =
     case (#1 c1, #1 c2) of
         (L'.CDisjoint (_, x1, y1, t1), L'.CDisjoint (_, x2, y2, t2)) =>
         let
             val gs1 = unifyCons' (env, denv) x1 x2
             val gs2 = unifyCons' (env, denv) y1 y2
             val (denv', gs3) = D.assert env denv (x1, y1)
             val gs4 = unifyCons' (env, denv') t1 t2
         in
             gs1 @ gs2 @ gs3 @ gs4
         end
       | _ =>
         case (kindof env c1, kindof env c2) of
             ((L'.KUnit, _), (L'.KUnit, _)) => []
           | _ =>
             let
                 val (c1, gs1) = hnormCon (env, denv) c1
                 val (c2, gs2) = hnormCon (env, denv) c2
             in
                 let
                     val gs3 = unifyCons'' (env, denv) c1 c2
                 in
                     gs1 @ gs2 @ gs3
                 end
                 handle ex => guessFold (env, denv) (c1, c2, gs1 @ gs2, ex)
             end

 and unifyCons'' (env, denv) (c1All as (c1, loc)) (c2All as (c2, _)) =
     let
         fun err f = raise CUnify' (f (c1All, c2All))

         fun isRecord () = unifyRecordCons (env, denv) (c1All, c2All)
     in
         (*eprefaces "unifyCons''" [("c1All", p_con env c1All),
                                  ("c2All", p_con env c2All)];*)

         case (c1, c2) of
             (L'.CUnit, L'.CUnit) => []

           | (L'.TFun (d1, r1), L'.TFun (d2, r2)) =>
             unifyCons' (env, denv) d1 d2
             @ unifyCons' (env, denv) r1 r2
           | (L'.TCFun (expl1, x1, d1, r1), L'.TCFun (expl2, _, d2, r2)) =>
             if expl1 <> expl2 then
                 err CExplicitness
             else
                 (unifyKinds d1 d2;
                  unifyCons' (E.pushCRel env x1 d1, D.enter denv) r1 r2)
           | (L'.TRecord r1, L'.TRecord r2) => unifyCons' (env, denv) r1 r2

           | (L'.CRel n1, L'.CRel n2) =>
             if n1 = n2 then
                 []
             else
                 err CIncompatible
           | (L'.CNamed n1, L'.CNamed n2) =>
             if n1 = n2 then
                 []
             else
                 err CIncompatible

           | (L'.CApp (d1, r1), L'.CApp (d2, r2)) =>
             (unifyCons' (env, denv) d1 d2;
              unifyCons' (env, denv) r1 r2)
           | (L'.CAbs (x1, k1, c1), L'.CAbs (_, k2, c2)) =>
             (unifyKinds k1 k2;
              unifyCons' (E.pushCRel env x1 k1, D.enter denv) c1 c2)

           | (L'.CName n1, L'.CName n2) =>
             if n1 = n2 then
                 []
             else
                 err CIncompatible

           | (L'.CModProj (n1, ms1, x1), L'.CModProj (n2, ms2, x2)) =>
             if n1 = n2 andalso ms1 = ms2 andalso x1 = x2 then
                 []
             else
                 err CIncompatible

           | (L'.CTuple cs1, L'.CTuple cs2) =>
             ((ListPair.foldlEq (fn (c1, c2, gs) =>
                                    let
                                        val gs' = unifyCons' (env, denv) c1 c2
                                    in
                                        gs' @ gs
                                    end) [] (cs1, cs2))
              handle ListPair.UnequalLengths => err CIncompatible)

           | (L'.CProj ((L'.CUnif (_, _, _, ref (SOME c1)), loc), n1), _) =>
             unifyCons' (env, denv) (L'.CProj (c1, n1), loc) c2All
           | (_, L'.CProj ((L'.CUnif (_, _, _, ref (SOME c2)), loc), n2)) =>
             unifyCons' (env, denv) c1All (L'.CProj (c2, n2), loc)
           | (L'.CProj ((L'.CUnif (_, (L'.KTuple ks, _), _, r), loc), n), _) =>
             let
                 val us = map (fn k => cunif (loc, k)) ks
             in
                 r := SOME (L'.CTuple us, loc);
                 unifyCons' (env, denv) (List.nth (us, n - 1)) c2All
             end
           | (_, L'.CProj ((L'.CUnif (_, (L'.KTuple ks, _), _, r), loc), n)) =>
             let
                 val us = map (fn k => cunif (loc, k)) ks
             in
                 r := SOME (L'.CTuple us, loc);
                 unifyCons' (env, denv) c1All (List.nth (us, n - 1))
             end
           | (L'.CProj (c1, n1), L'.CProj (c2, n2)) =>
             if n1 = n2 then
                 unifyCons' (env, denv) c1 c2
             else
                 err CIncompatible

           | (L'.CError, _) => []
           | (_, L'.CError) => []

           | (L'.CRecord _, _) => isRecord ()
           | (_, L'.CRecord _) => isRecord ()
           | (L'.CConcat _, _) => isRecord ()
           | (_, L'.CConcat _) => isRecord ()

           | (L'.CUnif (_, k1, _, r1), L'.CUnif (_, k2, _, r2)) =>
             if r1 = r2 then
                 []
             else
                 (unifyKinds k1 k2;
                  r1 := SOME c2All;
                  [])

           | (L'.CUnif (_, _, _, r), _) =>
             if occursCon r c2All then
                 err COccursCheckFailed
             else
                 (r := SOME c2All;
                  [])
           | (_, L'.CUnif (_, _, _, r)) =>
             if occursCon r c1All then
                 err COccursCheckFailed
             else
                 (r := SOME c1All;
                  [])

           | (L'.CFold (dom1, ran1), L'.CFold (dom2, ran2)) =>
             (unifyKinds dom1 dom2;
              unifyKinds ran1 ran2;
              [])

           | _ => err CIncompatible
     end

 and unifyCons (env, denv) c1 c2 =
     unifyCons' (env, denv) c1 c2
     handle CUnify' err => raise CUnify (c1, c2, err)
          | KUnify args => raise CUnify (c1, c2, CKind args)

 fun checkCon (env, denv) e c1 c2 =
     unifyCons (env, denv) c1 c2
     handle CUnify (c1, c2, err) =>
            (expError env (Unify (e, c1, c2, err));
             [])

 fun checkPatCon (env, denv) p c1 c2 =
     unifyCons (env, denv) c1 c2
     handle CUnify (c1, c2, err) =>
            (expError env (PatUnify (p, c1, c2, err));
             [])

 fun primType env p =
     case p of
         P.Int _ => !int
       | P.Float _ => !float
       | P.String _ => !string

 fun recCons (k, nm, v, rest, loc) =
     (L'.CConcat ((L'.CRecord (k, [(nm, v)]), loc),
                  rest), loc)

 fun foldType (dom, loc) =
     (L'.TCFun (L'.Explicit, "ran", (L'.KArrow ((L'.KRecord dom, loc), (L'.KType, loc)), loc),
                (L'.TFun ((L'.TCFun (L'.Explicit, "nm", (L'.KName, loc),
                                     (L'.TCFun (L'.Explicit, "v", dom,
                                                (L'.TCFun (L'.Explicit, "rest", (L'.KRecord dom, loc),
                                                           (L'.TFun ((L'.CApp ((L'.CRel 3, loc), (L'.CRel 0, loc)), loc),
                                                                     (L'.CDisjoint (L'.Instantiate,
                                                                                    (L'.CRecord
                                                                                         ((L'.KUnit, loc),
                                                                                          [((L'.CRel 2, loc),
                                                                                            (L'.CUnit, loc))]), loc),
                                                                                    (L'.CRel 0, loc),
                                                                                    (L'.CApp ((L'.CRel 3, loc),
                                                                                              recCons (dom,
                                                                                                       (L'.CRel 2, loc),
                                                                                                       (L'.CRel 1, loc),
                                                                                                       (L'.CRel 0, loc),
                                                                                                       loc)), loc)),
                                                                      loc)), loc)),
                                                 loc)), loc)), loc),
                          (L'.TFun ((L'.CApp ((L'.CRel 0, loc), (L'.CRecord (dom, []), loc)), loc),
                                    (L'.TCFun (L'.Explicit, "r", (L'.KRecord dom, loc),
                                               (L'.CApp ((L'.CRel 1, loc), (L'.CRel 0, loc)), loc)), loc)),
                           loc)), loc)), loc)

 datatype constraint =
          Disjoint of D.goal
        | TypeClass of E.env * L'.con * L'.exp option ref * ErrorMsg.span

 val enD = map Disjoint

 fun elabHead (env, denv) infer (e as (_, loc)) t =
     let
         fun unravel (t, e) =
             let
                 val (t, gs) = hnormCon (env, denv) t
             in
                 case t of
                     (L'.TCFun (L'.Implicit, x, k, t'), _) =>
                     let
                         val u = cunif (loc, k)

                         val t'' = subConInCon (0, u) t'
                         val (e, t, gs') = unravel (t'', (L'.ECApp (e, u), loc))
                     in
                         (*prefaces "Unravel" [("t'", p_con env t'),
                                             ("t''", p_con env t'')];*)
                         (e, t, enD gs @ gs')
                     end
                   | (L'.TFun (dom as (L'.CApp (cl, _), _), ran), _) =>
                     let
                         val cl = #1 (hnormCon (env, denv) cl)
                     in
                         if infer <> L.TypesOnly andalso E.isClass env cl then
                             let
                                 val r = ref NONE
                                 val (e, t, gs') = unravel (ran, (L'.EApp (e, (L'.EUnif r, loc)), loc))
                             in
                                 (e, t, TypeClass (env, dom, r, loc) :: enD gs @ gs')
                             end
                         else
                             (e, t, enD gs)
                     end
                  | _ => (e, t, enD gs)
            end
    in
        case infer of
            L.DontInfer => (e, t, [])
          | _ => unravel (t, e)
    end

fun elabPat (pAll as (p, loc), (env, denv, bound)) =
    let
        val perror = (L'.PWild, loc)
        val terror = (L'.CError, loc)
        val pterror = (perror, terror)
        val rerror = (pterror, (env, bound))

        fun pcon (pc, po, xs, to, dn, dk) =
            case (po, to) of
                (NONE, SOME _) => (expError env (PatHasNoArg loc);
                                   rerror)
              | (SOME _, NONE) => (expError env (PatHasArg loc);
                                   rerror)
              | (NONE, NONE) =>
                let
                    val k = (L'.KType, loc)
                    val unifs = map (fn _ => cunif (loc, k)) xs
                    val dn = foldl (fn (u, dn) => (L'.CApp (dn, u), loc)) dn unifs
                in
                    (((L'.PCon (dk, pc, unifs, NONE), loc), dn),
                     (env, bound))
                end
              | (SOME p, SOME t) =>
                let
                    val ((p', pt), (env, bound)) = elabPat (p, (env, denv, bound))

                    val k = (L'.KType, loc)
                    val unifs = map (fn _ => cunif (loc, k)) xs
                    val nxs = length unifs - 1
                    val t = ListUtil.foldli (fn (i, u, t) => subConInCon (nxs - i, u) t) t unifs
                    val dn = foldl (fn (u, dn) => (L'.CApp (dn, u), loc)) dn unifs
                in
                    ignore (checkPatCon (env, denv) p' pt t);
                    (((L'.PCon (dk, pc, unifs, SOME p'), loc), dn),
                     (env, bound))
                end
    in
        case p of
            L.PWild => (((L'.PWild, loc), cunif (loc, (L'.KType, loc))),
                        (env, bound))
          | L.PVar x =>
            let
                val t = if SS.member (bound, x) then
                            (expError env (DuplicatePatternVariable (loc, x));
                             terror)
                        else
                            cunif (loc, (L'.KType, loc))
            in
                (((L'.PVar (x, t), loc), t),
                 (E.pushERel env x t, SS.add (bound, x)))
            end
          | L.PPrim p => (((L'.PPrim p, loc), primType env p),
                          (env, bound))
          | L.PCon ([], x, po) =>
            (case E.lookupConstructor env x of
                 NONE => (expError env (UnboundConstructor (loc, [], x));
                          rerror)
               | SOME (dk, n, xs, to, dn) => pcon (L'.PConVar n, po, xs, to, (L'.CNamed dn, loc), dk))
          | L.PCon (m1 :: ms, x, po) =>
            (case E.lookupStr env m1 of
                 NONE => (expError env (UnboundStrInExp (loc, m1));
                          rerror)
               | SOME (n, sgn) =>
                 let
                     val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                                case E.projectStr env {sgn = sgn, str = str, field = m} of
                                                    NONE => raise Fail "elabPat: Unknown substructure"
                                                  | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                            ((L'.StrVar n, loc), sgn) ms
                 in
                     case E.projectConstructor env {str = str, sgn = sgn, field = x} of
                         NONE => (expError env (UnboundConstructor (loc, m1 :: ms, x));
                                  rerror)
                       | SOME (dk, _, xs, to, dn) => pcon (L'.PConProj (n, ms, x), po, xs, to, dn, dk)
                 end)

          | L.PRecord (xps, flex) =>
            let
                val (xpts, (env, bound, _)) =
                    ListUtil.foldlMap (fn ((x, p), (env, bound, fbound)) =>
                                          let
                                              val ((p', t), (env, bound)) = elabPat (p, (env, denv, bound))
                                          in
                                              if SS.member (fbound, x) then
                                                  expError env (DuplicatePatField (loc, x))
                                              else
                                                  ();
                                              ((x, p', t), (env, bound, SS.add (fbound, x)))
                                          end)
                    (env, bound, SS.empty) xps

                val k = (L'.KType, loc)
                val c = (L'.CRecord (k, map (fn (x, _, t) => ((L'.CName x, loc), t)) xpts), loc)
                val c =
                    if flex then
                        (L'.CConcat (c, cunif (loc, (L'.KRecord k, loc))), loc)
                    else
                        c
            in
                (((L'.PRecord xpts, loc),
                  (L'.TRecord c, loc)),
                 (env, bound))
            end
                                           
    end

datatype coverage =
         Wild
       | None
       | Datatype of coverage IM.map
       | Record of coverage SM.map list

fun c2s c =
    case c of
        Wild => "Wild"
      | None => "None"
      | Datatype _ => "Datatype"
      | Record _ => "Record"

fun exhaustive (env, denv, t, ps) =
    let
        fun depth (p, _) =
            case p of
                L'.PWild => 0
              | L'.PVar _ => 0
              | L'.PPrim _ => 0
              | L'.PCon (_, _, _, NONE) => 1
              | L'.PCon (_, _, _, SOME p) => 1 + depth p
              | L'.PRecord xps => foldl (fn ((_, p, _), n) => Int.max (depth p, n)) 0 xps

        val depth = 1 + foldl (fn (p, n) => Int.max (depth p, n)) 0 ps

        fun pcCoverage pc =
            case pc of
                L'.PConVar n => n
              | L'.PConProj (m1, ms, x) =>
                let
                    val (str, sgn) = E.chaseMpath env (m1, ms)
                in
                    case E.projectConstructor env {str = str, sgn = sgn, field = x} of
                        NONE => raise Fail "exhaustive: Can't project constructor"
                      | SOME (_, n, _, _, _) => n
                end

        fun coverage (p, _) =
            case p of
                L'.PWild => Wild
              | L'.PVar _ => Wild
              | L'.PPrim _ => None
              | L'.PCon (_, pc, _, NONE) => Datatype (IM.insert (IM.empty, pcCoverage pc, Wild))
              | L'.PCon (_, pc, _, SOME p) => Datatype (IM.insert (IM.empty, pcCoverage pc, coverage p))
              | L'.PRecord xps => Record [foldl (fn ((x, p, _), fmap) =>
                                                    SM.insert (fmap, x, coverage p)) SM.empty xps]

        fun merge (c1, c2) =
            case (c1, c2) of
                (None, _) => c2
              | (_, None) => c1
                
              | (Wild, _) => Wild
              | (_, Wild) => Wild

              | (Datatype cm1, Datatype cm2) => Datatype (IM.unionWith merge (cm1, cm2))

              | (Record fm1, Record fm2) => Record (fm1 @ fm2)

              | _ => None

        fun combinedCoverage ps =
            case ps of
                [] => raise Fail "Empty pattern list for coverage checking"
              | [p] => coverage p
              | p :: ps => merge (coverage p, combinedCoverage ps)

        fun enumerateCases depth t =
            if depth = 0 then
                [Wild]
            else
                let
                    fun dtype cons =
                        ListUtil.mapConcat (fn (_, n, to) =>
                                               case to of
                                                   NONE => [Datatype (IM.insert (IM.empty, n, Wild))]
                                                 | SOME t => map (fn c => Datatype (IM.insert (IM.empty, n, c)))
                                                                 (enumerateCases (depth-1) t)) cons
                in
                    case #1 (#1 (hnormCon (env, denv) t)) of
                        L'.CNamed n =>
                        (let
                             val dt = E.lookupDatatype env n
                             val cons = E.constructors dt
                         in
                             dtype cons
                         end handle E.UnboundNamed _ => [Wild])
                      | L'.TRecord c =>
                        (case #1 (#1 (hnormCon (env, denv) c)) of
                             L'.CRecord (_, xts) =>
                             let
                                 val xts = map (fn (x, t) => (#1 (hnormCon (env, denv) x), t)) xts

                                 fun exponentiate fs =
                                     case fs of
                                         [] => [SM.empty]
                                       | ((L'.CName x, _), t) :: rest =>
                                         let
                                             val this = enumerateCases depth t
                                             val rest = exponentiate rest
                                         in
                                             ListUtil.mapConcat (fn fmap =>
                                                                    map (fn c => SM.insert (fmap, x, c)) this) rest
                                         end
                                       | _ => raise Fail "exponentiate: Not CName"
                             in
                                 if List.exists (fn ((L'.CName _, _), _) => false
                                                  | (c, _) => true) xts then
                                     [Wild]
                                 else
                                     map (fn ls => Record [ls]) (exponentiate xts)
                             end
                           | _ => [Wild])
                      | _ => [Wild]
                end

        fun coverageImp (c1, c2) =
            let
                val r =
                    case (c1, c2) of
                        (Wild, _) => true

                      | (Datatype cmap1, Datatype cmap2) =>
                        List.all (fn (n, c2) =>
                                     case IM.find (cmap1, n) of
                                         NONE => false
                                       | SOME c1 => coverageImp (c1, c2)) (IM.listItemsi cmap2)
                      | (Datatype cmap1, Wild) =>
                        List.all (fn (n, c1) => coverageImp (c1, Wild)) (IM.listItemsi cmap1)

                      | (Record fmaps1, Record fmaps2) =>
                        List.all (fn fmap2 =>
                                     List.exists (fn fmap1 =>
                                                     List.all (fn (x, c2) =>
                                                                  case SM.find (fmap1, x) of
                                                                      NONE => true
                                                                    | SOME c1 => coverageImp (c1, c2))
                                                              (SM.listItemsi fmap2))
                                                 fmaps1) fmaps2

                      | (Record fmaps1, Wild) =>
                        List.exists (fn fmap1 =>
                                        List.all (fn (x, c1) => coverageImp (c1, Wild))
                                        (SM.listItemsi fmap1)) fmaps1

                      | _ => false
            in
                (*TextIO.print ("coverageImp(" ^ c2s c1 ^ ", " ^ c2s c2 ^ ") = " ^ Bool.toString r ^ "\n");*)
                r
            end

        fun isTotal (c, t) =
            case c of
                None => (false, [])
              | Wild => (true, [])
              | Datatype cm =>
                let
                    val ((t, _), gs) = hnormCon (env, denv) t

                    fun dtype cons =
                        foldl (fn ((_, n, to), (total, gs)) =>
                                  case IM.find (cm, n) of
                                      NONE => (false, gs)
                                    | SOME c' =>
                                      case to of
                                          NONE => (total, gs)
                                        | SOME t' =>
                                          let
                                              val (total, gs') = isTotal (c', t')
                                          in
                                              (total, gs' @ gs)
                                          end)
                              (true, gs) cons

                    fun unapp t =
                        case t of
                            L'.CApp ((t, _), _) => unapp t
                          | _ => t
                in
                    case unapp t of
                        L'.CNamed n =>
                        let
                            val dt = E.lookupDatatype env n
                            val cons = E.constructors dt
                        in
                            dtype cons
                        end
                      | L'.CModProj (m1, ms, x) =>
                        let
                            val (str, sgn) = E.chaseMpath env (m1, ms)
                        in
                            case E.projectDatatype env {str = str, sgn = sgn, field = x} of
                                NONE => raise Fail "isTotal: Can't project datatype"
                              | SOME (_, cons) => dtype cons
                        end
                      | L'.CError => (true, gs)
                      | c =>
                        (prefaces "Not a datatype" [("c", p_con env (c, ErrorMsg.dummySpan))];
                         raise Fail "isTotal: Not a datatype")
                end
              | Record _ => (List.all (fn c2 => coverageImp (c, c2)) (enumerateCases depth t), [])
    in
        isTotal (combinedCoverage ps, t)
    end

fun unmodCon env (c, loc) =
    case c of
        L'.CNamed n =>
        (case E.lookupCNamed env n of
             (_, _, SOME (c as (L'.CModProj _, _))) => unmodCon env c
           | _ => (c, loc))
      | L'.CModProj (m1, ms, x) =>
        let
            val (str, sgn) = E.chaseMpath env (m1, ms)
        in
            case E.projectCon env {str = str, sgn = sgn, field = x} of
                NONE => raise Fail "unmodCon: Can't projectCon"
              | SOME (_, SOME (c as (L'.CModProj _, _))) => unmodCon env c
              | _ => (c, loc)
        end
      | _ => (c, loc)

fun normClassConstraint envs (c, loc) =
    case c of
        L'.CApp (f, x) =>
        let
            val f = unmodCon (#1 envs) f
            val (x, gs) = hnormCon envs x
        in
            ((L'.CApp (f, x), loc), gs)
        end
      | _ => ((c, loc), [])

fun elabExp (env, denv) (eAll as (e, loc)) =
    let
        (*val () = eprefaces "elabExp" [("eAll", SourcePrint.p_exp eAll)];*)

        val r = case e of
            L.EAnnot (e, t) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (t', _, gs2) = elabCon (env, denv) t
                val gs3 = checkCon (env, denv) e' et t'
            in
                (e', t', gs1 @ enD gs2 @ enD gs3)
            end

          | L.EPrim p => ((L'.EPrim p, loc), primType env p, [])
          | L.EVar ([], s, infer) =>
            (case E.lookupE env s of
                 E.NotBound =>
                 (expError env (UnboundExp (loc, s));
                  (eerror, cerror, []))
               | E.Rel (n, t) => elabHead (env, denv) infer (L'.ERel n, loc) t
               | E.Named (n, t) => elabHead (env, denv) infer (L'.ENamed n, loc) t)
          | L.EVar (m1 :: ms, s, infer) =>
            (case E.lookupStr env m1 of
                 NONE => (expError env (UnboundStrInExp (loc, m1));
                          (eerror, cerror, []))
               | SOME (n, sgn) =>
                 let
                     val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                                case E.projectStr env {sgn = sgn, str = str, field = m} of
                                                    NONE => (conError env (UnboundStrInCon (loc, m));
                                                             (strerror, sgnerror))
                                                  | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                            ((L'.StrVar n, loc), sgn) ms

                     val t = case E.projectVal env {sgn = sgn, str = str, field = s} of
                                 NONE => (expError env (UnboundExp (loc, s));
                                          cerror)
                               | SOME t => t
                 in
                     elabHead (env, denv) infer (L'.EModProj (n, ms, s), loc) t
                 end)

          | L.EWild =>
            let
                val r = ref NONE
                val c = cunif (loc, (L'.KType, loc))
            in
                ((L'.EUnif r, loc), c, [TypeClass (env, c, r, loc)])
            end

          | L.EApp (e1, e2) =>
            let
                val (e1', t1, gs1) = elabExp (env, denv) e1
                val (e2', t2, gs2) = elabExp (env, denv) e2

                val dom = cunif (loc, ktype)
                val ran = cunif (loc, ktype)
                val t = (L'.TFun (dom, ran), dummy)

                val gs3 = checkCon (env, denv) e1' t1 t
                val gs4 = checkCon (env, denv) e2' t2 dom

                val gs = gs1 @ gs2 @ enD gs3 @ enD gs4
            in
                ((L'.EApp (e1', e2'), loc), ran, gs)
            end
          | L.EAbs (x, to, e) =>
            let
                val (t', gs1) = case to of
                                    NONE => (cunif (loc, ktype), [])
                                  | SOME t =>
                                    let
                                        val (t', tk, gs) = elabCon (env, denv) t
                                    in
                                        checkKind env t' tk ktype;
                                        (t', gs)
                                    end
                val (dom, gs2) = normClassConstraint (env, denv) t'
                val (e', et, gs3) = elabExp (E.pushERel env x dom, denv) e
            in
                ((L'.EAbs (x, t', et, e'), loc),
                 (L'.TFun (t', et), loc),
                 enD gs1 @ enD gs2 @ gs3)
            end
          | L.ECApp (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val oldEt = et
                val (c', ck, gs2) = elabCon (env, denv) c
                val ((et', _), gs3) = hnormCon (env, denv) et
            in
                case et' of
                    L'.CError => (eerror, cerror, [])
                  | L'.TCFun (_, x, k, eb) =>
                    let
                        val () = checkKind env c' ck k
                        val eb' = subConInCon (0, c') eb
                            handle SynUnif => (expError env (Unif ("substitution", loc, eb));
                                               cerror)
                    in
                        (*prefaces "Elab ECApp" [("e", SourcePrint.p_exp eAll),
                                               ("et", p_con env oldEt),
                                               ("x", PD.string x),
                                               ("eb", p_con (E.pushCRel env x k) eb),
                                               ("c", p_con env c'),
                                               ("eb'", p_con env eb')];*)
                        ((L'.ECApp (e', c'), loc), eb', gs1 @ enD gs2 @ enD gs3)
                    end

                  | _ =>
                    (expError env (WrongForm ("constructor function", e', et));
                     (eerror, cerror, []))
            end
          | L.ECAbs (expl, x, k, e) =>
            let
                val expl' = elabExplicitness expl
                val k' = elabKind k

                val env' = E.pushCRel env x k'
                val (e', et, gs) = elabExp (env', D.enter denv) e
            in
                ((L'.ECAbs (expl', x, k', e'), loc),
                 (L'.TCFun (expl', x, k', et), loc),
                 gs)
            end

          | L.EDisjoint (c1, c2, e) =>
            let
                val (c1', k1, gs1) = elabCon (env, denv) c1
                val (c2', k2, gs2) = elabCon (env, denv) c2

                val ku1 = kunif loc
                val ku2 = kunif loc

                val (denv', gs3) = D.assert env denv (c1', c2')
                val (e', t, gs4) = elabExp (env, denv') e
            in
                checkKind env c1' k1 (L'.KRecord ku1, loc);
                checkKind env c2' k2 (L'.KRecord ku2, loc);

                (e', (L'.CDisjoint (L'.LeaveAlone, c1', c2', t), loc), enD gs1 @ enD gs2 @ enD gs3 @ gs4)
            end

          | L.ERecord xes =>
            let
                val (xes', gs) = ListUtil.foldlMap (fn ((x, e), gs) =>
                                                       let
                                                           val (x', xk, gs1) = elabCon (env, denv) x
                                                           val (e', et, gs2) = elabExp (env, denv) e
                                                       in
                                                           checkKind env x' xk kname;
                                                           ((x', e', et), enD gs1 @ gs2 @ gs)
                                                       end)
                                                   [] xes

                val k = (L'.KType, loc)

                fun prove (xets, gs) =
                    case xets of
                        [] => gs
                      | (x, _, t) :: rest =>
                        let
                            val xc = (x, t)
                            val r1 = (L'.CRecord (k, [xc]), loc)
                            val gs = foldl (fn ((x', _, t'), gs) =>
                                               let
                                                   val xc' = (x', t')
                                                   val r2 = (L'.CRecord (k, [xc']), loc)
                                               in
                                                   D.prove env denv (r1, r2, loc) @ gs
                                               end)
                                           gs rest
                        in
                            prove (rest, gs)
                        end

                val gsD = List.mapPartial (fn Disjoint d => SOME d | _ => NONE) gs
                val gsO = List.filter (fn Disjoint _ => false | _ => true) gs
            in
                (*TextIO.print ("|gsO| = " ^ Int.toString (length gsO) ^ "\n");*)
                ((L'.ERecord xes', loc),
                 (L'.TRecord (L'.CRecord (ktype, map (fn (x', _, et) => (x', et)) xes'), loc), loc),
                 enD (prove (xes', gsD)) @ gsO)
            end

          | L.EField (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (c', ck, gs2) = elabCon (env, denv) c

                val ft = cunif (loc, ktype)
                val rest = cunif (loc, ktype_record)
                val first = (L'.CRecord (ktype, [(c', ft)]), loc)
                            
                val gs3 =
                    checkCon (env, denv) e' et
                             (L'.TRecord (L'.CConcat (first, rest), loc), loc)
                val gs4 = D.prove env denv (first, rest, loc)
            in
                ((L'.EField (e', c', {field = ft, rest = rest}), loc), ft, gs1 @ enD gs2 @ enD gs3 @ enD gs4)
            end

          | L.EConcat (e1, e2) =>
            let
                val (e1', e1t, gs1) = elabExp (env, denv) e1
                val (e2', e2t, gs2) = elabExp (env, denv) e2

                val r1 = cunif (loc, ktype_record)
                val r2 = cunif (loc, ktype_record)

                val gs3 = checkCon (env, denv) e1' e1t (L'.TRecord r1, loc)
                val gs4 = checkCon (env, denv) e2' e2t (L'.TRecord r2, loc)
                val gs5 = D.prove env denv (r1, r2, loc)
            in
                ((L'.EConcat (e1', r1, e2', r2), loc),
                 (L'.TRecord ((L'.CConcat (r1, r2), loc)), loc),
                 gs1 @ gs2 @ enD gs3 @ enD gs4 @ enD gs5)
            end
          | L.ECut (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (c', ck, gs2) = elabCon (env, denv) c

                val ft = cunif (loc, ktype)
                val rest = cunif (loc, ktype_record)
                val first = (L'.CRecord (ktype, [(c', ft)]), loc)
                            
                val gs3 =
                    checkCon (env, denv) e' et
                             (L'.TRecord (L'.CConcat (first, rest), loc), loc)
                val gs4 = D.prove env denv (first, rest, loc)
            in
                ((L'.ECut (e', c', {field = ft, rest = rest}), loc), (L'.TRecord rest, loc),
                 gs1 @ enD gs2 @ enD gs3 @ enD gs4)
            end

          | L.EFold =>
            let
                val dom = kunif loc
            in
                ((L'.EFold dom, loc), foldType (dom, loc), [])
            end

          | L.ECase (e, pes) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val result = cunif (loc, (L'.KType, loc))
                val (pes', gs) = ListUtil.foldlMap
                                 (fn ((p, e), gs) =>
                                     let
                                         val ((p', pt), (env, _)) = elabPat (p, (env, denv, SS.empty))

                                         val gs1 = checkPatCon (env, denv) p' pt et
                                         val (e', et, gs2) = elabExp (env, denv) e
                                         val gs3 = checkCon (env, denv) e' et result
                                     in
                                         ((p', e'), enD gs1 @ gs2 @ enD gs3 @ gs)
                                     end)
                                 gs1 pes

                val (total, gs') = exhaustive (env, denv, et, map #1 pes')
            in
                if total then
                    ()
                else
                    expError env (Inexhaustive loc);

                ((L'.ECase (e', pes', {disc = et, result = result}), loc), result, enD gs' @ gs)
            end
    in
        (*prefaces "/elabExp" [("e", SourcePrint.p_exp eAll)];*)
        r
    end

val hnormSgn = E.hnormSgn

fun tableOf () = (L'.CModProj (!basis_r, [], "sql_table"), ErrorMsg.dummySpan)
fun sequenceOf () = (L'.CModProj (!basis_r, [], "sql_sequence"), ErrorMsg.dummySpan)

fun elabSgn_item ((sgi, loc), (env, denv, gs)) =
    case sgi of
        L.SgiConAbs (x, k) =>
        let
            val k' = elabKind k

            val (env', n) = E.pushCNamed env x k' NONE
        in
            ([(L'.SgiConAbs (x, n, k'), loc)], (env', denv, gs))
        end

      | L.SgiCon (x, ko, c) =>
        let
            val k' = case ko of
                         NONE => kunif loc
                       | SOME k => elabKind k

            val (c', ck, gs') = elabCon (env, denv) c
            val (env', n) = E.pushCNamed env x k' (SOME c')
        in
            checkKind env c' ck k';

            ([(L'.SgiCon (x, n, k', c'), loc)], (env', denv, gs' @ gs))
        end

      | L.SgiDatatype (x, xs, xcs) =>
        let
            val k = (L'.KType, loc)
            val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
            val (env, n) = E.pushCNamed env x k' NONE
            val t = (L'.CNamed n, loc)
            val nxs = length xs - 1
            val t = ListUtil.foldli (fn (i, _, t) => (L'.CApp (t, (L'.CRel (nxs - i), loc)), loc)) t xs

            val (env', denv') = foldl (fn (x, (env', denv')) =>
                                          (E.pushCRel env' x k,
                                           D.enter denv')) (env, denv) xs

            val (xcs, (used, env, gs)) =
                ListUtil.foldlMap
                (fn ((x, to), (used, env, gs)) =>
                    let
                        val (to, t, gs') = case to of
                                           NONE => (NONE, t, gs)
                                         | SOME t' =>
                                           let
                                               val (t', tk, gs') = elabCon (env', denv') t'
                                           in
                                               checkKind env' t' tk k;
                                               (SOME t', (L'.TFun (t', t), loc), gs' @ gs)
                                           end
                        val t = foldl (fn (x, t) => (L'.TCFun (L'.Implicit, x, k, t), loc)) t xs

                        val (env, n') = E.pushENamed env x t
                    in
                        if SS.member (used, x) then
                            strError env (DuplicateConstructor (x, loc))
                        else
                            ();
                        ((x, n', to), (SS.add (used, x), env, gs'))
                    end)
                (SS.empty, env, []) xcs

            val env = E.pushDatatype env n xs xcs
        in
            ([(L'.SgiDatatype (x, n, xs, xcs), loc)], (env, denv, gs))
        end

      | L.SgiDatatypeImp (_, [], _) => raise Fail "Empty SgiDatatypeImp"

      | L.SgiDatatypeImp (x, m1 :: ms, s) =>
        (case E.lookupStr env m1 of
             NONE => (strError env (UnboundStr (loc, m1));
                      ([], (env, denv, gs)))
           | SOME (n, sgn) =>
             let
                 val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                     case E.projectStr env {sgn = sgn, str = str, field = m} of
                                         NONE => (conError env (UnboundStrInCon (loc, m));
                                                  (strerror, sgnerror))
                                       | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                  ((L'.StrVar n, loc), sgn) ms
             in
                 case hnormCon (env, denv) (L'.CModProj (n, ms, s), loc) of
                     ((L'.CModProj (n, ms, s), _), gs) =>
                     (case E.projectDatatype env {sgn = sgn, str = str, field = s} of
                          NONE => (conError env (UnboundDatatype (loc, s));
                                   ([], (env, denv, gs)))
                        | SOME (xs, xncs) =>
                          let
                              val k = (L'.KType, loc)
                              val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs

                              val t = (L'.CModProj (n, ms, s), loc)
                              val (env, n') = E.pushCNamed env x k' (SOME t)
                              val env = E.pushDatatype env n' xs xncs

                              val t = (L'.CNamed n', loc)
                              val env = foldl (fn ((x, n, to), env) =>
                                                  let
                                                      val t = case to of
                                                                  NONE => t
                                                                | SOME t' => (L'.TFun (t', t), loc)

                                                      val t = foldr (fn (x, t) =>
                                                                        (L'.TCFun (L'.Implicit, x, k, t), loc))
                                                              t xs
                                                  in
                                                      E.pushENamedAs env x n t
                                                  end) env xncs
                          in
                              ([(L'.SgiDatatypeImp (x, n', n, ms, s, xs, xncs), loc)], (env, denv, gs))
                          end)
                   | _ => (strError env (NotDatatype loc);
                           ([], (env, denv, [])))
             end)

      | L.SgiVal (x, c) =>
        let
            val (c', ck, gs') = elabCon (env, denv) c

            val (env', n) = E.pushENamed env x c'
            val (c', gs'') = normClassConstraint (env, denv) c'
        in
            (unifyKinds ck ktype
             handle KUnify ue => strError env (NotType (ck, ue)));

            ([(L'.SgiVal (x, n, c'), loc)], (env', denv, gs' @ gs'' @ gs))
        end

      | L.SgiStr (x, sgn) =>
        let
            val (sgn', gs') = elabSgn (env, denv) sgn
            val (env', n) = E.pushStrNamed env x sgn'
        in
            ([(L'.SgiStr (x, n, sgn'), loc)], (env', denv, gs' @ gs))
        end

      | L.SgiSgn (x, sgn) =>
        let
            val (sgn', gs') = elabSgn (env, denv) sgn
            val (env', n) = E.pushSgnNamed env x sgn'
        in
            ([(L'.SgiSgn (x, n, sgn'), loc)], (env', denv, gs' @ gs))
        end

      | L.SgiInclude sgn =>
        let
            val (sgn', gs') = elabSgn (env, denv) sgn
        in
            case #1 (hnormSgn env sgn') of
                L'.SgnConst sgis =>
                (sgis, (foldl (fn (sgi, env) => E.sgiBinds env sgi) env sgis, denv, gs' @ gs))
              | _ => (sgnError env (NotIncludable sgn');
                      ([], (env, denv, [])))
        end

      | L.SgiConstraint (c1, c2) =>
        let
            val (c1', k1, gs1) = elabCon (env, denv) c1
            val (c2', k2, gs2) = elabCon (env, denv) c2

            val (denv, gs3) = D.assert env denv (c1', c2')
        in
            checkKind env c1' k1 (L'.KRecord (kunif loc), loc);
            checkKind env c2' k2 (L'.KRecord (kunif loc), loc);

            ([(L'.SgiConstraint (c1', c2'), loc)], (env, denv, gs1 @ gs2 @ gs3))
        end

      | L.SgiTable (x, c) =>
        let
            val (c', k, gs) = elabCon (env, denv) c
            val (env, n) = E.pushENamed env x (L'.CApp (tableOf (), c'), loc)
        in
            checkKind env c' k (L'.KRecord (L'.KType, loc), loc);
            ([(L'.SgiTable (!basis_r, x, n, c'), loc)], (env, denv, gs))
        end

      | L.SgiSequence x =>
        let
            val (env, n) = E.pushENamed env x (sequenceOf ())
        in
            ([(L'.SgiSequence (!basis_r, x, n), loc)], (env, denv, gs))
        end

      | L.SgiClassAbs x =>
        let
            val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
            val (env, n) = E.pushCNamed env x k NONE
            val env = E.pushClass env n
        in
            ([(L'.SgiClassAbs (x, n), loc)], (env, denv, []))
        end

      | L.SgiClass (x, c) =>
        let
            val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
            val (c', ck, gs) = elabCon (env, denv) c
            val (env, n) = E.pushCNamed env x k (SOME c')
            val env = E.pushClass env n
        in
            checkKind env c' ck k;
            ([(L'.SgiClass (x, n, c'), loc)], (env, denv, []))
        end

and elabSgn (env, denv) (sgn, loc) =
    case sgn of
        L.SgnConst sgis =>
        let
            val (sgis', (_, _, gs)) = ListUtil.foldlMapConcat elabSgn_item (env, denv, []) sgis

            val _ = foldl (fn ((sgi, loc), (cons, vals, sgns, strs)) =>
                              case sgi of
                                  L'.SgiConAbs (x, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiCon (x, _, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiDatatype (x, _, _, xncs) =>
                                  let
                                      val vals = foldl (fn ((x, _, _), vals) =>
                                                           (if SS.member (vals, x) then
                                                                sgnError env (DuplicateVal (loc, x))
                                                            else
                                                                ();
                                                            SS.add (vals, x)))
                                                       vals xncs
                                  in
                                      if SS.member (cons, x) then
                                          sgnError env (DuplicateCon (loc, x))
                                      else
                                          ();
                                      (SS.add (cons, x), vals, sgns, strs)
                                  end
                                | L'.SgiDatatypeImp (x, _, _, _, _, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiVal (x, _, _) =>
                                  (if SS.member (vals, x) then
                                       sgnError env (DuplicateVal (loc, x))
                                   else
                                       ();
                                   (cons, SS.add (vals, x), sgns, strs))
                                | L'.SgiSgn (x, _, _) =>
                                  (if SS.member (sgns, x) then
                                       sgnError env (DuplicateSgn (loc, x))
                                   else
                                       ();
                                   (cons, vals, SS.add (sgns, x), strs))
                                | L'.SgiStr (x, _, _) =>
                                  (if SS.member (strs, x) then
                                       sgnError env (DuplicateStr (loc, x))
                                   else
                                       ();
                                   (cons, vals, sgns, SS.add (strs, x)))
                                | L'.SgiConstraint _ => (cons, vals, sgns, strs)
                                | L'.SgiTable (_, x, _, _) =>
                                  (if SS.member (vals, x) then
                                       sgnError env (DuplicateVal (loc, x))
                                   else
                                       ();
                                   (cons, SS.add (vals, x), sgns, strs))
                                | L'.SgiSequence (_, x, _) =>
                                  (if SS.member (vals, x) then
                                       sgnError env (DuplicateVal (loc, x))
                                   else
                                       ();
                                   (cons, SS.add (vals, x), sgns, strs))
                                | L'.SgiClassAbs (x, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiClass (x, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs)))
                    (SS.empty, SS.empty, SS.empty, SS.empty) sgis'
        in
            ((L'.SgnConst sgis', loc), gs)
        end
      | L.SgnVar x =>
        (case E.lookupSgn env x of
             NONE =>
             (sgnError env (UnboundSgn (loc, x));
              ((L'.SgnError, loc), []))
           | SOME (n, sgis) => ((L'.SgnVar n, loc), []))
      | L.SgnFun (m, dom, ran) =>
        let
            val (dom', gs1) = elabSgn (env, denv) dom
            val (env', n) = E.pushStrNamed env m dom'
            val (ran', gs2) = elabSgn (env', denv) ran
        in
            ((L'.SgnFun (m, n, dom', ran'), loc), gs1 @ gs2)
        end
      | L.SgnWhere (sgn, x, c) =>
        let
            val (sgn', ds1) = elabSgn (env, denv) sgn
            val (c', ck, ds2) = elabCon (env, denv) c
        in
            case #1 (hnormSgn env sgn') of
                L'.SgnError => (sgnerror, [])
              | L'.SgnConst sgis =>
                if List.exists (fn (L'.SgiConAbs (x', _, k), _) =>
                                   x' = x andalso
                                   (unifyKinds k ck
                                    handle KUnify x => sgnError env (WhereWrongKind x);
                                    true)
                                 | _ => false) sgis then
                    ((L'.SgnWhere (sgn', x, c'), loc), ds1 @ ds2)
                else
                    (sgnError env (UnWhereable (sgn', x));
                     (sgnerror, []))
              | _ => (sgnError env (UnWhereable (sgn', x));
                      (sgnerror, []))
        end
      | L.SgnProj (m, ms, x) =>
        (case E.lookupStr env m of
             NONE => (strError env (UnboundStr (loc, m));
                      (sgnerror, []))
           | SOME (n, sgn) =>
             let
                 val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                          case E.projectStr env {sgn = sgn, str = str, field = m} of
                                              NONE => (strError env (UnboundStr (loc, m));
                                                       (strerror, sgnerror))
                                            | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                ((L'.StrVar n, loc), sgn) ms
             in
                 case E.projectSgn env {sgn = sgn, str = str, field = x} of
                     NONE => (sgnError env (UnboundSgn (loc, x));
                              (sgnerror, []))
                   | SOME _ => ((L'.SgnProj (n, ms, x), loc), [])
             end)
                                                              

fun selfify env {str, strs, sgn} =
    case #1 (hnormSgn env sgn) of
        L'.SgnError => sgn
      | L'.SgnVar _ => sgn

      | L'.SgnConst sgis =>
        (L'.SgnConst (map (fn (L'.SgiConAbs (x, n, k), loc) =>
                              (L'.SgiCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc)
                            | (L'.SgiDatatype (x, n, xs, xncs), loc) =>
                              (L'.SgiDatatypeImp (x, n, str, strs, x, xs, xncs), loc)
                            | (L'.SgiClassAbs (x, n), loc) =>
                              (L'.SgiClass (x, n, (L'.CModProj (str, strs, x), loc)), loc)
                            | (L'.SgiStr (x, n, sgn), loc) =>
                              (L'.SgiStr (x, n, selfify env {str = str, strs = strs @ [x], sgn = sgn}), loc)
                            | x => x) sgis), #2 sgn)
      | L'.SgnFun _ => sgn
      | L'.SgnWhere _ => sgn
      | L'.SgnProj (m, ms, x) =>
        case E.projectSgn env {str = foldl (fn (m, str) => (L'.StrProj (str, m), #2 sgn))
                                           (L'.StrVar m, #2 sgn) ms,
                               sgn = #2 (E.lookupStrNamed env m),
                               field = x} of
            NONE => raise Fail "Elaborate.selfify: projectSgn returns NONE"
          | SOME sgn => selfify env {str = str, strs = strs, sgn = sgn}

fun selfifyAt env {str, sgn} =
    let
        fun self (str, _) =
            case str of
                L'.StrVar x => SOME (x, [])
              | L'.StrProj (str, x) =>
                (case self str of
                     NONE => NONE
                   | SOME (m, ms) => SOME (m, ms @ [x]))
              | _ => NONE
    in
        case self str of
            NONE => sgn
          | SOME (str, strs) => selfify env {sgn = sgn, str = str, strs = strs}
    end

fun dopen (env, denv) {str, strs, sgn} =
    let
        val m = foldl (fn (m, str) => (L'.StrProj (str, m), #2 sgn))
                (L'.StrVar str, #2 sgn) strs
    in
        case #1 (hnormSgn env sgn) of
            L'.SgnConst sgis =>
            ListUtil.foldlMap (fn ((sgi, loc), (env', denv')) =>
                                  let
                                      val d =
                                          case sgi of
                                              L'.SgiConAbs (x, n, k) =>
                                              let
                                                  val c = (L'.CModProj (str, strs, x), loc)
                                              in
                                                  (L'.DCon (x, n, k, c), loc)
                                              end
                                            | L'.SgiCon (x, n, k, c) =>
                                              (L'.DCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc)
                                            | L'.SgiDatatype (x, n, xs, xncs) =>
                                              (L'.DDatatypeImp (x, n, str, strs, x, xs, xncs), loc)
                                            | L'.SgiDatatypeImp (x, n, m1, ms, x', xs, xncs) =>
                                              (L'.DDatatypeImp (x, n, m1, ms, x', xs, xncs), loc)
                                            | L'.SgiVal (x, n, t) =>
                                              (L'.DVal (x, n, t, (L'.EModProj (str, strs, x), loc)), loc)
                                            | L'.SgiStr (x, n, sgn) =>
                                              (L'.DStr (x, n, sgn, (L'.StrProj (m, x), loc)), loc)
                                            | L'.SgiSgn (x, n, sgn) =>
                                              (L'.DSgn (x, n, (L'.SgnProj (str, strs, x), loc)), loc)
                                            | L'.SgiConstraint (c1, c2) =>
                                              (L'.DConstraint (c1, c2), loc)
                                            | L'.SgiTable (_, x, n, c) =>
                                              (L'.DVal (x, n, (L'.CApp (tableOf (), c), loc),
                                                        (L'.EModProj (str, strs, x), loc)), loc)
                                            | L'.SgiSequence (_, x, n) =>
                                              (L'.DVal (x, n, sequenceOf (),
                                                        (L'.EModProj (str, strs, x), loc)), loc)
                                            | L'.SgiClassAbs (x, n) =>
                                              let
                                                  val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
                                                  val c = (L'.CModProj (str, strs, x), loc)
                                              in
                                                  (L'.DCon (x, n, k, c), loc)
                                              end
                                            | L'.SgiClass (x, n, _) =>
                                              let
                                                  val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
                                                  val c = (L'.CModProj (str, strs, x), loc)
                                              in
                                                  (L'.DCon (x, n, k, c), loc)
                                              end
                                  in
                                      (d, (E.declBinds env' d, denv'))
                                  end)
                              (env, denv) sgis
          | _ => (strError env (UnOpenable sgn);
                  ([], (env, denv)))
    end

fun dopenConstraints (loc, env, denv) {str, strs} =
    case E.lookupStr env str of
        NONE => (strError env (UnboundStr (loc, str));
                 denv)
      | SOME (n, sgn) =>
        let
            val (st, sgn) = foldl (fn (m, (str, sgn)) =>
                                      case E.projectStr env {str = str, sgn = sgn, field = m} of
                                          NONE => (strError env (UnboundStr (loc, m));
                                                   (strerror, sgnerror))
                                        | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                  ((L'.StrVar n, loc), sgn) strs
                            
            val cso = E.projectConstraints env {sgn = sgn, str = st}

            val denv = case cso of
                           NONE => (strError env (UnboundStr (loc, str));
                                    denv)
                         | SOME cs => foldl (fn ((c1, c2), denv) =>
                                                let
                                                    val (denv, gs) = D.assert env denv (c1, c2)
                                                in
                                                    case gs of
                                                        [] => ()
                                                      | _ => raise Fail "dopenConstraints: Sub-constraints remain";

                                                    denv
                                                end) denv cs
        in
            denv
        end

fun sgiOfDecl (d, loc) =
    case d of
        L'.DCon (x, n, k, c) => [(L'.SgiCon (x, n, k, c), loc)]
      | L'.DDatatype x => [(L'.SgiDatatype x, loc)]
      | L'.DDatatypeImp x => [(L'.SgiDatatypeImp x, loc)]
      | L'.DVal (x, n, t, _) => [(L'.SgiVal (x, n, t), loc)]
      | L'.DValRec vis => map (fn (x, n, t, _) => (L'.SgiVal (x, n, t), loc)) vis
      | L'.DSgn (x, n, sgn) => [(L'.SgiSgn (x, n, sgn), loc)]
      | L'.DStr (x, n, sgn, _) => [(L'.SgiStr (x, n, sgn), loc)]
      | L'.DFfiStr (x, n, sgn) => [(L'.SgiStr (x, n, sgn), loc)]
      | L'.DConstraint cs => [(L'.SgiConstraint cs, loc)]
      | L'.DExport _ => []
      | L'.DTable (tn, x, n, c) => [(L'.SgiTable (tn, x, n, c), loc)]
      | L'.DSequence (tn, x, n) => [(L'.SgiSequence (tn, x, n), loc)]
      | L'.DClass (x, n, c) => [(L'.SgiClass (x, n, c), loc)]
      | L'.DDatabase _ => []

fun sgiBindsD (env, denv) (sgi, _) =
    case sgi of
        L'.SgiConstraint (c1, c2) =>
        (case D.assert env denv (c1, c2) of
             (denv, []) => denv
           | _ => raise Fail "sgiBindsD: Sub-constraints remain")
      | _ => denv

fun subSgn (env, denv) sgn1 (sgn2 as (_, loc2)) =
    case (#1 (hnormSgn env sgn1), #1 (hnormSgn env sgn2)) of
        (L'.SgnError, _) => ()
      | (_, L'.SgnError) => ()

      | (L'.SgnConst sgis1, L'.SgnConst sgis2) =>
        let
            (*val () = prefaces "subSgn" [("sgn1", p_sgn env sgn1),
                                        ("sgn2", p_sgn env sgn2),
                                        ("sgis1", p_sgn env (L'.SgnConst sgis1, loc2)),
                                        ("sgis2", p_sgn env (L'.SgnConst sgis2, loc2))]*)

            fun folder (sgi2All as (sgi, loc), (env, denv)) =
                let
                    (*val () = prefaces "folder" [("sgis1", p_sgn env (L'.SgnConst sgis1, loc2))]*)

                    fun seek p =
                        let
                            fun seek (env, denv) ls =
                                case ls of
                                    [] => (sgnError env (UnmatchedSgi sgi2All);
                                           (env, denv))
                                  | h :: t =>
                                    case p h of
                                        NONE => seek (E.sgiBinds env h, sgiBindsD (env, denv) h) t
                                      | SOME envs => envs
                        in
                            seek (env, denv) sgis1
                        end
                in
                    case sgi of
                        L'.SgiConAbs (x, n2, k2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     fun found (x', n1, k1, co1) =
                                         if x = x' then
                                             let
                                                 val () = unifyKinds k1 k2
                                                     handle KUnify (k1, k2, err) =>
                                                            sgnError env (SgiWrongKind (sgi1All, k1, sgi2All, k2, err))
                                                 val env = E.pushCNamedAs env x n1 k1 co1
                                             in
                                                 SOME (if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k2 (SOME (L'.CNamed n1, loc2)),
                                                       denv)
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiConAbs (x', n1, k1) => found (x', n1, k1, NONE)
                                       | L'.SgiCon (x', n1, k1, c1) => found (x', n1, k1, SOME c1)
                                       | L'.SgiDatatype (x', n1, xs, _) =>
                                         let
                                             val k = (L'.KType, loc)
                                             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
                                         in
                                             found (x', n1, k', NONE)
                                         end
                                       | L'.SgiDatatypeImp (x', n1, m1, ms, s, xs, _) =>
                                         let
                                             val k = (L'.KType, loc)
                                             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
                                         in
                                             found (x', n1, k', SOME (L'.CModProj (m1, ms, s), loc))
                                         end
                                       | L'.SgiClassAbs (x', n1) => found (x', n1,
                                                                           (L'.KArrow ((L'.KType, loc),
                                                                                       (L'.KType, loc)), loc),
                                                                           NONE)
                                       | L'.SgiClass (x', n1, c) => found (x', n1,
                                                                           (L'.KArrow ((L'.KType, loc),
                                                                                       (L'.KType, loc)), loc),
                                                                           SOME c)
                                       | _ => NONE
                                 end)

                      | L'.SgiCon (x, n2, k2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     fun found (x', n1, k1, c1) =
                                         if x = x' then
                                             let
                                                 fun good () =
                                                     let
                                                         val env = E.pushCNamedAs env x n2 k2 (SOME c2)
                                                         val env = if n1 = n2 then
                                                                       env
                                                                   else
                                                                       E.pushCNamedAs env x n1 k1 (SOME c1)
                                                     in
                                                         SOME (env, denv)
                                                     end
                                             in
                                                 (case unifyCons (env, denv) c1 c2 of
                                                      [] => good ()
                                                    | _ => NONE)
                                                 handle CUnify (c1, c2, err) =>
                                                        (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                         good ())
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiCon (x', n1, k1, c1) => found (x', n1, k1, c1)
                                       | L'.SgiClass (x', n1, c1) =>
                                         found (x', n1, (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc), c1)
                                       | _ => NONE
                                 end)

                      | L'.SgiDatatype (x, n2, xs2, xncs2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     fun found (n1, xs1, xncs1) =
                                         let
                                             fun mismatched ue =
                                                 (sgnError env (SgiMismatchedDatatypes (sgi1All, sgi2All, ue));
                                                  SOME (env, denv))

                                             val k = (L'.KType, loc)
                                             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs1

                                             fun good () =
                                                 let
                                                     val env = E.sgiBinds env sgi2All
                                                     val env = if n1 = n2 then
                                                                   env
                                                               else
                                                                   E.pushCNamedAs env x n1 k'
                                                                                  (SOME (L'.CNamed n2, loc))
                                                 in
                                                     SOME (env, denv)
                                                 end

                                             val env = E.pushCNamedAs env x n1 k' NONE
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k' (SOME (L'.CNamed n1, loc))
                                             val env = foldl (fn (x, env) => E.pushCRel env x k) env xs1
                                             fun xncBad ((x1, _, t1), (x2, _, t2)) =
                                                 String.compare (x1, x2) <> EQUAL
                                                 orelse case (t1, t2) of
                                                            (NONE, NONE) => false
                                                          | (SOME t1, SOME t2) =>
                                                            not (List.null (unifyCons (env, denv) t1 t2))
                                                          | _ => true
                                         in
                                             (if xs1 <> xs2
                                                 orelse length xncs1 <> length xncs2
                                                 orelse ListPair.exists xncBad (xncs1, xncs2) then
                                                  mismatched NONE
                                              else
                                                  good ())
                                             handle CUnify ue => mismatched (SOME ue)
                                         end
                                 in
                                     case sgi1 of
                                         L'.SgiDatatype (x', n1, xs, xncs1) =>
                                         if x' = x then
                                             found (n1, xs, xncs1)
                                         else
                                             NONE
                                       | L'.SgiDatatypeImp (x', n1, _, _, _, xs, xncs1) =>
                                         if x' = x then
                                             found (n1, xs, xncs1)
                                         else
                                             NONE
                                       | _ => NONE
                                 end)

                      | L'.SgiDatatypeImp (x, n2, m12, ms2, s2, xs, _) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiDatatypeImp (x', n1, m11, ms1, s1, _, _) =>
                                     if x = x' then
                                         let
                                             val k = (L'.KType, loc)
                                             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
                                             val t1 = (L'.CModProj (m11, ms1, s1), loc)
                                             val t2 = (L'.CModProj (m12, ms2, s2), loc)

                                             fun good () =
                                                 let
                                                     val env = E.pushCNamedAs env x n1 k' (SOME t1)
                                                     val env = E.pushCNamedAs env x n2 k' (SOME t2)
                                                 in
                                                     SOME (env, denv)
                                                 end
                                         in
                                             (case unifyCons (env, denv) t1 t2 of
                                                  [] => good ()
                                                | _ => NONE)
                                             handle CUnify (c1, c2, err) =>
                                                    (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                     good ())
                                         end
                                     else
                                         NONE

                                   | _ => NONE)

                      | L'.SgiVal (x, n2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiVal (x', n1, c1) =>
                                     if x = x' then
                                         ((*prefaces "Pre" [("c1", p_con env c1),
                                                          ("c2", p_con env c2)];*)
                                          case unifyCons (env, denv) c1 c2 of
                                              [] => SOME (env, denv)
                                            | _ => NONE)
                                         handle CUnify (c1, c2, err) =>
                                                (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                 SOME (env, denv))
                                     else
                                         NONE
                                   | L'.SgiTable (_, x', n1, c1) =>
                                     if x = x' then
                                         (case unifyCons (env, denv) (L'.CApp (tableOf (), c1), loc) c2 of
                                              [] => SOME (env, denv)
                                            | _ => NONE)
                                         handle CUnify (c1, c2, err) =>
                                                (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                 SOME (env, denv))
                                     else
                                         NONE
                                   | L'.SgiSequence (_, x', n1) =>
                                     if x = x' then
                                         (case unifyCons (env, denv) (sequenceOf ()) c2 of
                                              [] => SOME (env, denv)
                                            | _ => NONE)
                                         handle CUnify (c1, c2, err) =>
                                                (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                 SOME (env, denv))
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiStr (x, n2, sgn2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiStr (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn (env, denv) sgn1 sgn2
                                             val env = E.pushStrNamedAs env x n1 sgn1
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushStrNamedAs env x n2
                                                                            (selfifyAt env {str = (L'.StrVar n1, #2 sgn2),
                                                                                            sgn = sgn2})
                                         in
                                             SOME (env, denv)
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiSgn (x, n2, sgn2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiSgn (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn (env, denv) sgn1 sgn2
                                             val () = subSgn (env, denv) sgn2 sgn1

                                             val env = E.pushSgnNamedAs env x n2 sgn2
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushSgnNamedAs env x n1 sgn2
                                         in
                                             SOME (env, denv)
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiConstraint (c2, d2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiConstraint (c1, d1) =>
                                     if consEq (env, denv) (c1, c2) andalso consEq (env, denv) (d1, d2) then
                                         let
                                             val (denv, gs) = D.assert env denv (c2, d2)
                                         in
                                             case gs of
                                                 [] => ()
                                               | _ => raise Fail "subSgn: Sub-constraints remain";

                                             SOME (env, denv)
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiTable (_, x, n2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiTable (_, x', n1, c1) =>
                                     if x = x' then
                                         (case unifyCons (env, denv) c1 c2 of
                                              [] => SOME (env, denv)
                                            | _ => NONE)
                                         handle CUnify (c1, c2, err) =>
                                                (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                 SOME (env, denv))
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiSequence (_, x, n2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 case sgi1 of
                                     L'.SgiSequence (_, x', n1) =>
                                     if x = x' then
                                         SOME (env, denv)
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiClassAbs (x, n2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     fun found (x', n1, co) =
                                         if x = x' then
                                             let
                                                 val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
                                                 val env = E.pushCNamedAs env x n1 k co
                                             in
                                                 SOME (if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k (SOME (L'.CNamed n1, loc2)),
                                                       denv)
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiClassAbs (x', n1) => found (x', n1, NONE)
                                       | L'.SgiClass (x', n1, c) => found (x', n1, SOME c)
                                       | _ => NONE
                                 end)
                      | L'.SgiClass (x, n2, c2) =>
                        seek (fn sgi1All as (sgi1, _) =>
                                 let
                                     val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)

                                     fun found (x', n1, c1) =
                                         if x = x' then
                                             let
                                                 fun good () =
                                                     let
                                                         val env = E.pushCNamedAs env x n2 k (SOME c2)
                                                         val env = if n1 = n2 then
                                                                       env
                                                                   else
                                                                       E.pushCNamedAs env x n1 k (SOME c1)
                                                     in
                                                         SOME (env, denv)
                                                     end
                                             in
                                                 (case unifyCons (env, denv) c1 c2 of
                                                      [] => good ()
                                                    | _ => NONE)
                                                 handle CUnify (c1, c2, err) =>
                                                        (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                         good ())
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiClass (x', n1, c1) => found (x', n1, c1)
                                       | _ => NONE
                                 end)
                end
        in
            ignore (foldl folder (env, denv) sgis2)
        end

      | (L'.SgnFun (m1, n1, dom1, ran1), L'.SgnFun (m2, n2, dom2, ran2)) =>
        let
            val ran1 =
                if n1 = n2 then
                    ran1
                else
                    subStrInSgn (n1, n2) ran1
        in
            subSgn (env, denv) dom2 dom1;
            subSgn (E.pushStrNamedAs env m2 n2 dom2, denv) ran1 ran2
        end

      | _ => sgnError env (SgnWrongForm (sgn1, sgn2))


fun positive self =
    let
        open L

        fun none (c, _) =
            case c of
                CAnnot (c, _) => none c

              | TFun (c1, c2) => none c1 andalso none c2
              | TCFun (_, _, _, c) => none c
              | TRecord c => none c

              | CVar ([], x) => x <> self
              | CVar _ => true
              | CApp (c1, c2) => none c1 andalso none c2
              | CAbs _ => false
              | CDisjoint (c1, c2, c3) => none c1 andalso none c2 andalso none c3

              | CName _ => true

              | CRecord xcs => List.all (fn (c1, c2) => none c1 andalso none c2) xcs
              | CConcat (c1, c2) => none c1 andalso none c2
              | CFold => true

              | CUnit => true

              | CTuple cs => List.all none cs
              | CProj (c, _) => none c

              | CWild _ => false                

        fun pos (c, _) =
            case c of
                CAnnot (c, _) => pos c

              | TFun (c1, c2) => none c1 andalso pos c2
              | TCFun (_, _, _, c) => pos c
              | TRecord c => pos c

              | CVar _ => true
              | CApp (c1, c2) => pos c1 andalso none c2
              | CAbs _ => false
              | CDisjoint (c1, c2, c3) => none c1 andalso none c2 andalso none c3

              | CName _ => true

              | CRecord xcs => List.all (fn (c1, c2) => none c1 andalso pos c2) xcs
              | CConcat (c1, c2) => pos c1 andalso pos c2
              | CFold => true

              | CUnit => true

              | CTuple cs => List.all pos cs
              | CProj (c, _) => pos c

              | CWild _ => false
    in
        pos
    end

fun wildifyStr env (str, sgn) =
    case #1 (hnormSgn env sgn) of
        L'.SgnConst sgis =>
        (case #1 str of
             L.StrConst ds =>
             let
                 fun decompileCon env (c, loc) =
                     case c of
                         L'.CRel i =>
                         let
                             val (s, _) = E.lookupCRel env i
                         in
                             SOME (L.CVar ([], s), loc)
                         end
                       | L'.CNamed i =>
                         let
                             val (s, _, _) = E.lookupCNamed env i
                         in
                             SOME (L.CVar ([], s), loc)
                         end
                       | L'.CModProj (m1, ms, x) =>
                         let
                             val (s, _) = E.lookupStrNamed env m1
                         in
                             SOME (L.CVar (s :: ms, x), loc)
                         end
                       | L'.CName s => SOME (L.CName s, loc)
                       | L'.CRecord (_, xcs) =>
                         let
                             fun fields xcs =
                                 case xcs of
                                     [] => SOME []
                                   | (x, t) :: xcs =>
                                     case (decompileCon env x, decompileCon env t, fields xcs) of
                                         (SOME x, SOME t, SOME xcs) => SOME ((x, t) :: xcs)
                                       | _ => NONE
                         in
                             Option.map (fn xcs => (L.CRecord xcs, loc))
                             (fields xcs)
                         end
                       | L'.CConcat (c1, c2) =>
                         (case (decompileCon env c1, decompileCon env c2) of
                              (SOME c1, SOME c2) => SOME (L.CConcat (c1, c2), loc)
                            | _ => NONE)
                       | L'.CUnit => SOME (L.CUnit, loc)

                       | _ => NONE

                 val (needed, constraints, _) =
                     foldl (fn ((sgi, loc), (needed, constraints, env')) =>
                               let
                                   val (needed, constraints) =
                                       case sgi of
                                           L'.SgiConAbs (x, _, _) => (SS.add (needed, x), constraints)
                                         | L'.SgiConstraint cs => (needed, (env', cs, loc) :: constraints)
                                         | _ => (needed, constraints)
                               in
                                   (needed, constraints, E.sgiBinds env' (sgi, loc))
                               end)
                           (SS.empty, [], env) sgis
                                                              
                 val needed = foldl (fn ((d, _), needed) =>
                                        case d of
                                            L.DCon (x, _, _) => (SS.delete (needed, x)
                                                                 handle NotFound =>
                                                                        needed)
                                          | L.DClass (x, _) => (SS.delete (needed, x)
                                                                handle NotFound => needed)
                                          | L.DOpen _ => SS.empty
                                          | _ => needed)
                                    needed ds

                 val cds = List.mapPartial (fn (env', (c1, c2), loc) =>
                                               case (decompileCon env' c1, decompileCon env' c2) of
                                                   (SOME c1, SOME c2) =>
                                                   SOME (L.DConstraint (c1, c2), loc)
                                                 | _ => NONE) constraints
             in
                 case SS.listItems needed of
                     [] => (L.StrConst (ds @ cds), #2 str)
                   | xs =>
                     let
                         val kwild = (L.KWild, #2 str)
                         val cwild = (L.CWild kwild, #2 str)
                         val ds' = map (fn x => (L.DCon (x, NONE, cwild), #2 str)) xs
                     in
                         (L.StrConst (ds @ ds' @ cds), #2 str)
                     end
             end
           | _ => str)
      | _ => str

val makeInstantiable =
    let
        fun kind k = k
        fun con c =
            case c of
                L'.CDisjoint (L'.LeaveAlone, c1, c2, c) => L'.CDisjoint (L'.Instantiate, c1, c2, c)
              | _ => c
    in
        U.Con.map {kind = kind, con = con}
    end

fun elabDecl (dAll as (d, loc), (env, denv, gs : constraint list)) =
    let
        (*val () = preface ("elabDecl", SourcePrint.p_decl (d, loc))*)

        val r = 
            case d of
                L.DCon (x, ko, c) =>
                let
                    val k' = case ko of
                                 NONE => kunif loc
                               | SOME k => elabKind k

                    val (c', ck, gs') = elabCon (env, denv) c
                    val (env', n) = E.pushCNamed env x k' (SOME c')
                in
                    checkKind env c' ck k';

                    ([(L'.DCon (x, n, k', c'), loc)], (env', denv, enD gs' @ gs))
                end
              | L.DDatatype (x, xs, xcs) =>
                let
                    val positive = List.all (fn (_, to) =>
                                                case to of
                                                    NONE => true
                                                  | SOME t => positive x t) xcs

                    val k = (L'.KType, loc)
                    val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
                    val (env, n) = E.pushCNamed env x k' NONE
                    val t = (L'.CNamed n, loc)
                    val nxs = length xs - 1
                    val t = ListUtil.foldli (fn (i, _, t) => (L'.CApp (t, (L'.CRel (nxs - i), loc)), loc)) t xs

                    val (env', denv') = foldl (fn (x, (env', denv')) =>
                                                  (E.pushCRel env' x k,
                                                   D.enter denv')) (env, denv) xs

                    val (xcs, (used, env, gs')) =
                        ListUtil.foldlMap
                            (fn ((x, to), (used, env, gs)) =>
                                let
                                    val (to, t, gs') = case to of
                                                           NONE => (NONE, t, gs)
                                                         | SOME t' =>
                                                           let
                                                               val (t', tk, gs') = elabCon (env', denv') t'
                                                           in
                                                               checkKind env' t' tk k;
                                                               (SOME t', (L'.TFun (t', t), loc), enD gs' @ gs)
                                                           end
                                    val t = foldr (fn (x, t) => (L'.TCFun (L'.Implicit, x, k, t), loc)) t xs

                                    val (env, n') = E.pushENamed env x t
                                in
                                    if SS.member (used, x) then
                                        strError env (DuplicateConstructor (x, loc))
                                    else
                                        ();
                                    ((x, n', to), (SS.add (used, x), env, gs'))
                                end)
                            (SS.empty, env, []) xcs

                    val env = E.pushDatatype env n xs xcs
                    val d' = (L'.DDatatype (x, n, xs, xcs), loc)
                in
                    if positive then
                        ()
                    else
                        declError env (Nonpositive d');

                    ([d'], (env, denv, gs' @ gs))
                end

              | L.DDatatypeImp (_, [], _) => raise Fail "Empty DDatatypeImp"

              | L.DDatatypeImp (x, m1 :: ms, s) =>
                (case E.lookupStr env m1 of
                     NONE => (expError env (UnboundStrInExp (loc, m1));
                              ([], (env, denv, gs)))
                   | SOME (n, sgn) =>
                     let
                         val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                                    case E.projectStr env {sgn = sgn, str = str, field = m} of
                                                        NONE => (conError env (UnboundStrInCon (loc, m));
                                                                 (strerror, sgnerror))
                                                      | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                                ((L'.StrVar n, loc), sgn) ms
                     in
                         case hnormCon (env, denv) (L'.CModProj (n, ms, s), loc) of
                             ((L'.CModProj (n, ms, s), _), gs') =>
                             (case E.projectDatatype env {sgn = sgn, str = str, field = s} of
                                  NONE => (conError env (UnboundDatatype (loc, s));
                                           ([], (env, denv, gs)))
                                | SOME (xs, xncs) =>
                                  let
                                      val k = (L'.KType, loc)
                                      val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs
                                      val t = (L'.CModProj (n, ms, s), loc)
                                      val (env, n') = E.pushCNamed env x k' (SOME t)
                                      val env = E.pushDatatype env n' xs xncs

                                      val t = (L'.CNamed n', loc)
                                      val env = foldl (fn ((x, n, to), env) =>
                                                          let
                                                              val t = case to of
                                                                          NONE => t
                                                                        | SOME t' => (L'.TFun (t', t), loc)

                                                              val t = foldr (fn (x, t) =>
                                                                                (L'.TCFun (L'.Implicit, x, k, t), loc))
                                                                            t xs
                                                          in
                                                              E.pushENamedAs env x n t
                                                          end) env xncs
                                  in
                                      ([(L'.DDatatypeImp (x, n', n, ms, s, xs, xncs), loc)], (env, denv, enD gs' @ gs))
                                  end)
                           | _ => (strError env (NotDatatype loc);
                                   ([], (env, denv, [])))
                     end)

              | L.DVal (x, co, e) =>
                let
                    val (c', _, gs1) = case co of
                                           NONE => (cunif (loc, ktype), ktype, [])
                                         | SOME c => elabCon (env, denv) c

                    val (e', et, gs2) = elabExp (env, denv) e
                    val gs3 = checkCon (env, denv) e' et c'
                    val (c', gs4) = normClassConstraint (env, denv) c'
                    val (env', n) = E.pushENamed env x c'
                    val c' = makeInstantiable c'
                in
                    (*prefaces "DVal" [("x", Print.PD.string x),
                                     ("c'", p_con env c')];*)
                    ([(L'.DVal (x, n, c', e'), loc)], (env', denv, enD gs1 @ gs2 @ enD gs3 @ enD gs4 @ gs))
                end
              | L.DValRec vis =>
                let
                    fun allowable (e, _) =
                        case e of
                            L.EAbs _ => true
                          | L.ECAbs (_, _, _, e) => allowable e
                          | L.EDisjoint (_, _, e) => allowable e
                          | _ => false

                    val (vis, gs) = ListUtil.foldlMap
                                        (fn ((x, co, e), gs) =>
                                            let
                                                val (c', _, gs1) = case co of
                                                                       NONE => (cunif (loc, ktype), ktype, [])
                                                                     | SOME c => elabCon (env, denv) c
                                            in
                                                ((x, c', e), enD gs1 @ gs)
                                            end) gs vis

                    val (vis, env) = ListUtil.foldlMap (fn ((x, c', e), env) =>
                                                           let
                                                               val (env, n) = E.pushENamed env x c'
                                                           in
                                                               ((x, n, c', e), env)
                                                           end) env vis

                    val (vis, gs) = ListUtil.foldlMap (fn ((x, n, c', e), gs) =>
                                                          let
                                                              val (e', et, gs1) = elabExp (env, denv) e
                                                                                  
                                                              val gs2 = checkCon (env, denv) e' et c'

                                                              val c' = makeInstantiable c'
                                                          in
                                                              if allowable e then
                                                                  ()
                                                              else
                                                                  expError env (IllegalRec (x, e'));
                                                              ((x, n, c', e'), gs1 @ enD gs2 @ gs)
                                                          end) gs vis
                in
                    ([(L'.DValRec vis, loc)], (env, denv, gs))
                end

              | L.DSgn (x, sgn) =>
                let
                    val (sgn', gs') = elabSgn (env, denv) sgn
                    val (env', n) = E.pushSgnNamed env x sgn'
                in
                    ([(L'.DSgn (x, n, sgn'), loc)], (env', denv, enD gs' @ gs))
                end

              | L.DStr (x, sgno, str) =>
                let
                    val () = if x = "Basis" then
                                 raise Fail "Not allowed to redefine structure 'Basis'"
                             else
                                 ()

                    val formal = Option.map (elabSgn (env, denv)) sgno

                    val (str', sgn', gs') =
                        case formal of
                            NONE =>
                            let
                                val (str', actual, gs') = elabStr (env, denv) str
                            in
                                (str', selfifyAt env {str = str', sgn = actual}, gs')
                            end
                          | SOME (formal, gs1) =>
                            let
                                val str = wildifyStr env (str, formal)
                                val (str', actual, gs2) = elabStr (env, denv) str
                            in
                                subSgn (env, denv) (selfifyAt env {str = str', sgn = actual}) formal;
                                (str', formal, enD gs1 @ gs2)
                            end

                    val (env', n) = E.pushStrNamed env x sgn'
                in
                    case #1 (hnormSgn env sgn') of
                        L'.SgnFun _ =>
                        (case #1 str' of
                             L'.StrFun _ => ()
                           | _ => strError env (FunctorRebind loc))
                      | _ => ();

                    ([(L'.DStr (x, n, sgn', str'), loc)], (env', denv, gs' @ gs))
                end

              | L.DFfiStr (x, sgn) =>
                let
                    val (sgn', gs') = elabSgn (env, denv) sgn

                    val (env', n) = E.pushStrNamed env x sgn'
                in
                    ([(L'.DFfiStr (x, n, sgn'), loc)], (env', denv, enD gs' @ gs))
                end

              | L.DOpen (m, ms) =>
                (case E.lookupStr env m of
                     NONE => (strError env (UnboundStr (loc, m));
                              ([], (env, denv, gs)))
                   | SOME (n, sgn) =>
                     let
                         val (_, sgn) = foldl (fn (m, (str, sgn)) =>
                                                  case E.projectStr env {str = str, sgn = sgn, field = m} of
                                                      NONE => (strError env (UnboundStr (loc, m));
                                                               (strerror, sgnerror))
                                                    | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                                              ((L'.StrVar n, loc), sgn) ms

                         val (ds, (env', denv')) = dopen (env, denv) {str = n, strs = ms, sgn = sgn}
                         val denv' = dopenConstraints (loc, env', denv') {str = m, strs = ms}
                     in
                         (ds, (env', denv', gs))
                     end)

              | L.DConstraint (c1, c2) =>
                let
                    val (c1', k1, gs1) = elabCon (env, denv) c1
                    val (c2', k2, gs2) = elabCon (env, denv) c2
                    val gs3 = D.prove env denv (c1', c2', loc)

                    val (denv', gs4) = D.assert env denv (c1', c2')
                in
                    checkKind env c1' k1 (L'.KRecord (kunif loc), loc);
                    checkKind env c2' k2 (L'.KRecord (kunif loc), loc);

                    ([(L'.DConstraint (c1', c2'), loc)], (env, denv', enD gs1 @ enD gs2 @ enD gs3 @ enD gs4 @ gs))
                end

              | L.DOpenConstraints (m, ms) =>
                let
                    val denv = dopenConstraints (loc, env, denv) {str = m, strs = ms}
                in
                    ([], (env, denv, gs))
                end

              | L.DExport str =>
                let
                    val (str', sgn, gs') = elabStr (env, denv) str

                    val sgn =
                        case #1 (hnormSgn env sgn) of
                            L'.SgnConst sgis =>
                            let
                                fun doOne (all as (sgi, _), env) =
                                    (case sgi of
                                         L'.SgiVal (x, n, t) =>
                                         let
                                             fun doPage (makeRes, ran) =
                                                 case hnormCon (env, denv) ran of
                                                     ((L'.CApp (tf, arg), _), []) =>
                                                     (case (hnormCon (env, denv) tf, hnormCon (env, denv) arg) of
                                                          (((L'.CModProj (basis, [], "transaction"), _), []),
                                                           ((L'.CApp (tf, arg3), _), [])) =>
                                                          (case (basis = !basis_r,
                                                                 hnormCon (env, denv) tf, hnormCon (env, denv) arg3) of
                                                               (true,
                                                                ((L'.CApp (tf, arg2), _), []),
                                                                (((L'.CRecord (_, []), _), []))) =>
                                                               (case (hnormCon (env, denv) tf) of
                                                                    ((L'.CApp (tf, arg1), _), []) =>
                                                                    (case (hnormCon (env, denv) tf,
                                                                           hnormCon (env, denv) arg1,
                                                                           hnormCon (env, denv) arg2) of
                                                                         ((tf, []), (arg1, []),
                                                                          ((L'.CRecord (_, []), _), [])) =>
                                                                         let
                                                                             val t = (L'.CApp (tf, arg1), loc)
                                                                             val t = (L'.CApp (t, arg2), loc)
                                                                             val t = (L'.CApp (t, arg3), loc)
                                                                             val t = (L'.CApp (
                                                                                      (L'.CModProj
                                                                                           (basis, [], "transaction"), loc),
                                                                                      t), loc)
                                                                         in
                                                                             (L'.SgiVal (x, n, makeRes t), loc)
                                                                         end
                                                                       | _ => all)
                                                                  | _ => all)
                                                             | _ => all)
                                                        | _ => all)
                                                   | _ => all
                                         in
                                             case hnormCon (env, denv) t of
                                                 ((L'.TFun (dom, ran), _), []) =>
                                                 (case hnormCon (env, denv) dom of
                                                      ((L'.TRecord domR, _), []) =>
                                                      doPage (fn t => (L'.TFun ((L'.TRecord domR,
                                                                                 loc),
                                                                                t), loc), ran)
                                                    | _ => all)
                                               | _ => doPage (fn t => t, t)
                                         end
                                       | _ => all,
                                     E.sgiBinds env all)
                            in
                                (L'.SgnConst (#1 (ListUtil.foldlMap doOne env sgis)), loc)
                            end
                          | _ => sgn
                in
                    ([(L'.DExport (E.newNamed (), sgn, str'), loc)], (env, denv, gs' @ gs))
                end

              | L.DTable (x, c) =>
                let
                    val (c', k, gs') = elabCon (env, denv) c
                    val (env, n) = E.pushENamed env x (L'.CApp (tableOf (), c'), loc)
                in
                    checkKind env c' k (L'.KRecord (L'.KType, loc), loc);
                    ([(L'.DTable (!basis_r, x, n, c'), loc)], (env, denv, enD gs' @ gs))
                end
              | L.DSequence x =>
                let
                    val (env, n) = E.pushENamed env x (sequenceOf ())
                in
                    ([(L'.DSequence (!basis_r, x, n), loc)], (env, denv, gs))
                end

              | L.DClass (x, c) =>
                let
                    val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
                    val (c', ck, gs') = elabCon (env, denv) c
                    val (env, n) = E.pushCNamed env x k (SOME c')
                    val env = E.pushClass env n
                in
                    checkKind env c' ck k;
                    ([(L'.DClass (x, n, c'), loc)], (env, denv, enD gs' @ gs))
                end

              | L.DDatabase s => ([(L'.DDatabase s, loc)], (env, denv, gs))

        (*val tcs = List.filter (fn TypeClass _ => true | _ => false) (#3 (#2 r))*)
    in
        (*prefaces "elabDecl" [("e", SourcePrint.p_decl dAll),
                            ("|tcs|", PD.string (Int.toString (length tcs)))];*)

        r
    end

and elabStr (env, denv) (str, loc) =
    case str of
        L.StrConst ds =>
        let
            val (ds', (_, _, gs)) = ListUtil.foldlMapConcat elabDecl (env, denv, []) ds
            val sgis = ListUtil.mapConcat sgiOfDecl ds'

            val (sgis, _, _, _, _) =
                foldr (fn ((sgi, loc), (sgis, cons, vals, sgns, strs)) =>
                          case sgi of
                              L'.SgiConAbs (x, n, k) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiConAbs (x, n, k), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiCon (x, n, k, c) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiCon (x, n, k, c), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiDatatype (x, n, xs, xncs) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)

                                  val (xncs, vals) =
                                      ListUtil.foldlMap
                                          (fn ((x, n, t), vals) =>
                                              if SS.member (vals, x) then
                                                  (("?" ^ x, n, t), vals)
                                              else
                                                  ((x, n, t), SS.add (vals, x)))
                                      vals xncs
                              in
                                  ((L'.SgiDatatype (x, n, xs, xncs), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiDatatypeImp (x, n, m1, ms, x', xs, xncs) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiDatatypeImp (x, n, m1, ms, x', xs, xncs), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiVal (x, n, c) =>
                              let
                                  val (vals, x) =
                                      if SS.member (vals, x) then
                                          (vals, "?" ^ x)
                                      else
                                          (SS.add (vals, x), x)
                              in
                                  ((L'.SgiVal (x, n, c), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiSgn (x, n, sgn) =>
                              let
                                  val (sgns, x) =
                                      if SS.member (sgns, x) then
                                          (sgns, "?" ^ x)
                                      else
                                          (SS.add (sgns, x), x)
                              in
                                  ((L'.SgiSgn (x, n, sgn), loc) :: sgis, cons, vals, sgns, strs)
                              end
 
                            | L'.SgiStr (x, n, sgn) =>
                              let
                                  val (strs, x) =
                                      if SS.member (strs, x) then
                                          (strs, "?" ^ x)
                                      else
                                          (SS.add (strs, x), x)
                              in
                                  ((L'.SgiStr (x, n, sgn), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiConstraint _ => ((sgi, loc) :: sgis, cons, vals, sgns, strs)
                            | L'.SgiTable (tn, x, n, c) =>
                              let
                                  val (vals, x) =
                                      if SS.member (vals, x) then
                                          (vals, "?" ^ x)
                                      else
                                          (SS.add (vals, x), x)
                              in
                                  ((L'.SgiTable (tn, x, n, c), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiSequence (tn, x, n) =>
                              let
                                  val (vals, x) =
                                      if SS.member (vals, x) then
                                          (vals, "?" ^ x)
                                      else
                                          (SS.add (vals, x), x)
                              in
                                  ((L'.SgiSequence (tn, x, n), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiClassAbs (x, n) =>
                              let
                                  val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)

                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiClassAbs (x, n), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiClass (x, n, c) =>
                              let
                                  val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)

                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiClass (x, n, c), loc) :: sgis, cons, vals, sgns, strs)
                              end)

                ([], SS.empty, SS.empty, SS.empty, SS.empty) sgis
        in
            ((L'.StrConst ds', loc), (L'.SgnConst sgis, loc), gs)
        end
      | L.StrVar x =>
        (case E.lookupStr env x of
             NONE =>
             (strError env (UnboundStr (loc, x));
              (strerror, sgnerror, []))
           | SOME (n, sgn) => ((L'.StrVar n, loc), sgn, []))
      | L.StrProj (str, x) =>
        let
            val (str', sgn, gs) = elabStr (env, denv) str
        in
            case E.projectStr env {str = str', sgn = sgn, field = x} of
                NONE => (strError env (UnboundStr (loc, x));
                         (strerror, sgnerror, []))
              | SOME sgn => ((L'.StrProj (str', x), loc), sgn, gs)
        end
      | L.StrFun (m, dom, ranO, str) =>
        let
            val (dom', gs1) = elabSgn (env, denv) dom
            val (env', n) = E.pushStrNamed env m dom'
            val (str', actual, gs2) = elabStr (env', denv) str

            val (formal, gs3) =
                case ranO of
                    NONE => (actual, [])
                  | SOME ran =>
                    let
                        val (ran', gs) = elabSgn (env', denv) ran
                    in
                        subSgn (env', denv) actual ran';
                        (ran', gs)
                    end
        in
            ((L'.StrFun (m, n, dom', formal, str'), loc),
             (L'.SgnFun (m, n, dom', formal), loc),
             enD gs1 @ gs2 @ enD gs3)
        end
      | L.StrApp (str1, str2) =>
        let
            val (str1', sgn1, gs1) = elabStr (env, denv) str1
            val str2 =
                case sgn1 of
                    (L'.SgnFun (_, _, dom, _), _) =>
                    wildifyStr env (str2, dom)
                  | _ => str2
            val (str2', sgn2, gs2) = elabStr (env, denv) str2
        in
            case #1 (hnormSgn env sgn1) of
                L'.SgnError => (strerror, sgnerror, [])
              | L'.SgnFun (m, n, dom, ran) =>
                (subSgn (env, denv) sgn2 dom;
                 case #1 (hnormSgn env ran) of
                     L'.SgnError => (strerror, sgnerror, [])
                   | L'.SgnConst sgis =>
                     ((L'.StrApp (str1', str2'), loc),
                      (L'.SgnConst ((L'.SgiStr (m, n, selfifyAt env {str = str2', sgn = sgn2}), loc) :: sgis), loc),
                      gs1 @ gs2)
                   | _ => raise Fail "Unable to hnormSgn in functor application")
              | _ => (strError env (NotFunctor sgn1);
                      (strerror, sgnerror, []))
        end

fun elabFile basis topStr topSgn env file =
    let
        val (sgn, gs) = elabSgn (env, D.empty) (L.SgnConst basis, ErrorMsg.dummySpan)
        val () = case gs of
                     [] => ()
                   | _ => (app (fn (_, env, _, c1, c2) =>
                                   prefaces "Unresolved"
                                   [("c1", p_con env c1),
                                    ("c2", p_con env c2)]) gs;
                           raise Fail "Unresolved disjointness constraints in Basis")

        val (env', basis_n) = E.pushStrNamed env "Basis" sgn
        val () = basis_r := basis_n

        val (ds, (env', _)) = dopen (env', D.empty) {str = basis_n, strs = [], sgn = sgn}

        fun discoverC r x =
            case E.lookupC env' x of
                E.NotBound => raise Fail ("Constructor " ^ x ^ " unbound in Basis")
              | E.Rel _ => raise Fail ("Constructor " ^ x ^ " bound relatively in Basis")
              | E.Named (n, (_, loc)) => r := (L'.CNamed n, loc)

        val () = discoverC int "int"
        val () = discoverC float "float"
        val () = discoverC string "string"
        val () = discoverC table "sql_table"

        val (topSgn, gs) = elabSgn (env', D.empty) (L.SgnConst topSgn, ErrorMsg.dummySpan)
        val () = case gs of
                     [] => ()
                   | _ => raise Fail "Unresolved disjointness constraints in top.urs"
        val (topStr, topSgn', gs) = elabStr (env', D.empty) (L.StrConst topStr, ErrorMsg.dummySpan)
        val () = case gs of
                     [] => ()
                   | _ => app (fn Disjoint (loc, env, denv, c1, c2) =>
                                  (case D.prove env denv (c1, c2, loc) of
                                       [] => ()
                                     | _ =>
                                       (prefaces "Unresolved constraint in top.ur"
                                                 [("loc", PD.string (ErrorMsg.spanToString loc)),
                                                  ("c1", p_con env c1),
                                                  ("c2", p_con env c2)];
                                        raise Fail "Unresolve constraint in top.ur"))
                                | TypeClass _ => raise Fail "Unresolved type class constraint in top.ur") gs
        val () = subSgn (env', D.empty) topSgn' topSgn

        val (env', top_n) = E.pushStrNamed env' "Top" topSgn

        val (ds', (env', _)) = dopen (env', D.empty) {str = top_n, strs = [], sgn = topSgn}

        fun elabDecl' (d, (env, gs)) =
            let
                val () = resetKunif ()
                val () = resetCunif ()
                val (ds, (env, _, gs)) = elabDecl (d, (env, D.empty, gs))
            in
                if ErrorMsg.anyErrors () then
                    ()
                else (
                    if List.exists kunifsInDecl ds then
                        declError env (KunifsRemain ds)
                    else
                        ();
                    
                    case ListUtil.search cunifsInDecl ds of
                        NONE => ()
                      | SOME loc =>
                        declError env (CunifsRemain ds)
                    );

                (ds, (env, gs))
            end

        val (file, (_, gs)) = ListUtil.foldlMapConcat elabDecl' (env', []) file
    in
        if ErrorMsg.anyErrors () then
            ()
        else
            app (fn Disjoint (loc, env, denv, c1, c2) =>
                    (case D.prove env denv (c1, c2, loc) of
                         [] => ()
                       | _ =>
                         (ErrorMsg.errorAt loc "Couldn't prove field name disjointness";
                          eprefaces' [("Con 1", p_con env c1),
                                      ("Con 2", p_con env c2),
                                      ("Hnormed 1", p_con env (ElabOps.hnormCon env c1)),
                                      ("Hnormed 2", p_con env (ElabOps.hnormCon env c2))]))
                  | TypeClass (env, c, r, loc) =>
                    let
                        val c = ElabOps.hnormCon env c
                    in
                        case E.resolveClass env c of
                            SOME e => r := SOME e
                          | NONE => expError env (Unresolvable (loc, c))
                    end) gs;

        (L'.DFfiStr ("Basis", basis_n, sgn), ErrorMsg.dummySpan)
        :: ds
        @ (L'.DStr ("Top", top_n, topSgn, topStr), ErrorMsg.dummySpan)
        :: ds' @ file
    end

end
