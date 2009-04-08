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
 val top_r = ref 0

 fun elabExplicitness e =
     case e of
         L.Explicit => L'.Explicit
       | L.Implicit => L'.Implicit

 fun occursKind r =
     U.Kind.exists (fn L'.KUnif (_, _, r') => r = r'
                     | _ => false)

 exception KUnify' of kunify_error

 fun unifyKinds' env (k1All as (k1, _)) (k2All as (k2, _)) =
     let
         fun err f = raise KUnify' (f (k1All, k2All))
     in
         case (k1, k2) of
             (L'.KType, L'.KType) => ()
           | (L'.KUnit, L'.KUnit) => ()

           | (L'.KArrow (d1, r1), L'.KArrow (d2, r2)) =>
             (unifyKinds' env d1 d2;
              unifyKinds' env r1 r2)
           | (L'.KName, L'.KName) => ()
           | (L'.KRecord k1, L'.KRecord k2) => unifyKinds' env k1 k2
           | (L'.KTuple ks1, L'.KTuple ks2) =>
             ((ListPair.appEq (fn (k1, k2) => unifyKinds' env k1 k2) (ks1, ks2))
              handle ListPair.UnequalLengths => err KIncompatible)

           | (L'.KRel n1, L'.KRel n2) =>
             if n1 = n2 then
                 ()
             else
                 err KIncompatible
           | (L'.KFun (x, k1), L'.KFun (_, k2)) =>
             unifyKinds' (E.pushKRel env x) k1 k2

           | (L'.KError, _) => ()
           | (_, L'.KError) => ()

           | (L'.KUnif (_, _, ref (SOME k1All)), _) => unifyKinds' env k1All k2All
           | (_, L'.KUnif (_, _, ref (SOME k2All))) => unifyKinds' env k1All k2All

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

 fun unifyKinds env k1 k2 =
     unifyKinds' env k1 k2
     handle KUnify' err => raise KUnify (k1, k2, err)

 fun checkKind env c k1 k2 =
     unifyKinds env k1 k2
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

 fun elabKind env (k, loc) =
     case k of
         L.KType => (L'.KType, loc)
       | L.KArrow (k1, k2) => (L'.KArrow (elabKind env k1, elabKind env k2), loc)
       | L.KName => (L'.KName, loc)
       | L.KRecord k => (L'.KRecord (elabKind env k), loc)
       | L.KUnit => (L'.KUnit, loc)
       | L.KTuple ks => (L'.KTuple (map (elabKind env) ks), loc)
       | L.KWild => kunif loc

       | L.KVar s => (case E.lookupK env s of
                          NONE =>
                          (kindError env (UnboundKind (loc, s));
                           kerror)
                        | SOME n => (L'.KRel n, loc))
       | L.KFun (x, k) => (L'.KFun (x, elabKind (E.pushKRel env x) k), loc)

 fun mapKind (dom, ran, loc)=
     (L'.KArrow ((L'.KArrow (dom, ran), loc),
                 (L'.KArrow ((L'.KRecord dom, loc),
                             (L'.KRecord ran, loc)), loc)), loc)

 fun hnormKind (kAll as (k, _)) =
     case k of
         L'.KUnif (_, _, ref (SOME k)) => hnormKind k
       | _ => kAll

 open ElabOps

 fun elabConHead (c as (_, loc)) k =
     let
         fun unravel (k, c) =
             case hnormKind k of
                 (L'.KFun (x, k'), _) =>
                 let
                     val u = kunif loc
                             
                     val k'' = subKindInKind (0, u) k'
                 in
                     unravel (k'', (L'.CKApp (c, u), loc))
                 end
               | _ => (c, k)
    in
         unravel (k, c)
    end

 fun elabCon (env, denv) (c, loc) =
     case c of
         L.CAnnot (c, k) =>
         let
             val k' = elabKind env k
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
             val k' = elabKind env k
             val env' = E.pushCRel env x k'
             val (t', tk, gs) = elabCon (env', D.enter denv) t
         in
             checkKind env t' tk ktype;
             ((L'.TCFun (e', x, k', t'), loc), ktype, gs)
         end
       | L.TKFun (x, t) =>
         let
             val env' = E.pushKRel env x
             val (t', tk, gs) = elabCon (env', denv) t
         in
             checkKind env t' tk ktype;
             ((L'.TKFun (x, t'), loc), ktype, gs)
         end
       | L.TDisjoint (c1, c2, c) =>
         let
             val (c1', k1, gs1) = elabCon (env, denv) c1
             val (c2', k2, gs2) = elabCon (env, denv) c2

             val ku1 = kunif loc
             val ku2 = kunif loc

             val denv' = D.assert env denv (c1', c2')
             val (c', k, gs4) = elabCon (env, denv') c
         in
             checkKind env c1' k1 (L'.KRecord ku1, loc);
             checkKind env c2' k2 (L'.KRecord ku2, loc);

             ((L'.TDisjoint (c1', c2', c'), loc), k, gs1 @ gs2 @ gs4)
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
              let
                  val (c, k) = elabConHead (L'.CRel n, loc) k
              in
                  (c, k, [])
              end
            | E.Named (n, k) =>
              let
                  val (c, k) = elabConHead (L'.CNamed n, loc) k
              in
                  (c, k, [])
              end)
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
                        | SOME k => elabKind env k
             val env' = E.pushCRel env x k'
             val (t', tk, gs) = elabCon (env', D.enter denv) t
         in
             ((L'.CAbs (x, k', t'), loc),
              (L'.KArrow (k', tk), loc),
              gs)
         end
       | L.CKAbs (x, t) =>
         let
             val env' = E.pushKRel env x
             val (t', tk, gs) = elabCon (env', denv) t
         in
             ((L'.CKAbs (x, t'), loc),
              (L'.KFun (x, tk), loc),
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
       | L.CMap =>
         let
             val dom = kunif loc
             val ran = kunif loc
         in
             ((L'.CMap (dom, ran), loc),
              mapKind (dom, ran, loc),
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
             val k' = elabKind env k
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
       | L'.TDisjoint _ => ktype

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


       | L'.CName _ => kname

       | L'.CRecord (k, _) => (L'.KRecord k, loc)
       | L'.CConcat (c, _) => kindof env c
       | L'.CMap (dom, ran) => mapKind (dom, ran, loc)

       | L'.CUnit => (L'.KUnit, loc)

       | L'.CTuple cs => (L'.KTuple (map (kindof env) cs), loc)
       | L'.CProj (c, n) =>
         (case hnormKind (kindof env c) of
              (L'.KTuple ks, _) => List.nth (ks, n - 1)
            | k => raise CUnify' (CKindof (k, c, "tuple")))

       | L'.CError => kerror
       | L'.CUnif (_, k, _, _) => k

       | L'.CKAbs (x, c) => (L'.KFun (x, kindof (E.pushKRel env x) c), loc)
       | L'.CKApp (c, k) =>
         (case hnormKind (kindof env c) of
              (L'.KFun (_, k'), _) => subKindInKind (0, k) k'
            | k => raise CUnify' (CKindof (k, c, "kapp")))
       | L'.TKFun _ => ktype

 exception GuessFailure

 fun isUnitCon env (c, loc) =
     case c of
         L'.TFun _ => false
       | L'.TCFun _ => false
       | L'.TRecord _ => false
       | L'.TDisjoint _ => false

       | L'.CRel xn => #1 (#2 (E.lookupCRel env xn)) = L'.KUnit
       | L'.CNamed xn => #1 (#2 (E.lookupCNamed env xn)) = L'.KUnit
       | L'.CModProj (n, ms, x) => false
         (*let
             val (_, sgn) = E.lookupStrNamed env n
             val (str, sgn) = foldl (fn (m, (str, sgn)) =>
                                        case E.projectStr env {sgn = sgn, str = str, field = m} of
                                            NONE => raise Fail "kindof: Unknown substructure"
                                          | SOME sgn => ((L'.StrProj (str, m), loc), sgn))
                              ((L'.StrVar n, loc), sgn) ms
         in
             case E.projectCon env {sgn = sgn, str = str, field = x} of
                 NONE => raise Fail "kindof: Unknown con in structure"
               | SOME ((k, _), _) => k = L'.KUnit
         end*)

       | L'.CApp (c, _) => false
         (*(case hnormKind (kindof env c) of
              (L'.KArrow (_, k), _) => #1 k = L'.KUnit
            | (L'.KError, _) => false
            | k => raise CUnify' (CKindof (k, c, "arrow")))*)
       | L'.CAbs _ => false

       | L'.CName _ => false

       | L'.CRecord _ => false
       | L'.CConcat _ => false
       | L'.CMap _ => false

       | L'.CUnit => true

       | L'.CTuple _ => false
       | L'.CProj (c, n) => false
         (*(case hnormKind (kindof env c) of
              (L'.KTuple ks, _) => #1 (List.nth (ks, n - 1)) = L'.KUnit
            | k => raise CUnify' (CKindof (k, c, "tuple")))*)

       | L'.CError => false
       | L'.CUnif (_, k, _, _) => #1 k = L'.KUnit

       | L'.CKAbs _ => false
       | L'.CKApp _ => false
       | L'.TKFun _ => false

 val recdCounter = ref 0

 fun unifyRecordCons env (c1, c2) =
     let
         fun rkindof c =
             case hnormKind (kindof env c) of
                 (L'.KRecord k, _) => k
               | (L'.KError, _) => kerror
               | k => raise CUnify' (CKindof (k, c, "record"))

         val k1 = rkindof c1
         val k2 = rkindof c2

         val r1 = recordSummary env c1
         val r2 = recordSummary env c2
     in
         unifyKinds env k1 k2;
         unifySummaries env (k1, r1, r2)
     end

 and recordSummary env c =
     let
         val c = hnormCon env c

         val sum =
             case c of
                 (L'.CRecord (_, xcs), _) => {fields = xcs, unifs = [], others = []}
               | (L'.CConcat (c1, c2), _) =>
                 let
                     val s1 = recordSummary env c1
                     val s2 = recordSummary env c2
                 in
                     {fields = #fields s1 @ #fields s2,
                      unifs = #unifs s1 @ #unifs s2,
                      others = #others s1 @ #others s2}
                 end
               | (L'.CUnif (_, _, _, ref (SOME c)), _) => recordSummary env c
               | c' as (L'.CUnif (_, _, _, r), _) => {fields = [], unifs = [(c', r)], others = []}
               | c' => {fields = [], unifs = [], others = [c']}
     in
         sum
     end

 and consEq env (c1, c2) =
     (unifyCons env c1 c2;
      true)
     handle CUnify _ => false

 and consNeq env (c1, c2) =
     case (#1 (hnormCon env c1), #1 (hnormCon env c2)) of
         (L'.CName x1, L'.CName x2) => x1 <> x2
       | _ => false

 and unifySummaries env (k, s1 : record_summary, s2 : record_summary) =
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
                                          andalso consEq env (c1, c2)
                                          andalso consEq env (x1, x2))
                                      (#fields s1, #fields s2)
         (*val () = eprefaces "Summaries2" [("#1", p_summary env {fields = fs1, unifs = #unifs s1, others = #others s1}),
                                          ("#2", p_summary env {fields = fs2, unifs = #unifs s2, others = #others s2})]*)

         val (unifs1, unifs2) = eatMatching (fn ((_, r1), (_, r2)) => r1 = r2) (#unifs s1, #unifs s2)

         val (others1, others2) = eatMatching (consEq env) (#others s1, #others s2)
         (*val () = eprefaces "Summaries3" [("#1", p_summary env {fields = fs1, unifs = unifs1, others = others1}),
                                          ("#2", p_summary env {fields = fs2, unifs = unifs2, others = others2})]*)

         fun unsummarize {fields, unifs, others} =
             let
                 val c = (L'.CRecord (k, fields), loc)

                 val c = foldl (fn ((c1, _), c2) => (L'.CConcat (c1, c2), loc))
                               c unifs
             in
                 foldl (fn (c1, c2) => (L'.CConcat (c1, c2), loc))
                       c others
             end

         val empties = ([], [], [], [], [], [])

         val (unifs1, fs1, others1, unifs2, fs2, others2) =
             case (unifs1, fs1, others1, unifs2, fs2, others2) of
                 orig as ([(_, r)], [], [], _, _, _) =>
                 let
                     val c = unsummarize {fields = fs2, others = others2, unifs = unifs2}
                 in
                     if occursCon r c then
                         orig
                     else
                         (r := SOME c;
                          empties)
                 end
               | orig as (_, _, _, [(_, r)], [], []) =>
                 let
                     val c = unsummarize {fields = fs1, others = others1, unifs = unifs1}
                 in
                     if occursCon r c then
                         orig
                     else
                         (r := SOME c;
                          empties)
                 end
               | orig as ([(_, r1 as ref NONE)], _, [], [(_, r2 as ref NONE)], _, []) =>
                 if List.all (fn (x1, _) => List.all (fn (x2, _) => consNeq env (x1, x2)) fs2) fs1 then
                     let
                         val kr = (L'.KRecord k, dummy)
                         val u = cunif (loc, kr)
                     in
                         r1 := SOME (L'.CConcat ((L'.CRecord (k, fs2), loc), u), loc);
                         r2 := SOME (L'.CConcat ((L'.CRecord (k, fs1), loc), u), loc);
                         empties
                     end
                 else
                     orig
               | orig => orig

         (*val () = eprefaces "Summaries4" [("#1", p_summary env {fields = fs1, unifs = unifs1, others = others1}),
                                            ("#2", p_summary env {fields = fs2, unifs = unifs2, others = others2})]*)

         fun isGuessable (other, fs) =
             (guessMap env (other, (L'.CRecord (k, fs), loc), GuessFailure);
              true)
             handle GuessFailure => false

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
     in
         case (unifs1, fs1, others1, unifs2, fs2, others2) of
             (_, [], [], [], [], []) =>
             app (fn (_, r) => r := SOME empty) unifs1
           | ([], [], [], _, [], []) =>
             app (fn (_, r) => r := SOME empty) unifs2
           | _ => if clear then
                      ()
                  else
                      raise CUnify' (CRecordFailure (unsummarize s1, unsummarize s2))
         (*before eprefaces "Summaries'" [("#1", p_summary env s1),
                                       ("#2", p_summary env s2)]*)
     end

 and guessMap env (c1, c2, ex) =
     let
         val loc = #2 c1

         fun unfold (dom, ran, f, r, c) =
             let
                 fun unfold (r, c) =
                     case #1 c of
                         L'.CRecord (_, []) => unifyCons env r (L'.CRecord (dom, []), loc)
                       | L'.CRecord (_, [(x, v)]) =>
                         let
                             val v' = case dom of
                                          (L'.KUnit, _) => (L'.CUnit, loc)
                                        | _ => cunif (loc, dom)
                         in
                             unifyCons env v (L'.CApp (f, v'), loc);
                             unifyCons env r (L'.CRecord (dom, [(x, v')]), loc)
                         end
                       | L'.CRecord (_, (x, v) :: rest) =>
                         let
                             val r1 = cunif (loc, (L'.KRecord dom, loc))
                             val r2 = cunif (loc, (L'.KRecord dom, loc))
                         in
                             unfold (r1, (L'.CRecord (ran, [(x, v)]), loc));
                             unfold (r2, (L'.CRecord (ran, rest), loc));
                             unifyCons env r (L'.CConcat (r1, r2), loc)
                         end
                       | L'.CConcat (c1', c2') =>
                         let
                             val r1 = cunif (loc, (L'.KRecord dom, loc))
                             val r2 = cunif (loc, (L'.KRecord dom, loc))
                         in
                             unfold (r1, c1');
                             unfold (r2, c2');
                             unifyCons env r (L'.CConcat (r1, r2), loc)
                         end
                       | _ => raise ex
             in
                 unfold (r, c)
             end
             handle _ => raise ex
     in
         case (#1 c1, #1 c2) of
             (L'.CApp ((L'.CApp ((L'.CMap (dom, ran), _), f), _), r), _) =>
             unfold (dom, ran, f, r, c2)
           | (_, L'.CApp ((L'.CApp ((L'.CMap (dom, ran), _), f), _), r)) =>
             unfold (dom, ran, f, r, c1)
           | _ => raise ex
     end

 and unifyCons' env c1 c2 =
     if isUnitCon env c1 andalso isUnitCon env c2 then
         ()
     else
         let
             (*val befor = Time.now ()
              val old1 = c1
              val old2 = c2*)
             val c1 = hnormCon env c1
             val c2 = hnormCon env c2
         in
             unifyCons'' env c1 c2
             handle ex => guessMap env (c1, c2, ex)
         end

 and unifyCons'' env (c1All as (c1, loc)) (c2All as (c2, _)) =
     let
         fun err f = raise CUnify' (f (c1All, c2All))

         fun projSpecial1 (c1, n1, onFail) =
             let
                 fun trySnd () =
                     case #1 (hnormCon env c2All) of
                         L'.CProj (c2, n2) =>
                         let
                             fun tryNormal () =
                                 if n1 = n2 then
                                     unifyCons' env c1 c2
                                 else
                                     onFail ()
                         in
                             case #1 (hnormCon env c2) of
                                 L'.CUnif (_, k, _, r) =>
                                 (case #1 (hnormKind k) of
                                      L'.KTuple ks =>
                                      let
                                          val loc = #2 c2
                                          val us = map (fn k => cunif (loc, k)) ks
                                      in
                                          r := SOME (L'.CTuple us, loc);
                                          unifyCons' env c1All (List.nth (us, n2 - 1))
                                      end
                                    | _ => tryNormal ())
                            | _ => tryNormal ()
                         end
                       | _ => onFail ()
             in
                 case #1 (hnormCon env c1) of
                     L'.CUnif (_, k, _, r) =>
                     (case #1 (hnormKind k) of
                          L'.KTuple ks =>
                          let
                              val loc = #2 c1
                              val us = map (fn k => cunif (loc, k)) ks
                          in
                              r := SOME (L'.CTuple us, loc);
                              unifyCons' env (List.nth (us, n1 - 1)) c2All
                          end
                        | _ => trySnd ())
                   | _ => trySnd ()
             end

         fun projSpecial2 (c2, n2, onFail) =
             case #1 (hnormCon env c2) of
                 L'.CUnif (_, k, _, r) =>
                 (case #1 (hnormKind k) of
                      L'.KTuple ks =>
                      let
                          val loc = #2 c2
                          val us = map (fn k => cunif (loc, k)) ks
                      in
                          r := SOME (L'.CTuple us, loc);
                          unifyCons' env c1All (List.nth (us, n2 - 1))
                      end
                    | _ => onFail ())
               | _ => onFail ()

         fun isRecord' () = unifyRecordCons env (c1All, c2All)

         fun isRecord () =
             case (c1, c2) of
                 (L'.CProj (c1, n1), _) => projSpecial1 (c1, n1, isRecord')
               | (_, L'.CProj (c2, n2)) => projSpecial2 (c2, n2, isRecord')
               | _ => isRecord' ()
     in
         (*eprefaces "unifyCons''" [("c1All", p_con env c1All),
                                  ("c2All", p_con env c2All)];*)

         case (c1, c2) of
             (L'.CError, _) => ()
           | (_, L'.CError) => ()

           | (L'.CRecord _, _) => isRecord ()
           | (_, L'.CRecord _) => isRecord ()
           | (L'.CConcat _, _) => isRecord ()
           | (_, L'.CConcat _) => isRecord ()

           | (L'.CUnif (_, k1, _, r1), L'.CUnif (_, k2, _, r2)) =>
             if r1 = r2 then
                 ()
             else
                 (unifyKinds env k1 k2;
                  r1 := SOME c2All)

           | (L'.CUnif (_, _, _, r), _) =>
             if occursCon r c2All then
                 err COccursCheckFailed
             else
                 r := SOME c2All
           | (_, L'.CUnif (_, _, _, r)) =>
             if occursCon r c1All then
                 err COccursCheckFailed
             else
                 r := SOME c1All

           | (L'.CUnit, L'.CUnit) => ()

           | (L'.TFun (d1, r1), L'.TFun (d2, r2)) =>
             (unifyCons' env d1 d2;
             unifyCons' env r1 r2)
           | (L'.TCFun (expl1, x1, d1, r1), L'.TCFun (expl2, _, d2, r2)) =>
             if expl1 <> expl2 then
                 err CExplicitness
             else
                 (unifyKinds env d1 d2;
                  let
                      (*val befor = Time.now ()*)
                      val env' = E.pushCRel env x1 d1
                  in
                      (*TextIO.print ("E.pushCRel: "
                                    ^ LargeReal.toString (Time.toReal (Time.- (Time.now (), befor)))
                                    ^ "\n");*)
                      unifyCons' env' r1 r2
                  end)
           | (L'.TRecord r1, L'.TRecord r2) => unifyCons' env r1 r2
           | (L'.TDisjoint (c1, d1, e1), L'.TDisjoint (c2, d2, e2)) =>
             (unifyCons' env c1 c2;
              unifyCons' env d1 d2;
              unifyCons' env e1 e2)

           | (L'.CRel n1, L'.CRel n2) =>
             if n1 = n2 then
                 ()
             else
                 err CIncompatible
           | (L'.CNamed n1, L'.CNamed n2) =>
             if n1 = n2 then
                 ()
             else
                 err CIncompatible

           | (L'.CApp (d1, r1), L'.CApp (d2, r2)) =>
             (unifyCons' env d1 d2;
              unifyCons' env r1 r2)
           | (L'.CAbs (x1, k1, c1), L'.CAbs (_, k2, c2)) =>
             (unifyKinds env k1 k2;
              unifyCons' (E.pushCRel env x1 k1) c1 c2)

           | (L'.CName n1, L'.CName n2) =>
             if n1 = n2 then
                 ()
             else
                 err CIncompatible

           | (L'.CModProj (n1, ms1, x1), L'.CModProj (n2, ms2, x2)) =>
             if n1 = n2 andalso ms1 = ms2 andalso x1 = x2 then
                 ()
             else
                 err CIncompatible

           | (L'.CTuple cs1, L'.CTuple cs2) =>
             ((ListPair.appEq (fn (c1, c2) => unifyCons' env c1 c2) (cs1, cs2))
              handle ListPair.UnequalLengths => err CIncompatible)

           | (L'.CProj (c1, n1), _) => projSpecial1 (c1, n1, fn () => err CIncompatible)
           | (_, L'.CProj (c2, n2)) => projSpecial2 (c2, n2, fn () => err CIncompatible)

           | (L'.CMap (dom1, ran1), L'.CMap (dom2, ran2)) =>
             (unifyKinds env dom1 dom2;
              unifyKinds env ran1 ran2)

           | (L'.CKAbs (x, c1), L'.CKAbs (_, c2)) =>
             unifyCons' (E.pushKRel env x) c1 c2
           | (L'.CKApp (c1, k1), L'.CKApp (c2, k2)) =>
             (unifyKinds env k1 k2;
              unifyCons' env c1 c2)
           | (L'.TKFun (x, c1), L'.TKFun (_, c2)) =>
             unifyCons' (E.pushKRel env x) c1 c2

           | _ => err CIncompatible
     end

 and unifyCons env c1 c2 =
     unifyCons' env c1 c2
     handle CUnify' err => raise CUnify (c1, c2, err)
          | KUnify args => raise CUnify (c1, c2, CKind args)

 fun checkCon env e c1 c2 =
     unifyCons env c1 c2
     handle CUnify (c1, c2, err) =>
            expError env (Unify (e, c1, c2, err))

 fun checkPatCon env p c1 c2 =
     unifyCons env c1 c2
     handle CUnify (c1, c2, err) =>
            expError env (PatUnify (p, c1, c2, err))

 fun primType env p =
     case p of
         P.Int _ => !int
       | P.Float _ => !float
       | P.String _ => !string
                           
 datatype constraint =
          Disjoint of D.goal
        | TypeClass of E.env * L'.con * L'.exp option ref * ErrorMsg.span

 val enD = map Disjoint

 fun isClassOrFolder env cl =
     E.isClass env cl
     orelse case hnormCon env cl of
                (L'.CKApp (cl, _), _) =>
                (case hnormCon env cl of
                     (L'.CModProj (top_n, [], "folder"), _) => top_n = !top_r
                   | _ => false)
              | _ => false

 fun elabHead (env, denv) infer (e as (_, loc)) t =
     let
         fun unravel (t, e) =
             case hnormCon env t of
                 (L'.TKFun (x, t'), _) =>
                 let
                     val u = kunif loc

                     val t'' = subKindInCon (0, u) t'
                 in
                     unravel (t'', (L'.EKApp (e, u), loc))
                 end
               | (L'.TCFun (L'.Implicit, x, k, t'), _) =>
                 let
                     val u = cunif (loc, k)

                     val t'' = subConInCon (0, u) t'
                 in
                     unravel (t'', (L'.ECApp (e, u), loc))
                 end
               | (L'.TFun (dom, ran), _) =>
                 let
                     fun default () = (e, t, [])
                 in
                     case #1 (hnormCon env dom) of
                         L'.CApp (cl, x) =>
                         let
                             val cl = hnormCon env cl
                         in
                             if infer <> L.TypesOnly then
                                 if isClassOrFolder env cl then
                                     let
                                         val r = ref NONE
                                         val (e, t, gs) = unravel (ran, (L'.EApp (e, (L'.EUnif r, loc)), loc))
                                     in
                                         (e, t, TypeClass (env, dom, r, loc) :: gs)
                                     end
                                 else
                                     default ()
                             else
                                 default ()
                         end
                       | _ => default ()
                 end
               | (L'.TDisjoint (r1, r2, t'), loc) =>
                 if infer <> L.TypesOnly then
                     let
                         val gs = D.prove env denv (r1, r2, loc)
                         val (e, t, gs') = unravel (t', e)
                     in
                         (e, t, enD gs @ gs')
                     end
                 else
                     (e, t, [])
               | t => (e, t, [])
     in
         case infer of
             L.DontInfer => (e, t, [])
           | _ => unravel (t, e)
     end

fun elabPat (pAll as (p, loc), (env, bound)) =
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
                    val ((p', pt), (env, bound)) = elabPat (p, (env, bound))

                    val k = (L'.KType, loc)
                    val unifs = map (fn _ => cunif (loc, k)) xs
                    val nxs = length unifs - 1
                    val t = ListUtil.foldli (fn (i, u, t) => subConInCon (nxs - i, u) t) t unifs
                    val dn = foldl (fn (u, dn) => (L'.CApp (dn, u), loc)) dn unifs
                in
                    ignore (checkPatCon env p' pt t);
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
                                              val ((p', t), (env, bound)) = elabPat (p, (env, bound))
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

fun exhaustive (env, t, ps, loc) =
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
                    case #1 (hnormCon env t) of
                        L'.CNamed n =>
                        (let
                             val dt = E.lookupDatatype env n
                             val cons = E.constructors dt
                         in
                             dtype cons
                         end handle E.UnboundNamed _ => [Wild])
                      | L'.TRecord c =>
                        (case #1 (hnormCon env c) of
                             L'.CRecord (_, xts) =>
                             let
                                 val xts = map (fn (x, t) => (hnormCon env x, t)) xts

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
                None => false
              | Wild => true
              | Datatype cm =>
                let
                    val (t, _) = hnormCon env t

                    val dtype =
                        List.all (fn (_, n, to) =>
                                     case IM.find (cm, n) of
                                         NONE => false
                                       | SOME c' =>
                                         case to of
                                             NONE => true
                                           | SOME t' => isTotal (c', t'))

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
                      | L'.CError => true
                      | c =>
                        (prefaces "Not a datatype" [("loc", PD.string (ErrorMsg.spanToString loc)),
                                                    ("c", p_con env (c, ErrorMsg.dummySpan))];
                         raise Fail "isTotal: Not a datatype")
                end
              | Record _ => List.all (fn c2 => coverageImp (c, c2)) (enumerateCases depth t)
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

fun normClassKey envs c =
    let
        val c = hnormCon envs c
    in
        case #1 c of
            L'.CApp (c1, c2) =>
            let
                val c1 = normClassKey envs c1
                val c2 = normClassKey envs c2
            in
                (L'.CApp (c1, c2), #2 c)
            end
          | _ => c
    end

fun normClassConstraint env (c, loc) =
    case c of
        L'.CApp (f, x) =>
        let
            val f = unmodCon env f
            val x = normClassKey env x
        in
            (L'.CApp (f, x), loc)
        end
      | L'.TFun (c1, c2) =>
        let
            val c1 = normClassConstraint env c1
            val c2 = normClassConstraint env c2
        in
            (L'.TFun (c1, c2), loc)
        end
      | L'.TCFun (expl, x, k, c1) => (L'.TCFun (expl, x, k, normClassConstraint env c1), loc)
      | L'.CUnif (_, _, _, ref (SOME c)) => normClassConstraint env c
      | _ => (c, loc)

fun elabExp (env, denv) (eAll as (e, loc)) =
    let
        (*val () = eprefaces "elabExp" [("eAll", SourcePrint.p_exp eAll)];*)
        (*val befor = Time.now ()*)

        val r = case e of
            L.EAnnot (e, t) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (t', _, gs2) = elabCon (env, denv) t
            in
                checkCon env e' et t';
                (e', t', gs1 @ enD gs2)
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
            in
                checkCon env e1' t1 t;
                checkCon env e2' t2 dom;
                ((L'.EApp (e1', e2'), loc), ran, gs1 @ gs2)
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
                val dom = normClassConstraint env t'
                val (e', et, gs2) = elabExp (E.pushERel env x dom, denv) e
            in
                ((L'.EAbs (x, t', et, e'), loc),
                 (L'.TFun (t', et), loc),
                 enD gs1 @ gs2)
            end
          | L.ECApp (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val oldEt = et
                val (c', ck, gs2) = elabCon (env, denv) c
                val (et', _) = hnormCon env et
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
                        ((L'.ECApp (e', c'), loc), eb', gs1 @ enD gs2)
                    end

                  | _ =>
                    (expError env (WrongForm ("constructor function", e', et));
                     (eerror, cerror, []))
            end
          | L.ECAbs (expl, x, k, e) =>
            let
                val expl' = elabExplicitness expl
                val k' = elabKind env k

                val env' = E.pushCRel env x k'
                val (e', et, gs) = elabExp (env', D.enter denv) e
            in
                ((L'.ECAbs (expl', x, k', e'), loc),
                 (L'.TCFun (expl', x, k', et), loc),
                 gs)
            end
          | L.EKAbs (x, e) =>
            let
                val env' = E.pushKRel env x
                val (e', et, gs) = elabExp (env', denv) e
            in
                ((L'.EKAbs (x, e'), loc),
                 (L'.TKFun (x, et), loc),
                 gs)
            end

          | L.EDisjoint (c1, c2, e) =>
            let
                val (c1', k1, gs1) = elabCon (env, denv) c1
                val (c2', k2, gs2) = elabCon (env, denv) c2

                val ku1 = kunif loc
                val ku2 = kunif loc

                val denv' = D.assert env denv (c1', c2')
                val (e', t, gs3) = elabExp (env, denv') e
            in
                checkKind env c1' k1 (L'.KRecord ku1, loc);
                checkKind env c2' k2 (L'.KRecord ku2, loc);

                (e', (L'.TDisjoint (c1', c2', t), loc), enD gs1 @ enD gs2 @ gs3)
            end
          | L.EDisjointApp e =>
            let
                val (e', t, gs1) = elabExp (env, denv) e

                val k1 = kunif loc
                val c1 = cunif (loc, (L'.KRecord k1, loc))
                val k2 = kunif loc
                val c2 = cunif (loc, (L'.KRecord k2, loc))
                val t' = cunif (loc, ktype)
                val () = checkCon env e' t (L'.TDisjoint (c1, c2, t'), loc)
                val gs2 = D.prove env denv (c1, c2, loc)
            in
                (e', t', enD gs2 @ gs1)
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
                val () = checkCon env e' et
                                  (L'.TRecord (L'.CConcat (first, rest), loc), loc);
                val gs3 = D.prove env denv (first, rest, loc)
            in
                ((L'.EField (e', c', {field = ft, rest = rest}), loc), ft, gs1 @ enD gs2 @ enD gs3)
            end

          | L.EConcat (e1, e2) =>
            let
                val (e1', e1t, gs1) = elabExp (env, denv) e1
                val (e2', e2t, gs2) = elabExp (env, denv) e2

                val r1 = cunif (loc, ktype_record)
                val r2 = cunif (loc, ktype_record)

                val () = checkCon env e1' e1t (L'.TRecord r1, loc)
                val () = checkCon env e2' e2t (L'.TRecord r2, loc)

                val gs3 = D.prove env denv (r1, r2, loc)
            in
                ((L'.EConcat (e1', r1, e2', r2), loc),
                 (L'.TRecord ((L'.CConcat (r1, r2), loc)), loc),
                 gs1 @ gs2 @ enD gs3)
            end
          | L.ECut (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (c', ck, gs2) = elabCon (env, denv) c

                val ft = cunif (loc, ktype)
                val rest = cunif (loc, ktype_record)
                val first = (L'.CRecord (ktype, [(c', ft)]), loc)

                val () = checkCon env e' et
                                  (L'.TRecord (L'.CConcat (first, rest), loc), loc)
                            
                val gs3 = D.prove env denv (first, rest, loc)
            in
                ((L'.ECut (e', c', {field = ft, rest = rest}), loc), (L'.TRecord rest, loc),
                 gs1 @ enD gs2 @ enD gs3)
            end
          | L.ECutMulti (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (c', ck, gs2) = elabCon (env, denv) c

                val rest = cunif (loc, ktype_record)

                val () = checkCon env e' et
                                  (L'.TRecord (L'.CConcat (c', rest), loc), loc)
                            
                val gs3 = D.prove env denv (c', rest, loc)
            in
                ((L'.ECutMulti (e', c', {rest = rest}), loc), (L'.TRecord rest, loc),
                 gs1 @ enD gs2 @ enD gs3)
            end

          | L.ECase (e, pes) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val result = cunif (loc, (L'.KType, loc))
                val (pes', gs) = ListUtil.foldlMap
                                 (fn ((p, e), gs) =>
                                     let
                                         val ((p', pt), (env, _)) = elabPat (p, (env, SS.empty))

                                         val (e', et', gs1) = elabExp (env, denv) e
                                     in
                                         checkPatCon env p' pt et;
                                         checkCon env e' et' result;
                                         ((p', e'), gs1 @ gs)
                                     end)
                                 gs1 pes
            in
                if exhaustive (env, et, map #1 pes', loc) then
                    ()
                else
                    expError env (Inexhaustive loc);

                ((L'.ECase (e', pes', {disc = et, result = result}), loc), result, gs)
            end

          | L.ELet (eds, e) =>
            let
                val (eds, (env, gs1)) = ListUtil.foldlMap (elabEdecl denv) (env, []) eds
                val (e, t, gs2) = elabExp (env, denv) e
            in
                ((L'.ELet (eds, e), loc), t, gs1 @ gs2)
            end
    in
        (*prefaces "elabExp" [("e", SourcePrint.p_exp eAll),
                            ("t", PD.string (LargeReal.toString (Time.toReal (Time.- (Time.now (), befor)))))];*)
        r
    end

and elabEdecl denv (dAll as (d, loc), (env, gs)) =
    let
        val r = 
            case d of
                L.EDVal (x, co, e) =>
                let
                    val (c', _, gs1) = case co of
                                           NONE => (cunif (loc, ktype), ktype, [])
                                         | SOME c => elabCon (env, denv) c

                    val (e', et, gs2) = elabExp (env, denv) e

                    val () = checkCon env e' et c'

                    val c' = normClassConstraint env c'
                    val env' = E.pushERel env x c'
                in
                    ((L'.EDVal (x, c', e'), loc), (env', enD gs1 @ gs2 @ gs))
                end
              | L.EDValRec vis =>
                let
                    fun allowable (e, _) =
                        case e of
                            L.EAbs _ => true
                          | L.ECAbs (_, _, _, e) => allowable e
                          | L.EKAbs (_, e) => allowable e
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

                    val env = foldl (fn ((x, c', _), env) => E.pushERel env x c') env vis

                    val (vis, gs) = ListUtil.foldlMap (fn ((x, c', e), gs) =>
                                                          let
                                                              val (e', et, gs1) = elabExp (env, denv) e
                                                          in
                                                              checkCon env e' et c';
                                                              if allowable e then
                                                                  ()
                                                              else
                                                                  expError env (IllegalRec (x, e'));
                                                              ((x, c', e'), gs1 @ gs)
                                                          end) gs vis
                in
                    ((L'.EDValRec vis, loc), (env, gs))
                end
    in
        r
    end

val hnormSgn = E.hnormSgn

fun tableOf () = (L'.CModProj (!basis_r, [], "sql_table"), ErrorMsg.dummySpan)
fun sequenceOf () = (L'.CModProj (!basis_r, [], "sql_sequence"), ErrorMsg.dummySpan)
fun cookieOf () = (L'.CModProj (!basis_r, [], "http_cookie"), ErrorMsg.dummySpan)

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
        in
            case cso of
                NONE => (strError env (UnboundStr (loc, str));
                         denv)
              | SOME cs => foldl (fn ((c1, c2), denv) =>
                                     D.assert env denv (c1, c2)) denv cs
        end

fun elabSgn_item ((sgi, loc), (env, denv, gs)) =
    case sgi of
        L.SgiConAbs (x, k) =>
        let
            val k' = elabKind env k

            val (env', n) = E.pushCNamed env x k' NONE
        in
            ([(L'.SgiConAbs (x, n, k'), loc)], (env', denv, gs))
        end

      | L.SgiCon (x, ko, c) =>
        let
            val k' = case ko of
                         NONE => kunif loc
                       | SOME k => elabKind env k

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
                 case hnormCon env (L'.CModProj (n, ms, s), loc) of
                     (L'.CModProj (n, ms, s), _) =>
                     (case E.projectDatatype env {sgn = sgn, str = str, field = s} of
                          NONE => (conError env (UnboundDatatype (loc, s));
                                   ([], (env, denv, [])))
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
                              ([(L'.SgiDatatypeImp (x, n', n, ms, s, xs, xncs), loc)], (env, denv, []))
                          end)
                   | _ => (strError env (NotDatatype loc);
                           ([], (env, denv, [])))
             end)

      | L.SgiVal (x, c) =>
        let
            val (c', ck, gs') = elabCon (env, denv) c

            val (env', n) = E.pushENamed env x c'
            val c' = normClassConstraint env c'
        in
            (unifyKinds env ck ktype
             handle KUnify ue => strError env (NotType (loc, ck, ue)));

            ([(L'.SgiVal (x, n, c'), loc)], (env', denv, gs' @ gs))
        end

      | L.SgiTable (x, c, pe, ce) =>
        let
            val cstK = (L'.KRecord (L'.KRecord (L'.KUnit, loc), loc), loc)
            val x' = x ^ "_hidden_constraints"
            val (env', hidden_n) = E.pushCNamed env x' cstK NONE
            val hidden = (L'.CNamed hidden_n, loc)

            val (c', ck, gs') = elabCon (env, denv) c
            val pkey = cunif (loc, cstK)
            val visible = cunif (loc, cstK)
            val uniques = (L'.CConcat (visible, hidden), loc)

            val ct = tableOf ()
            val ct = (L'.CApp (ct, c'), loc)
            val ct = (L'.CApp (ct, (L'.CConcat (pkey, uniques), loc)), loc)

            val (pe', pet, gs'') = elabExp (env', denv) pe
            val gs'' = List.mapPartial (fn Disjoint x => SOME x
                                          | _ => NONE) gs''

            val pst = (L'.CModProj (!basis_r, [], "primary_key"), loc)
            val pst = (L'.CApp (pst, c'), loc)
            val pst = (L'.CApp (pst, pkey), loc)

            val (env', n) = E.pushENamed env' x ct

            val (ce', cet, gs''') = elabExp (env', denv) ce
            val gs''' = List.mapPartial (fn Disjoint x => SOME x
                                         | _ => NONE) gs'''

            val cst = (L'.CModProj (!basis_r, [], "sql_constraints"), loc)
            val cst = (L'.CApp (cst, c'), loc)
            val cst = (L'.CApp (cst, visible), loc)
        in
            checkKind env c' ck (L'.KRecord (L'.KType, loc), loc);
            checkCon env' pe' pet pst;
            checkCon env' ce' cet cst;

            ([(L'.SgiConAbs (x', hidden_n, cstK), loc),
              (L'.SgiVal (x, n, ct), loc)], (env', denv, gs''' @ gs'' @ gs' @ gs))
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

            val denv = D.assert env denv (c1', c2')
        in
            checkKind env c1' k1 (L'.KRecord (kunif loc), loc);
            checkKind env c2' k2 (L'.KRecord (kunif loc), loc);

            ([(L'.SgiConstraint (c1', c2'), loc)], (env, denv, gs1 @ gs2))
        end

      | L.SgiClassAbs (x, k) =>
        let
            val k = elabKind env k
            val k' = (L'.KArrow (k, (L'.KType, loc)), loc)
            val (env, n) = E.pushCNamed env x k' NONE
            val env = E.pushClass env n
        in
            ([(L'.SgiClassAbs (x, n, k), loc)], (env, denv, []))
        end

      | L.SgiClass (x, k, c) =>
        let
            val k = elabKind env k
            val k' = (L'.KArrow (k, (L'.KType, loc)), loc)
            val (c', ck, gs) = elabCon (env, denv) c
            val (env, n) = E.pushCNamed env x k' (SOME c')
            val env = E.pushClass env n
        in
            checkKind env c' ck k';
            ([(L'.SgiClass (x, n, k, c'), loc)], (env, denv, []))
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
                                | L'.SgiClassAbs (x, _, _) =>
                                  (if SS.member (cons, x) then
                                       sgnError env (DuplicateCon (loc, x))
                                   else
                                       ();
                                   (SS.add (cons, x), vals, sgns, strs))
                                | L'.SgiClass (x, _, _, _) =>
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
            val denv' = dopenConstraints (loc, env', denv) {str = m, strs = []}
            val (ran', gs2) = elabSgn (env', denv') ran
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
                                   (unifyKinds env k ck
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
                                                              

and selfify env {str, strs, sgn} =
    case #1 (hnormSgn env sgn) of
        L'.SgnError => sgn
      | L'.SgnVar _ => sgn

      | L'.SgnConst sgis =>
        (L'.SgnConst (map (fn (L'.SgiConAbs (x, n, k), loc) =>
                              (L'.SgiCon (x, n, k, (L'.CModProj (str, strs, x), loc)), loc)
                            | (L'.SgiDatatype (x, n, xs, xncs), loc) =>
                              (L'.SgiDatatypeImp (x, n, str, strs, x, xs, xncs), loc)
                            | (L'.SgiClassAbs (x, n, k), loc) =>
                              (L'.SgiClass (x, n, k, (L'.CModProj (str, strs, x), loc)), loc)
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

and selfifyAt env {str, sgn} =
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

and dopen env {str, strs, sgn} =
    let
        val m = foldl (fn (m, str) => (L'.StrProj (str, m), #2 sgn))
                (L'.StrVar str, #2 sgn) strs
    in
        case #1 (hnormSgn env sgn) of
            L'.SgnConst sgis =>
            ListUtil.foldlMap (fn ((sgi, loc), env') =>
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
                                            | L'.SgiClassAbs (x, n, k) =>
                                              let
                                                  val k' = (L'.KArrow (k, (L'.KType, loc)), loc)
                                                  val c = (L'.CModProj (str, strs, x), loc)
                                              in
                                                  (L'.DCon (x, n, k', c), loc)
                                              end
                                            | L'.SgiClass (x, n, k, _) =>
                                              let
                                                  val k' = (L'.KArrow (k, (L'.KType, loc)), loc)
                                                  val c = (L'.CModProj (str, strs, x), loc)
                                              in
                                                  (L'.DCon (x, n, k', c), loc)
                                              end
                                  in
                                      (d, E.declBinds env' d)
                                  end)
                              env sgis
          | _ => (strError env (UnOpenable sgn);
                  ([], env))
    end

and sgiOfDecl (d, loc) =
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
      | L'.DTable (tn, x, n, c, _, pc, _, cc) =>
        [(L'.SgiVal (x, n, (L'.CApp ((L'.CApp (tableOf (), c), loc),
                                     (L'.CConcat (pc, cc), loc)), loc)), loc)]
      | L'.DSequence (tn, x, n) => [(L'.SgiVal (x, n, sequenceOf ()), loc)]
      | L'.DClass (x, n, k, c) => [(L'.SgiClass (x, n, k, c), loc)]
      | L'.DDatabase _ => []
      | L'.DCookie (tn, x, n, c) => [(L'.SgiVal (x, n, (L'.CApp (cookieOf (), c), loc)), loc)]

and subSgn env sgn1 (sgn2 as (_, loc2)) =
    ((*prefaces "subSgn" [("sgn1", p_sgn env sgn1),
                        ("sgn2", p_sgn env sgn2)];*)
    case (#1 (hnormSgn env sgn1), #1 (hnormSgn env sgn2)) of
        (L'.SgnError, _) => ()
      | (_, L'.SgnError) => ()

      | (L'.SgnConst sgis1, L'.SgnConst sgis2) =>
        let
            (*val () = prefaces "subSgn" [("sgn1", p_sgn env sgn1),
                                        ("sgn2", p_sgn env sgn2),
                                        ("sgis1", p_sgn env (L'.SgnConst sgis1, loc2)),
                                        ("sgis2", p_sgn env (L'.SgnConst sgis2, loc2))]*)

            fun folder (sgi2All as (sgi, loc), env) =
                let
                    (*val () = prefaces "folder" [("sgis1", p_sgn env (L'.SgnConst sgis1, loc2))]*)

                    fun seek p =
                        let
                            fun seek env ls =
                                case ls of
                                    [] => (sgnError env (UnmatchedSgi sgi2All);
                                           env)
                                  | h :: t =>
                                    case p (env, h) of
                                        NONE =>
                                        let
                                            val env = case #1 h of
                                                          L'.SgiCon (x, n, k, c) =>
                                                          if E.checkENamed env n then
                                                              env
                                                          else
                                                              E.pushCNamedAs env x n k (SOME c)
                                                        | L'.SgiConAbs (x, n, k) =>
                                                          if E.checkENamed env n then
                                                              env
                                                          else
                                                              E.pushCNamedAs env x n k NONE
                                                        | _ => env
                                        in
                                            seek (E.sgiBinds env h) t
                                        end
                                      | SOME envs => envs
                        in
                            seek env sgis1
                        end
                in
                    case sgi of
                        L'.SgiConAbs (x, n2, k2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 let
                                     fun found (x', n1, k1, co1) =
                                         if x = x' then
                                             let
                                                 val () = unifyKinds env k1 k2
                                                     handle KUnify (k1, k2, err) =>
                                                            sgnError env (SgiWrongKind (sgi1All, k1, sgi2All, k2, err))
                                                 val env = E.pushCNamedAs env x n1 k1 co1
                                             in
                                                 SOME (if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k2 (SOME (L'.CNamed n1, loc2)))
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
                                       | L'.SgiClassAbs (x', n1, k) => found (x', n1,
                                                                              (L'.KArrow (k,
                                                                                          (L'.KType, loc)), loc),
                                                                              NONE)
                                       | L'.SgiClass (x', n1, k, c) => found (x', n1,
                                                                              (L'.KArrow (k,
                                                                                          (L'.KType, loc)), loc),
                                                                              SOME c)
                                       | _ => NONE
                                 end)

                      | L'.SgiCon (x, n2, k2, c2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
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
                                                         SOME env
                                                     end
                                             in
                                                 (unifyCons env c1 c2;
                                                  good ())
                                                 handle CUnify (c1, c2, err) =>
                                                        (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                         good ())
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiCon (x', n1, k1, c1) => found (x', n1, k1, c1)
                                       | L'.SgiClass (x', n1, k1, c1) =>
                                         found (x', n1, (L'.KArrow (k1, (L'.KType, loc)), loc), c1)
                                       | _ => NONE
                                 end)

                      | L'.SgiDatatype (x, n2, xs2, xncs2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 let
                                     fun found (n1, xs1, xncs1) =
                                         let
                                             fun mismatched ue =
                                                 (sgnError env (SgiMismatchedDatatypes (sgi1All, sgi2All, ue));
                                                  SOME env)

                                             val k = (L'.KType, loc)
                                             val k' = foldl (fn (_, k') => (L'.KArrow (k, k'), loc)) k xs1

                                             fun good () =
                                                 let
                                                     val env = E.sgiBinds env sgi1All
                                                     val env = if n1 = n2 then
                                                                   env
                                                               else
                                                                   E.pushCNamedAs env x n2 k'
                                                                                  (SOME (L'.CNamed n1, loc))
                                                 in
                                                     SOME env
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
                                                            (unifyCons env t1 t2; false)
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
                        seek (fn (env, sgi1All as (sgi1, _)) =>
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
                                                     SOME env
                                                 end
                                         in
                                             (unifyCons env t1 t2;
                                              good ())
                                             handle CUnify (c1, c2, err) =>
                                                    (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                     good ())
                                         end
                                     else
                                         NONE

                                   | _ => NONE)

                      | L'.SgiVal (x, n2, c2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 case sgi1 of
                                     L'.SgiVal (x', n1, c1) =>
                                     if x = x' then
                                         (unifyCons env c1 c2;
                                          SOME env)
                                         handle CUnify (c1, c2, err) =>
                                                (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                 SOME env)
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiStr (x, n2, sgn2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 case sgi1 of
                                     L'.SgiStr (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn env sgn1 sgn2
                                             val env = E.pushStrNamedAs env x n1 sgn1
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushStrNamedAs env x n2
                                                                            (selfifyAt env {str = (L'.StrVar n1, #2 sgn2),
                                                                                            sgn = sgn2})
                                         in
                                             SOME env
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiSgn (x, n2, sgn2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 case sgi1 of
                                     L'.SgiSgn (x', n1, sgn1) =>
                                     if x = x' then
                                         let
                                             val () = subSgn env sgn1 sgn2
                                             val () = subSgn env sgn2 sgn1

                                             val env = E.pushSgnNamedAs env x n2 sgn2
                                             val env = if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushSgnNamedAs env x n1 sgn2
                                         in
                                             SOME env
                                         end
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiConstraint (c2, d2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 case sgi1 of
                                     L'.SgiConstraint (c1, d1) =>
                                     if consEq env (c1, c2) andalso consEq env (d1, d2) then
                                         SOME env
                                     else
                                         NONE
                                   | _ => NONE)

                      | L'.SgiClassAbs (x, n2, k2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 let
                                     fun found (x', n1, k1, co) =
                                         if x = x' then
                                             let
                                                 val () = unifyKinds env k1 k2
                                                     handle KUnify (k1, k2, err) =>
                                                            sgnError env (SgiWrongKind (sgi1All, k1, sgi2All, k2, err))

                                                 val k = (L'.KArrow (k1, (L'.KType, loc)), loc)
                                                 val env = E.pushCNamedAs env x n1 k co
                                             in
                                                 SOME (if n1 = n2 then
                                                           env
                                                       else
                                                           E.pushCNamedAs env x n2 k (SOME (L'.CNamed n1, loc2)))
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiClassAbs (x', n1, k1) => found (x', n1, k1, NONE)
                                       | L'.SgiClass (x', n1, k1, c) => found (x', n1, k1, SOME c)
                                       | _ => NONE
                                 end)
                      | L'.SgiClass (x, n2, k2, c2) =>
                        seek (fn (env, sgi1All as (sgi1, _)) =>
                                 let
                                     val k = (L'.KArrow (k2, (L'.KType, loc)), loc)

                                     fun found (x', n1, k1, c1) =
                                         if x = x' then
                                             let
                                                 val () = unifyKinds env k1 k2
                                                     handle KUnify (k1, k2, err) =>
                                                            sgnError env (SgiWrongKind (sgi1All, k1, sgi2All, k2, err))

                                                 fun good () =
                                                     let
                                                         val env = E.pushCNamedAs env x n2 k (SOME c2)
                                                         val env = if n1 = n2 then
                                                                       env
                                                                   else
                                                                       E.pushCNamedAs env x n1 k (SOME c1)
                                                     in
                                                         SOME env
                                                     end
                                             in
                                                 (unifyCons env c1 c2;
                                                  good ())
                                                 handle CUnify (c1, c2, err) =>
                                                        (sgnError env (SgiWrongCon (sgi1All, c1, sgi2All, c2, err));
                                                         good ())
                                             end
                                         else
                                             NONE
                                 in
                                     case sgi1 of
                                         L'.SgiClass (x', n1, k1, c1) => found (x', n1, k1, c1)
                                       | _ => NONE
                                 end)
                end
        in
            ignore (foldl folder env sgis2)
        end

      | (L'.SgnFun (m1, n1, dom1, ran1), L'.SgnFun (m2, n2, dom2, ran2)) =>
        let
            val ran2 =
                if n1 = n2 then
                    ran2
                else
                    subStrInSgn (n2, n1) ran2
        in
            subSgn env dom2 dom1;
            subSgn (E.pushStrNamedAs env m1 n1 dom2) ran1 ran2
        end

      | _ => sgnError env (SgnWrongForm (sgn1, sgn2)))


and positive self =
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
              | TDisjoint (c1, c2, c3) => none c1 andalso none c2 andalso none c3

              | CKAbs _ => false
              | TKFun _ => false

              | CName _ => true

              | CRecord xcs => List.all (fn (c1, c2) => none c1 andalso none c2) xcs
              | CConcat (c1, c2) => none c1 andalso none c2
              | CMap => true

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
              | TDisjoint (c1, c2, c3) => none c1 andalso none c2 andalso none c3

              | CKAbs _ => false
              | TKFun _ => false

              | CName _ => true

              | CRecord xcs => List.all (fn (c1, c2) => none c1 andalso pos c2) xcs
              | CConcat (c1, c2) => pos c1 andalso pos c2
              | CMap => true

              | CUnit => true

              | CTuple cs => List.all pos cs
              | CProj (c, _) => pos c

              | CWild _ => false
    in
        pos
    end

and wildifyStr env (str, sgn) =
    case #1 (hnormSgn env sgn) of
        L'.SgnConst sgis =>
        (case #1 str of
             L.StrConst ds =>
             let
                 fun decompileKind (k, loc) =
                     case k of
                         L'.KType => SOME (L.KType, loc)
                       | L'.KArrow (k1, k2) =>
                         (case (decompileKind k1, decompileKind k2) of
                              (SOME k1, SOME k2) => SOME (L.KArrow (k1, k2), loc)
                            | _ => NONE)
                       | L'.KName => SOME (L.KName, loc)
                       | L'.KRecord k =>
                         (case decompileKind k of
                              SOME k => SOME (L.KRecord k, loc)
                            | _ => NONE)
                       | L'.KUnit => SOME (L.KUnit, loc)
                       | L'.KTuple ks =>
                         let
                             val ks' = List.mapPartial decompileKind ks
                         in
                             if length ks' = length ks then
                                 SOME (L.KTuple ks', loc)
                             else
                                 NONE
                         end

                       | L'.KError => NONE
                       | L'.KUnif (_, _, ref (SOME k)) => decompileKind k
                       | L'.KUnif _ => NONE

                       | L'.KRel _ => NONE
                       | L'.KFun _ => NONE

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

                 val (neededC, constraints, neededV, _) =
                     foldl (fn ((sgi, loc), (neededC, constraints, neededV, env')) =>
                               let
                                   val (needed, constraints, neededV) =
                                       case sgi of
                                           L'.SgiConAbs (x, _, k) => (SM.insert (neededC, x, k), constraints, neededV)
                                         | L'.SgiConstraint cs => (neededC, (env', cs, loc) :: constraints, neededV)

                                         | L'.SgiVal (x, _, t) =>
                                           let
                                               fun default () = (neededC, constraints, neededV)

                                               val t = normClassConstraint env' t
                                           in
                                               case #1 t of
                                                   L'.CApp (f, _) =>
                                                   if isClassOrFolder env' f then
                                                       (neededC, constraints, SS.add (neededV, x))
                                                   else
                                                       default ()
                                                 | _ => default ()
                                           end

                                         | _ => (neededC, constraints, neededV)
                               in
                                   (needed, constraints, neededV, E.sgiBinds env' (sgi, loc))
                               end)
                           (SM.empty, [], SS.empty, env) sgis
                                                              
                 val (neededC, neededV) =
                     foldl (fn ((d, _), needed as (neededC, neededV)) =>
                               case d of
                                   L.DCon (x, _, _) => ((#1 (SM.remove (neededC, x)), neededV)
                                                        handle NotFound =>
                                                               needed)
                                 | L.DClass (x, _, _) => ((#1 (SM.remove (neededC, x)), neededV)
                                                          handle NotFound => needed)
                                 | L.DVal (x, _, _) => ((neededC, SS.delete (neededV, x))
                                                        handle NotFound => needed)
                                 | L.DOpen _ => (SM.empty, SS.empty)
                                 | _ => needed)
                           (neededC, neededV) ds

                 val ds' = List.mapPartial (fn (env', (c1, c2), loc) =>
                                               case (decompileCon env' c1, decompileCon env' c2) of
                                                   (SOME c1, SOME c2) =>
                                                   SOME (L.DConstraint (c1, c2), loc)
                                                 | _ => NONE) constraints

                 val ds' =
                     case SS.listItems neededV of
                         [] => ds'
                       | xs =>
                         let
                             val ewild = (L.EWild, #2 str)
                             val ds'' = map (fn x => (L.DVal (x, NONE, ewild), #2 str)) xs
                         in
                             ds'' @ ds'
                         end

                 val ds' =
                     case SM.listItemsi neededC of
                         [] => ds'
                       | xs =>
                         let
                             val ds'' = map (fn (x, k) =>
                                                let
                                                    val k =
                                                        case decompileKind k of
                                                            NONE => (L.KWild, #2 str)
                                                          | SOME k => k
                                                    val cwild = (L.CWild k, #2 str)
                                                in
                                                    (L.DCon (x, NONE, cwild), #2 str)
                                                end) xs
                         in
                             ds'' @ ds'
                         end
             in
                 (L.StrConst (ds @ ds'), #2 str)
             end
           | _ => str)
      | _ => str

and elabDecl (dAll as (d, loc), (env, denv, gs)) =
    let
        (*val () = preface ("elabDecl", SourcePrint.p_decl (d, loc))*)
        (*val befor = Time.now ()*)

        val r = 
            case d of
                L.DCon (x, ko, c) =>
                let
                    val k' = case ko of
                                 NONE => kunif loc
                               | SOME k => elabKind env k

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
                    (*if positive then
                        ()
                    else
                        declError env (Nonpositive d');*)

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
                         case hnormCon env (L'.CModProj (n, ms, s), loc) of
                             (L'.CModProj (n, ms, s), _) =>
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
                                      ([(L'.DDatatypeImp (x, n', n, ms, s, xs, xncs), loc)], (env, denv, gs))
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

                    val () = checkCon env e' et c'

                    val c' = normClassConstraint env c'
                    val (env', n) = E.pushENamed env x c'
                in
                    (*prefaces "DVal" [("x", Print.PD.string x),
                                     ("c'", p_con env c')];*)
                    ([(L'.DVal (x, n, c', e'), loc)], (env', denv, enD gs1 @ gs2 @ gs))
                end
              | L.DValRec vis =>
                let
                    fun allowable (e, _) =
                        case e of
                            L.EAbs _ => true
                          | L.ECAbs (_, _, _, e) => allowable e
                          | L.EKAbs (_, e) => allowable e
                          | L.EDisjoint (_, _, e) => allowable e
                          | _ => false

                    val (vis, gs) = ListUtil.foldlMap
                                        (fn ((x, co, e), gs) =>
                                            let
                                                val (c', _, gs1) = case co of
                                                                       NONE => (cunif (loc, ktype), ktype, [])
                                                                     | SOME c => elabCon (env, denv) c
                                                val c' = normClassConstraint env c'
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
                                                          in
                                                              checkCon env e' et c';
                                                              if allowable e then
                                                                  ()
                                                              else
                                                                  expError env (IllegalRec (x, e'));
                                                              ((x, n, c', e'), gs1 @ gs)
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
                                subSgn env (selfifyAt env {str = str', sgn = actual}) formal;
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

                         val (ds, env') = dopen env {str = n, strs = ms, sgn = sgn}
                         val denv' = dopenConstraints (loc, env', denv) {str = m, strs = ms}
                     in
                         (ds, (env', denv', gs))
                     end)

              | L.DConstraint (c1, c2) =>
                let
                    val (c1', k1, gs1) = elabCon (env, denv) c1
                    val (c2', k2, gs2) = elabCon (env, denv) c2
                    val gs3 = D.prove env denv (c1', c2', loc)

                    val denv' = D.assert env denv (c1', c2')
                in
                    checkKind env c1' k1 (L'.KRecord (kunif loc), loc);
                    checkKind env c2' k2 (L'.KRecord (kunif loc), loc);

                    ([(L'.DConstraint (c1', c2'), loc)], (env, denv', enD gs1 @ enD gs2 @ enD gs3 @ gs))
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
                                                 case hnormCon env ran of
                                                     (L'.CApp (tf, arg), _) =>
                                                     (case (hnormCon env tf, hnormCon env arg) of
                                                          ((L'.CModProj (basis, [], "transaction"), _),
                                                           (L'.CApp (tf, arg3), _)) =>
                                                          (case (basis = !basis_r,
                                                                 hnormCon env tf, hnormCon env arg3) of
                                                               (true,
                                                                (L'.CApp (tf, arg2), _),
                                                                ((L'.CRecord (_, []), _))) =>
                                                               (case (hnormCon env tf) of
                                                                    (L'.CApp (tf, arg1), _) =>
                                                                    (case (hnormCon env tf,
                                                                           hnormCon env arg1,
                                                                           hnormCon env arg2) of
                                                                         (tf, arg1,
                                                                          (L'.CRecord (_, []), _)) =>
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
                                             case hnormCon env t of
                                                 (L'.TFun (dom, ran), _) =>
                                                 (case hnormCon env dom of
                                                      (L'.TRecord domR, _) =>
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

              | L.DTable (x, c, pe, ce) =>
                let
                    val cstK = (L'.KRecord (L'.KRecord (L'.KUnit, loc), loc), loc)

                    val (c', k, gs') = elabCon (env, denv) c
                    val pkey = cunif (loc, cstK)
                    val uniques = cunif (loc, cstK)

                    val ct = tableOf ()
                    val ct = (L'.CApp (ct, c'), loc)
                    val ct = (L'.CApp (ct, (L'.CConcat (pkey, uniques), loc)), loc)

                    val (env, n) = E.pushENamed env x ct
                    val (pe', pet, gs'') = elabExp (env, denv) pe
                    val (ce', cet, gs''') = elabExp (env, denv) ce

                    val pst = (L'.CModProj (!basis_r, [], "primary_key"), loc)
                    val pst = (L'.CApp (pst, c'), loc)
                    val pst = (L'.CApp (pst, pkey), loc)

                    val cst = (L'.CModProj (!basis_r, [], "sql_constraints"), loc)
                    val cst = (L'.CApp (cst, c'), loc)
                    val cst = (L'.CApp (cst, uniques), loc)
                in
                    checkKind env c' k (L'.KRecord (L'.KType, loc), loc);
                    checkCon env pe' pet pst;
                    checkCon env ce' cet cst;
                    ([(L'.DTable (!basis_r, x, n, c', pe', pkey, ce', uniques), loc)],
                     (env, denv, gs''' @ gs'' @ enD gs' @ gs))
                end
              | L.DSequence x =>
                let
                    val (env, n) = E.pushENamed env x (sequenceOf ())
                in
                    ([(L'.DSequence (!basis_r, x, n), loc)], (env, denv, gs))
                end

              | L.DClass (x, k, c) =>
                let
                    val k = elabKind env k
                    val k' = (L'.KArrow (k, (L'.KType, loc)), loc)
                    val (c', ck, gs') = elabCon (env, denv) c
                    val (env, n) = E.pushCNamed env x k' (SOME c')
                    val env = E.pushClass env n
                in
                    checkKind env c' ck k';
                    ([(L'.DClass (x, n, k, c'), loc)], (env, denv, enD gs' @ gs))
                end

              | L.DDatabase s => ([(L'.DDatabase s, loc)], (env, denv, gs))

              | L.DCookie (x, c) =>
                let
                    val (c', k, gs') = elabCon (env, denv) c
                    val (env, n) = E.pushENamed env x (L'.CApp (cookieOf (), c'), loc)
                in
                    checkKind env c' k (L'.KType, loc);
                    ([(L'.DCookie (!basis_r, x, n, c'), loc)], (env, denv, enD gs' @ gs))
                end

        (*val tcs = List.filter (fn TypeClass _ => true | _ => false) (#3 (#2 r))*)
    in
        (*prefaces "elabDecl" [("e", SourcePrint.p_decl dAll),
                             ("t", PD.string (LargeReal.toString (Time.toReal
                                                                      (Time.- (Time.now (), befor)))))];*)

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
                            | L'.SgiClassAbs (x, n, k) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiClassAbs (x, n, k), loc) :: sgis, cons, vals, sgns, strs)
                              end
                            | L'.SgiClass (x, n, k, c) =>
                              let
                                  val (cons, x) =
                                      if SS.member (cons, x) then
                                          (cons, "?" ^ x)
                                      else
                                          (SS.add (cons, x), x)
                              in
                                  ((L'.SgiClass (x, n, k, c), loc) :: sgis, cons, vals, sgns, strs)
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
                        subSgn env' actual ran';
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
                (subSgn env sgn2 dom;
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

        val (ds, env') = dopen env' {str = basis_n, strs = [], sgn = sgn}

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
                                        raise Fail "Unresolved constraint in top.ur"))
                                | TypeClass (env, c, r, loc) =>
                                  let
                                      val c = normClassKey env c
                                  in
                                      case E.resolveClass env c of
                                          SOME e => r := SOME e
                                        | NONE => expError env (Unresolvable (loc, c))
                                  end) gs

        val () = subSgn env' topSgn' topSgn

        val (env', top_n) = E.pushStrNamed env' "Top" topSgn
        val () = top_r := top_n

        val (ds', env') = dopen env' {str = top_n, strs = [], sgn = topSgn}

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
                        fun default () = expError env (Unresolvable (loc, c))

                        val c = normClassKey env c
                    in
                        case E.resolveClass env c of
                            SOME e => r := SOME e
                          | NONE =>
                            case #1 (hnormCon env c) of
                                L'.CApp (f, x) =>
                                (case (#1 (hnormCon env f), #1 (hnormCon env x)) of
                                     (L'.CKApp (f, _), L'.CRecord (k, xcs)) =>
                                     (case #1 (hnormCon env f) of
                                          L'.CModProj (top_n', [], "folder") =>
                                          if top_n' = top_n then
                                              let
                                                  val e = (L'.EModProj (top_n, ["Folder"], "nil"), loc)
                                                  val e = (L'.EKApp (e, k), loc)

                                                  val (folder, _) = foldr (fn ((x, c), (folder, xcs)) =>
                                                                              let
                                                                                  val e = (L'.EModProj (top_n, ["Folder"],
                                                                                                        "cons"), loc)
                                                                                  val e = (L'.EKApp (e, k), loc)
                                                                                  val e = (L'.ECApp (e,
                                                                                                     (L'.CRecord (k, xcs),
                                                                                                      loc)), loc)
                                                                                  val e = (L'.ECApp (e, x), loc)
                                                                                  val e = (L'.ECApp (e, c), loc)
                                                                                  val e = (L'.EApp (e, folder), loc)
                                                                              in
                                                                                  (e, (x, c) :: xcs)
                                                                              end)
                                                                          (e, []) xcs
                                              in
                                                  r := SOME folder
                                              end
                                          else
                                              default ()
                                        | _ => default ())
                                   | _ => default ())
                              | _ => default ()
                    end) gs;

        (L'.DFfiStr ("Basis", basis_n, sgn), ErrorMsg.dummySpan)
        :: ds
        @ (L'.DStr ("Top", top_n, topSgn, topStr), ErrorMsg.dummySpan)
        :: ds' @ file
    end

end
