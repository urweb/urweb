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

datatype kunify_error =
         KOccursCheckFailed of L'.kind * L'.kind
       | KIncompatible of L'.kind * L'.kind

exception KUnify' of kunify_error

fun kunifyError err =
    case err of
        KOccursCheckFailed (k1, k2) =>
        eprefaces "Kind occurs check failed"
        [("Kind 1", p_kind k1),
         ("Kind 2", p_kind k2)]
      | KIncompatible (k1, k2) =>
        eprefaces "Incompatible kinds"
        [("Kind 1", p_kind k1),
         ("Kind 2", p_kind k2)]

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

datatype con_error =
         UnboundCon of ErrorMsg.span * string
       | UnboundDatatype of ErrorMsg.span * string
       | UnboundStrInCon of ErrorMsg.span * string
       | WrongKind of L'.con * L'.kind * L'.kind * kunify_error
       | DuplicateField of ErrorMsg.span * string
       | ProjBounds of L'.con * int
       | ProjMismatch of L'.con * L'.kind

fun conError env err =
    case err of
        UnboundCon (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound constructor variable " ^ s)
      | UnboundDatatype (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound datatype " ^ s)
      | UnboundStrInCon (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure " ^ s)
      | WrongKind (c, k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 c) "Wrong kind";
         eprefaces' [("Constructor", p_con env c),
                     ("Have kind", p_kind k1),
                     ("Need kind", p_kind k2)];
         kunifyError kerr)
      | DuplicateField (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate record field " ^ s)
      | ProjBounds (c, n) =>
        (ErrorMsg.errorAt (#2 c) "Out of bounds constructor projection";
         eprefaces' [("Constructor", p_con env c),
                     ("Index", Print.PD.string (Int.toString n))])
      | ProjMismatch (c, k) =>
        (ErrorMsg.errorAt (#2 c) "Projection from non-tuple constructor";
         eprefaces' [("Constructor", p_con env c),
                     ("Kind", p_kind k)])

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
      | L.TDisjoint (c1, c2, c) =>
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

            ((L'.TDisjoint (c1', c2', c'), loc), k, gs1 @ gs2 @ gs3 @ gs4)
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

            ((L'.CDisjoint (c1', c2', c'), loc), k, gs1 @ gs2 @ gs3 @ gs4)
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

datatype cunify_error =
         CKind of L'.kind * L'.kind * kunify_error
       | COccursCheckFailed of L'.con * L'.con
       | CIncompatible of L'.con * L'.con
       | CExplicitness of L'.con * L'.con
       | CKindof of L'.kind * L'.con
       | CRecordFailure of PD.pp_desc * PD.pp_desc

exception CUnify' of cunify_error

fun cunifyError env err =
    case err of
        CKind (k1, k2, kerr) =>
        (eprefaces "Kind unification failure"
                   [("Kind 1", p_kind k1),
                    ("Kind 2", p_kind k2)];
         kunifyError kerr)
      | COccursCheckFailed (c1, c2) =>
        eprefaces "Constructor occurs check failed"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CIncompatible (c1, c2) =>
        eprefaces "Incompatible constructors"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CExplicitness (c1, c2) =>
        eprefaces "Differing constructor function explicitness"
                  [("Con 1", p_con env c1),
                   ("Con 2", p_con env c2)]
      | CKindof (k, c) =>
        eprefaces "Unexpected kind for kindof calculation"
                  [("Kind", p_kind k),
                   ("Con", p_con env c)]
      | CRecordFailure (s1, s2) =>
        eprefaces "Can't unify record constructors"
        [("Summary 1", s1),
         ("Summary 2", s2)]

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
      | L'.TDisjoint _ => ktype
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
           | k => raise CUnify' (CKindof (k, c)))
      | L'.CAbs (x, k, c) => (L'.KArrow (k, kindof (E.pushCRel env x k) c), loc)
      | L'.CDisjoint (_, _, c) => kindof env c

      | L'.CName _ => kname

      | L'.CRecord (k, _) => (L'.KRecord k, loc)
      | L'.CConcat (c, _) => kindof env c
      | L'.CFold (dom, ran) => foldKind (dom, ran, loc)

      | L'.CUnit => (L'.KUnit, loc)

      | L'.CTuple cs => (L'.KTuple (map (kindof env) cs), loc)
      | L'.CProj (c, n) =>
        (case hnormKind (kindof env c) of
             (L'.KTuple ks, _) => List.nth (ks, n - 1)
           | k => raise CUnify' (CKindof (k, c)))

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
          | L'.CDisjoint (_, _, c) =>
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

fun unifyRecordCons (env, denv) (c1, c2) =
    let
        fun rkindof c =
            case hnormKind (kindof env c) of
                (L'.KRecord k, _) => k
              | (L'.KError, _) => kerror
              | k => raise CUnify' (CKindof (k, c))

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

        val clear = case (fs1, others1, fs2, others2) of
                         ([], [], [], []) => true
                       | _ => false
        val empty = (L'.CRecord (k, []), dummy)
        fun pairOffUnifs (unifs1, unifs2) =
            case (unifs1, unifs2) of
                ([], _) =>
                if clear then
                    List.app (fn (_, r) => r := SOME empty) unifs2
                else
                    raise CUnify' (CRecordFailure (p_summary env s1, p_summary env s2))
              | (_, []) =>
                if clear then
                    List.app (fn (_, r) => r := SOME empty) unifs1
                else
                    raise CUnify' (CRecordFailure (p_summary env s1, p_summary env s2))
              | ((c1, _) :: rest1, (_, r2) :: rest2) =>
                (r2 := SOME c1;
                 pairOffUnifs (rest1, rest2))
    in
        pairOffUnifs (unifs1, unifs2)
    end

and unifyCons' (env, denv) c1 c2 =
    let
        val (c1, gs1) = hnormCon (env, denv) c1
        val (c2, gs2) = hnormCon (env, denv) c2
    in
        let
            val gs3 = unifyCons'' (env, denv) c1 c2
        in
            gs1 @ gs2 @ gs3
        end
        handle ex =>
               let
                   val loc = #2 c1

                   fun unfold (dom, f, i, r, c) =
                       let
                           val nm = cunif (loc, (L'.KName, loc))
                           val v = cunif (loc, dom)
                           val rest = cunif (loc, (L'.KRecord dom, loc))

                           val (iS, gs3) = summarizeCon (env, denv) i

                           val app = (L'.CApp (f, nm), loc)
                           val app = (L'.CApp (app, v), loc)
                           val app = (L'.CApp (app, rest), loc)
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
                                   gs1 @ gs2 @ gs3 @ gs4 @ gs5 @ gs6 @ gs7
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
                                  val gs7 = unifyCons (env, denv) r concat
                              in
                                  gs1 @ gs2 @ gs3 @ gs4 @ gs5 @ gs6 @ gs7
                              end
                           else
                             raise ex
                       end
                       handle _ => raise ex
               in
                   case (#1 c1, #1 c2) of
                       (L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold (dom, _), _), f), _), i), _), r), _) =>
                       unfold (dom, f, i, r, c2)
                     | (_, L'.CApp ((L'.CApp ((L'.CApp ((L'.CFold (dom, _), _), f), _), i), _), r)) =>
                       unfold (dom, f, i, r, c1)
                     | _ => raise ex
               end
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

datatype exp_error =
       UnboundExp of ErrorMsg.span * string
     | UnboundStrInExp of ErrorMsg.span * string
     | Unify of L'.exp * L'.con * L'.con * cunify_error
     | Unif of string * L'.con
     | WrongForm of string * L'.exp * L'.con
     | IncompatibleCons of L'.con * L'.con
     | DuplicatePatternVariable of ErrorMsg.span * string
     | PatUnify of L'.pat * L'.con * L'.con * cunify_error
     | UnboundConstructor of ErrorMsg.span * string list * string
     | PatHasArg of ErrorMsg.span
     | PatHasNoArg of ErrorMsg.span
     | Inexhaustive of ErrorMsg.span
     | DuplicatePatField of ErrorMsg.span * string
     | Unresolvable of ErrorMsg.span * L'.con
     | OutOfContext of ErrorMsg.span

fun expError env err =
    case err of
        UnboundExp (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound expression variable " ^ s)
      | UnboundStrInExp (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure " ^ s)
      | Unify (e, c1, c2, uerr) =>
        (ErrorMsg.errorAt (#2 e) "Unification failure";
         eprefaces' [("Expression", p_exp env e),
                     ("Have con", p_con env c1),
                     ("Need con", p_con env c2)];
         cunifyError env uerr)
      | Unif (action, c) =>
        (ErrorMsg.errorAt (#2 c) ("Unification variable blocks " ^ action);
         eprefaces' [("Con", p_con env c)])
      | WrongForm (variety, e, t) =>
        (ErrorMsg.errorAt (#2 e) ("Expression is not a " ^ variety);
         eprefaces' [("Expression", p_exp env e),
                     ("Type", p_con env t)])
      | IncompatibleCons (c1, c2) =>
        (ErrorMsg.errorAt (#2 c1) "Incompatible constructors";
         eprefaces' [("Con 1", p_con env c1),
                     ("Con 2", p_con env c2)])
      | DuplicatePatternVariable (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate pattern variable " ^ s)
      | PatUnify (p, c1, c2, uerr) =>
        (ErrorMsg.errorAt (#2 p) "Unification failure for pattern";
         eprefaces' [("Pattern", p_pat env p),
                     ("Have con", p_con env c1),
                     ("Need con", p_con env c2)];
         cunifyError env uerr)
      | UnboundConstructor (loc, ms, s) =>
        ErrorMsg.errorAt loc ("Unbound constructor " ^ String.concatWith "." (ms @ [s]) ^ " in pattern")
      | PatHasArg loc =>
        ErrorMsg.errorAt loc "Constructor expects no argument but is used with argument"
      | PatHasNoArg loc =>
        ErrorMsg.errorAt loc "Constructor expects argument but is used with no argument"
      | Inexhaustive loc =>
        ErrorMsg.errorAt loc "Inexhaustive 'case'"
      | DuplicatePatField (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate record field " ^ s ^ " in pattern")
      | OutOfContext loc =>
        ErrorMsg.errorAt loc "Type class wildcard occurs out of context"
      | Unresolvable (loc, c) =>
        (ErrorMsg.errorAt loc "Can't resolve type class instance";
         eprefaces' [("Class constraint", p_con env c)])
         
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
                                                                    (L'.CApp ((L'.CRel 3, loc),
                                                                              recCons (dom,
                                                                                       (L'.CRel 2, loc),
                                                                                       (L'.CRel 1, loc),
                                                                                       (L'.CRel 0, loc),
                                                                                       loc)), loc)), loc)),
                                                loc)), loc)), loc),
                         (L'.TFun ((L'.CApp ((L'.CRel 0, loc), (L'.CRecord (dom, []), loc)), loc),
                                   (L'.TCFun (L'.Explicit, "r", (L'.KRecord dom, loc),
                                              (L'.CApp ((L'.CRel 1, loc), (L'.CRel 0, loc)), loc)), loc)),
                          loc)), loc)), loc)

fun elabHead (env, denv) (e as (_, loc)) t =
    let
        fun unravel (t, e) =
            let
                val (t, gs) = hnormCon (env, denv) t
            in
                case t of
                    (L'.TCFun (L'.Implicit, x, k, t'), _) =>
                    let
                        val u = cunif (loc, k)

                        val (e, t, gs') = unravel (subConInCon (0, u) t',
                                                   (L'.ECApp (e, u), loc))
                    in
                        (e, t, gs @ gs')
                    end
                  | _ => (e, t, gs)
            end
    in
        unravel (t, e)
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

fun exhaustive (env, denv, t, ps) =
    let
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

        fun enumerateCases t =
            let
                fun dtype cons =
                    ListUtil.mapConcat (fn (_, n, to) =>
                                           case to of
                                               NONE => [Datatype (IM.insert (IM.empty, n, Wild))]
                                             | SOME t => map (fn c => Datatype (IM.insert (IM.empty, n, c)))
                                                         (enumerateCases t)) cons
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
                                         val this = enumerateCases t
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
            case (c1, c2) of
                (Wild, _) => true

              | (Datatype cmap1, Datatype cmap2) =>
                List.all (fn (n, c2) =>
                             case IM.find (cmap1, n) of
                                 NONE => false
                               | SOME c1 => coverageImp (c1, c2)) (IM.listItemsi cmap2)

              | (Record fmaps1, Record fmaps2) =>
                List.all (fn fmap2 =>
                             List.exists (fn fmap1 =>
                                             List.all (fn (x, c2) =>
                                                          case SM.find (fmap1, x) of
                                                              NONE => true
                                                            | SOME c1 => coverageImp (c1, c2))
                                                      (SM.listItemsi fmap2))
                                         fmaps1) fmaps2

              | _ => false

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
                      | _ => raise Fail "isTotal: Not a datatype"
                end
              | Record _ => (List.all (fn c2 => coverageImp (c, c2)) (enumerateCases t), [])
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
        
    in
        (*eprefaces "elabExp" [("eAll", SourcePrint.p_exp eAll)];*)

        case e of
            L.EAnnot (e, t) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (t', _, gs2) = elabCon (env, denv) t
                val gs3 = checkCon (env, denv) e' et t'
            in
                (e', t', gs1 @ gs2 @ gs3)
            end

          | L.EPrim p => ((L'.EPrim p, loc), primType env p, [])
          | L.EVar ([], s) =>
            (case E.lookupE env s of
                 E.NotBound =>
                 (expError env (UnboundExp (loc, s));
                  (eerror, cerror, []))
               | E.Rel (n, t) => ((L'.ERel n, loc), t, [])
               | E.Named (n, t) =>
                 if Char.isUpper (String.sub (s, 0)) then
                     elabHead (env, denv) (L'.ENamed n, loc) t
                 else
                     ((L'.ENamed n, loc), t, []))
          | L.EVar (m1 :: ms, s) =>
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
                     ((L'.EModProj (n, ms, s), loc), t, [])
                 end)

          | L.EApp (e1, (L.EWild, _)) =>
            let
                val (e1', t1, gs1) = elabExp (env, denv) e1
                val (e1', t1, gs2) = elabHead (env, denv) e1' t1
                val (t1, gs3) = hnormCon (env, denv) t1
            in
                case t1 of
                    (L'.TFun (dom, ran), _) =>
                    let
                        val (dom, gs4) = normClassConstraint (env, denv) dom
                    in
                        case E.resolveClass env dom of
                            NONE => (expError env (Unresolvable (loc, dom));
                                     (eerror, cerror, []))
                          | SOME pf => ((L'.EApp (e1', pf), loc), ran, gs1 @ gs2 @ gs3 @ gs4)
                    end
                  | _ => (expError env (OutOfContext loc);
                          (eerror, cerror, []))
            end
          | L.EWild => (expError env (OutOfContext loc);
                        (eerror, cerror, []))

          | L.EApp (e1, e2) =>
            let
                val (e1', t1, gs1) = elabExp (env, denv) e1
                val (e1', t1, gs2) = elabHead (env, denv) e1' t1
                val (e2', t2, gs3) = elabExp (env, denv) e2

                val dom = cunif (loc, ktype)
                val ran = cunif (loc, ktype)
                val t = (L'.TFun (dom, ran), dummy)

                val gs4 = checkCon (env, denv) e1' t1 t
                val gs5 = checkCon (env, denv) e2' t2 dom

                val gs = gs1 @ gs2 @ gs3 @ gs4 @ gs5
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
                val (e', et, gs2) = elabExp (E.pushERel env x t', denv) e
            in
                ((L'.EAbs (x, t', et, e'), loc),
                 (L'.TFun (t', et), loc),
                 gs1 @ gs2)
            end
          | L.ECApp (e, c) =>
            let
                val (e', et, gs1) = elabExp (env, denv) e
                val (e', et, gs2) = elabHead (env, denv) e' et
                val (c', ck, gs3) = elabCon (env, denv) c
                val ((et', _), gs4) = hnormCon (env, denv) et
            in
                case et' of
                    L'.CError => (eerror, cerror, [])
                  | L'.TCFun (_, _, k, eb) =>
                    let
                        val () = checkKind env c' ck k
                        val eb' = subConInCon (0, c') eb
                            handle SynUnif => (expError env (Unif ("substitution", eb));
                                               cerror)
                    in
                        ((L'.ECApp (e', c'), loc), eb', gs1 @ gs2 @ gs3 @ gs4)
                    end

                  | L'.CUnif _ =>
                    (expError env (Unif ("application", et));
                     (eerror, cerror, []))

                  | _ =>
                    (expError env (WrongForm ("constructor function", e', et));
                     (eerror, cerror, []))
            end
          | L.ECAbs (expl, x, k, e) =>
            let
                val expl' = elabExplicitness expl
                val k' = elabKind k
                val (e', et, gs) = elabExp (E.pushCRel env x k', D.enter denv) e
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

                (e', (L'.TDisjoint (c1', c2', t), loc), gs1 @ gs2 @ gs3 @ gs4)
            end

          | L.ERecord xes =>
            let
                val (xes', gs) = ListUtil.foldlMap (fn ((x, e), gs) =>
                                                       let
                                                           val (x', xk, gs1) = elabCon (env, denv) x
                                                           val (e', et, gs2) = elabExp (env, denv) e
                                                       in
                                                           checkKind env x' xk kname;
                                                           ((x', e', et), gs1 @ gs2 @ gs)
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
            in
                ((L'.ERecord xes', loc),
                 (L'.TRecord (L'.CRecord (ktype, map (fn (x', _, et) => (x', et)) xes'), loc), loc),
                 prove (xes', gs))
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
                ((L'.EField (e', c', {field = ft, rest = rest}), loc), ft, gs1 @ gs2 @ gs3 @ gs4)
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
                ((L'.ECut (e', c', {field = ft, rest = rest}), loc), (L'.TRecord rest, loc), gs1 @ gs2 @ gs3 @ gs4)
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
                                         ((p', e'), gs1 @ gs2 @ gs3 @ gs)
                                     end)
                                 gs1 pes

                val (total, gs') = exhaustive (env, denv, et, map #1 pes')
            in
                if total then
                    ()
                else
                    expError env (Inexhaustive loc);

                ((L'.ECase (e', pes', {disc = et, result = result}), loc), result, gs' @ gs)
            end
    end
            

datatype decl_error =
         KunifsRemain of L'.decl list
       | CunifsRemain of L'.decl list

fun lspan [] = ErrorMsg.dummySpan
  | lspan ((_, loc) :: _) = loc

fun declError env err =
    case err of
        KunifsRemain ds =>
        (ErrorMsg.errorAt (lspan ds) "Some kind unification variables are undetermined in declaration";
         eprefaces' [("Decl", p_list_sep PD.newline (p_decl env) ds)])
      | CunifsRemain ds =>
        (ErrorMsg.errorAt (lspan ds) "Some constructor unification variables are undetermined in declaration";
         eprefaces' [("Decl", p_list_sep PD.newline (p_decl env) ds)])

datatype sgn_error =
         UnboundSgn of ErrorMsg.span * string
       | UnmatchedSgi of L'.sgn_item
       | SgiWrongKind of L'.sgn_item * L'.kind * L'.sgn_item * L'.kind * kunify_error
       | SgiWrongCon of L'.sgn_item * L'.con * L'.sgn_item * L'.con * cunify_error
       | SgiMismatchedDatatypes of L'.sgn_item * L'.sgn_item * (L'.con * L'.con * cunify_error) option
       | SgnWrongForm of L'.sgn * L'.sgn
       | UnWhereable of L'.sgn * string
       | WhereWrongKind of L'.kind * L'.kind * kunify_error
       | NotIncludable of L'.sgn
       | DuplicateCon of ErrorMsg.span * string
       | DuplicateVal of ErrorMsg.span * string
       | DuplicateSgn of ErrorMsg.span * string
       | DuplicateStr of ErrorMsg.span * string
       | NotConstraintsable of L'.sgn

fun sgnError env err =
    case err of
        UnboundSgn (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound signature variable " ^ s)
      | UnmatchedSgi (sgi as (_, loc)) =>
        (ErrorMsg.errorAt loc "Unmatched signature item";
         eprefaces' [("Item", p_sgn_item env sgi)])
      | SgiWrongKind (sgi1, k1, sgi2, k2, kerr) =>
        (ErrorMsg.errorAt (#2 sgi1) "Kind unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Kind 1", p_kind k1),
                     ("Kind 2", p_kind k2)];
         kunifyError kerr)
      | SgiWrongCon (sgi1, c1, sgi2, c2, cerr) =>
        (ErrorMsg.errorAt (#2 sgi1) "Constructor unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Con 1", p_con env c1),
                     ("Con 2", p_con env c2)];
         cunifyError env cerr)
      | SgiMismatchedDatatypes (sgi1, sgi2, cerro) =>
        (ErrorMsg.errorAt (#2 sgi1) "Mismatched 'datatype' specifications:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2)];
         Option.app (fn (c1, c2, ue) =>
                        (eprefaces "Unification error"
                                   [("Con 1", p_con env c1),
                                    ("Con 2", p_con env c2)];
                         cunifyError env ue)) cerro)
      | SgnWrongForm (sgn1, sgn2) =>
        (ErrorMsg.errorAt (#2 sgn1) "Incompatible signatures:";
         eprefaces' [("Sig 1", p_sgn env sgn1),
                     ("Sig 2", p_sgn env sgn2)])
      | UnWhereable (sgn, x) =>
        (ErrorMsg.errorAt (#2 sgn) "Unavailable field for 'where'";
         eprefaces' [("Signature", p_sgn env sgn),
                     ("Field", PD.string x)])
      | WhereWrongKind (k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 k1) "Wrong kind for 'where'";
         eprefaces' [("Have", p_kind k1),
                     ("Need", p_kind k2)];
         kunifyError kerr)
      | NotIncludable sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Invalid signature to 'include'";
         eprefaces' [("Signature", p_sgn env sgn)])
      | DuplicateCon (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate constructor " ^ s ^ " in signature")
      | DuplicateVal (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate value " ^ s ^ " in signature")
      | DuplicateSgn (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate signature " ^ s ^ " in signature")
      | DuplicateStr (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate structure " ^ s ^ " in signature")
      | NotConstraintsable sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Invalid signature for 'open constraints'";
         eprefaces' [("Signature", p_sgn env sgn)])

datatype str_error =
         UnboundStr of ErrorMsg.span * string
       | NotFunctor of L'.sgn
       | FunctorRebind of ErrorMsg.span
       | UnOpenable of L'.sgn
       | NotType of L'.kind * (L'.kind * L'.kind * kunify_error)
       | DuplicateConstructor of string * ErrorMsg.span
       | NotDatatype of ErrorMsg.span

fun strError env err =
    case err of
        UnboundStr (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound structure variable " ^ s)
      | NotFunctor sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Application of non-functor";
         eprefaces' [("Signature", p_sgn env sgn)])
      | FunctorRebind loc =>
        ErrorMsg.errorAt loc "Attempt to rebind functor"
      | UnOpenable sgn =>
        (ErrorMsg.errorAt (#2 sgn) "Un-openable structure";
         eprefaces' [("Signature", p_sgn env sgn)])
      | NotType (k, (k1, k2, ue)) =>
        (ErrorMsg.errorAt (#2 k) "'val' type kind is not 'Type'";
         eprefaces' [("Kind", p_kind k),
                     ("Subkind 1", p_kind k1),
                     ("Subkind 2", p_kind k2)];
         kunifyError ue)
      | DuplicateConstructor (x, loc) =>
        ErrorMsg.errorAt loc ("Duplicate datatype constructor " ^ x)
      | NotDatatype loc =>
        ErrorMsg.errorAt loc "Trying to import non-datatype as a datatype"

val hnormSgn = E.hnormSgn

fun tableOf () = (L'.CModProj (!basis_r, [], "sql_table"), ErrorMsg.dummySpan)

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

            val (xcs, (used, env, gs)) =
                ListUtil.foldlMap
                (fn ((x, to), (used, env, gs)) =>
                    let
                        val (to, t, gs') = case to of
                                           NONE => (NONE, t, gs)
                                         | SOME t' =>
                                           let
                                               val (t', tk, gs') = elabCon (env, denv) t'
                                           in
                                               checkKind env t' tk k;
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
        in
            (unifyKinds ck ktype
             handle KUnify ue => strError env (NotType (ck, ue)));

            ([(L'.SgiVal (x, n, c'), loc)], (env', denv, gs' @ gs))
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
      | L'.DClass (x, n, c) => [(L'.SgiClass (x, n, c), loc)]

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
            fun folder (sgi2All as (sgi, loc), (env, denv)) =
                let
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
                                                 fun good () = SOME (E.pushCNamedAs env x n2 k2 (SOME c2), denv)
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
                                                                   E.pushCNamedAs env x n1 (L'.KType, loc)
                                                                                  (SOME (L'.CNamed n1, loc))
                                                 in
                                                     SOME (env, denv)
                                                 end

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
                                         (case unifyCons (env, denv) c1 c2 of
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
                                                 fun good () = SOME (E.pushCNamedAs env x n2 k (SOME c2), denv)
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


fun elabDecl ((d, loc), (env, denv, gs)) =
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

            ([(L'.DCon (x, n, k', c'), loc)], (env', denv, gs' @ gs))
        end
      | L.DDatatype (x, xs, xcs) =>
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
        in
            ([(L'.DDatatype (x, n, xs, xcs), loc)], (env, denv, gs))
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
            val gs3 = checkCon (env, denv) e' et c'
            val (c', gs4) = normClassConstraint (env, denv) c'
            val (env', n) = E.pushENamed env x c'
        in
            (*prefaces "DVal" [("x", Print.PD.string x),
                             ("c'", p_con env c')];*)
            ([(L'.DVal (x, n, c', e'), loc)], (env', denv, gs1 @ gs2 @ gs3 @ gs4 @ gs))
        end
      | L.DValRec vis =>
        let
            val (vis, gs) = ListUtil.foldlMap
                                (fn ((x, co, e), gs) =>
                                    let
                                        val (c', _, gs1) = case co of
                                                               NONE => (cunif (loc, ktype), ktype, [])
                                                             | SOME c => elabCon (env, denv) c
                                    in
                                        ((x, c', e), gs1 @ gs)
                                    end) [] vis

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
                                                  in
                                                      ((x, n, c', e'), gs1 @ gs2 @ gs)
                                                  end) gs vis
        in
            ([(L'.DValRec vis, loc)], (env, denv, gs))
        end

      | L.DSgn (x, sgn) =>
        let
            val (sgn', gs') = elabSgn (env, denv) sgn
            val (env', n) = E.pushSgnNamed env x sgn'
        in
            ([(L'.DSgn (x, n, sgn'), loc)], (env', denv, gs' @ gs))
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
                        val (str', actual, ds) = elabStr (env, denv) str
                    in
                        (str', selfifyAt env {str = str', sgn = actual}, ds)
                    end
                  | SOME (formal, gs1) =>
                    let
                        val str =
                            case #1 (hnormSgn env formal) of
                                L'.SgnConst sgis =>
                                (case #1 str of
                                     L.StrConst ds =>
                                     let
                                         val needed = foldl (fn ((sgi, _), needed) =>
                                                                case sgi of
                                                                    L'.SgiConAbs (x, _, _) => SS.add (needed, x)
                                                                  | _ => needed)
                                                            SS.empty sgis
                                                      
                                         val needed = foldl (fn ((d, _), needed) =>
                                                                case d of
                                                                    L.DCon (x, _, _) => (SS.delete (needed, x)
                                                                                         handle NotFound => needed)
                                                                  | L.DOpen _ => SS.empty
                                                                  | _ => needed)
                                                            needed ds
                                     in
                                         case SS.listItems needed of
                                             [] => str
                                           | xs =>
                                             let
                                                 val kwild = (L.KWild, #2 str)
                                                 val cwild = (L.CWild kwild, #2 str)
                                                 val ds' = map (fn x => (L.DCon (x, NONE, cwild), #2 str)) xs
                                             in
                                                 (L.StrConst (ds @ ds'), #2 str)
                                             end
                                     end
                                   | _ => str)
                              | _ => str

                        val (str', actual, gs2) = elabStr (env, denv) str
                    in
                        subSgn (env, denv) (selfifyAt env {str = str', sgn = actual}) formal;
                        (str', formal, gs1 @ gs2)
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
            ([(L'.DFfiStr (x, n, sgn'), loc)], (env', denv, gs' @ gs))
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

            ([(L'.DConstraint (c1', c2'), loc)], (env, denv', gs1 @ gs2 @ gs3 @ gs4 @ gs))
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
                                 (case hnormCon (env, denv) t of
                                      ((L'.TFun (dom, ran), _), []) =>
                                      (case (hnormCon (env, denv) dom, hnormCon (env, denv) ran) of
                                           (((L'.TRecord domR, _), []),
                                            ((L'.CApp (tf, arg3), _), [])) =>
                                           (case (hnormCon (env, denv) tf, hnormCon (env, denv) arg3) of
                                                (((L'.CApp (tf, arg2), _), []),
                                                 (((L'.CRecord (_, []), _), []))) =>
                                                (case (hnormCon (env, denv) tf) of
                                                     ((L'.CApp (tf, arg1), _), []) =>
                                                     (case (hnormCon (env, denv) tf,
                                                            hnormCon (env, denv) domR,
                                                            hnormCon (env, denv) arg1,
                                                            hnormCon (env, denv) arg2) of
                                                          ((tf, []), (domR, []), (arg1, []),
                                                           ((L'.CRecord (_, []), _), [])) =>
                                                          let
                                                              val t = (L'.CApp (tf, arg1), loc)
                                                              val t = (L'.CApp (t, arg2), loc)
                                                              val t = (L'.CApp (t, arg3), loc)
                                                          in
                                                              (L'.SgiVal (x, n, (L'.TFun ((L'.TRecord domR, loc),
                                                                                          t),
                                                                                 loc)), loc)
                                                          end
                                                        | _ => all)
                                                   | _ => all)
                                              | _ => all)
                                         | _ => all)
                                    | _ => all)
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
            ([(L'.DTable (!basis_r, x, n, c'), loc)], (env, denv, gs' @ gs))
        end

      | L.DClass (x, c) =>
        let
            val k = (L'.KArrow ((L'.KType, loc), (L'.KType, loc)), loc)
            val (c', ck, gs) = elabCon (env, denv) c
            val (env, n) = E.pushCNamed env x k (SOME c')
            val env = E.pushClass env n
        in
            checkKind env c' ck k;
            ([(L'.DClass (x, n, c'), loc)], (env, denv, []))
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
             gs1 @ gs2 @ gs3)
        end
      | L.StrApp (str1, str2) =>
        let
            val (str1', sgn1, gs1) = elabStr (env, denv) str1
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

fun elabFile basis env file =
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
            app (fn (loc, env, denv, c1, c2) =>
                    case D.prove env denv (c1, c2, loc) of
                        [] => ()
                      | _ =>
                        (ErrorMsg.errorAt loc "Couldn't prove field name disjointness";
                         eprefaces' [("Con 1", p_con env c1),
                                     ("Con 2", p_con env c2),
                                     ("Hnormed 1", p_con env (ElabOps.hnormCon env c1)),
                                     ("Hnormed 2", p_con env (ElabOps.hnormCon env c2))])) gs;

        (L'.DFfiStr ("Basis", basis_n, sgn), ErrorMsg.dummySpan) :: ds @ file
    end

end
