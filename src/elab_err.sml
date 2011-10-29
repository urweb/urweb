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

structure ElabErr :> ELAB_ERR = struct

structure L = Source
open Elab

structure E = ElabEnv
structure U = ElabUtil

open Print
structure P = ElabPrint

val simplCon = U.Con.mapB {kind = fn _ => fn k => k,
                           con = fn env => fn c =>
                                              let
                                                  val c = (c, ErrorMsg.dummySpan)
                                                  val c' = ElabOps.hnormCon env c
                                              in
                                                  (*prefaces "simpl" [("c", P.p_con env c),
                                                                    ("c'", P.p_con env c')];*)
                                                  #1 c'
                                              end,
                           bind = fn (env, U.Con.RelC (x, k)) => E.pushCRel env x k
                                   | (env, U.Con.NamedC (x, n, k, co)) => E.pushCNamedAs env x n k co
                                   | (env, _) => env}

val p_kind = P.p_kind
             
datatype kind_error =
         UnboundKind of ErrorMsg.span * string

fun kindError env err =
    case err of
        UnboundKind (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound kind variable " ^ s)

datatype kunify_error =
         KOccursCheckFailed of kind * kind
       | KIncompatible of kind * kind

fun kunifyError env err =
    case err of
        KOccursCheckFailed (k1, k2) =>
        eprefaces "Kind occurs check failed"
        [("Kind 1", p_kind env k1),
         ("Kind 2", p_kind env k2)]
      | KIncompatible (k1, k2) =>
        eprefaces "Incompatible kinds"
        [("Kind 1", p_kind env k1),
         ("Kind 2", p_kind env k2)]


fun p_con env c = P.p_con env (simplCon env c)

datatype con_error =
         UnboundCon of ErrorMsg.span * string
       | UnboundDatatype of ErrorMsg.span * string
       | UnboundStrInCon of ErrorMsg.span * string
       | WrongKind of con * kind * kind * kunify_error
       | DuplicateField of ErrorMsg.span * string
       | ProjBounds of con * int
       | ProjMismatch of con * kind

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
                     ("Have kind", p_kind env k1),
                     ("Need kind", p_kind env k2)];
         kunifyError env kerr)
      | DuplicateField (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate record field " ^ s)
      | ProjBounds (c, n) =>
        (ErrorMsg.errorAt (#2 c) "Out of bounds constructor projection";
         eprefaces' [("Constructor", p_con env c),
                     ("Index", Print.PD.string (Int.toString n))])
      | ProjMismatch (c, k) =>
        (ErrorMsg.errorAt (#2 c) "Projection from non-tuple constructor";
         eprefaces' [("Constructor", p_con env c),
                     ("Kind", p_kind env k)])

datatype cunify_error =
         CKind of kind * kind * kunify_error
       | COccursCheckFailed of con * con
       | CIncompatible of con * con
       | CExplicitness of con * con
       | CKindof of kind * con * string
       | CRecordFailure of con * con * (con * con * con * cunify_error option) option
       | TooLifty of ErrorMsg.span * ErrorMsg.span
       | TooUnify of con * con
       | TooDeep

fun cunifyError env err =
    case err of
        CKind (k1, k2, kerr) =>
        (eprefaces "Kind unification failure"
                   [("Have", p_kind env k1),
                    ("Need", p_kind env k2)];
         kunifyError env kerr)
      | COccursCheckFailed (c1, c2) =>
        eprefaces "Constructor occurs check failed"
                  [("Have", p_con env c1),
                   ("Need", p_con env c2)]
      | CIncompatible (c1, c2) =>
        eprefaces "Incompatible constructors"
                  [("Have", p_con env c1),
                   ("Need", p_con env c2)]
      | CExplicitness (c1, c2) =>
        eprefaces "Differing constructor function explicitness"
                  [("Have", p_con env c1),
                   ("Need", p_con env c2)]
      | CKindof (k, c, expected) =>
        eprefaces ("Unexpected kind for kindof calculation (expecting " ^ expected ^ ")")
                  [("Kind", p_kind env k),
                   ("Con", p_con env c)]
      | CRecordFailure (c1, c2, fo) =>
        (eprefaces "Can't unify record constructors"
                   (("Have", p_con env c1)
                    :: ("Need", p_con env c2)
                    :: (case fo of
                            NONE => []
                          | SOME (nm, t1, t2, _) =>
                            [("Field", p_con env nm),
                             ("Value 1", p_con env t1),
                             ("Value 2", p_con env t2)]));
         case fo of
             SOME (_, _, _, SOME err') => cunifyError env err'
           | _ => ())
      | TooLifty (loc1, loc2) =>
        (ErrorMsg.errorAt loc1 "Can't unify two unification variables that both have suspended liftings";
         eprefaces' [("Other location", Print.PD.string (ErrorMsg.spanToString loc2))])
      | TooUnify (c1, c2) =>
        (ErrorMsg.errorAt (#2 c1) "Substitution in constructor is blocked by a too-deep unification variable";
         eprefaces' [("Replacement", p_con env c1),
                     ("Body", p_con env c2)])
      | TooDeep => ErrorMsg.error "Can't reverse-engineer unification variable lifting"

datatype exp_error =
       UnboundExp of ErrorMsg.span * string
     | UnboundStrInExp of ErrorMsg.span * string
     | Unify of exp * con * con * cunify_error
     | Unif of string * ErrorMsg.span * con
     | WrongForm of string * exp * con
     | IncompatibleCons of con * con
     | DuplicatePatternVariable of ErrorMsg.span * string
     | PatUnify of pat * con * con * cunify_error
     | UnboundConstructor of ErrorMsg.span * string list * string
     | PatHasArg of ErrorMsg.span
     | PatHasNoArg of ErrorMsg.span
     | Inexhaustive of ErrorMsg.span * pat
     | DuplicatePatField of ErrorMsg.span * string
     | Unresolvable of ErrorMsg.span * con
     | OutOfContext of ErrorMsg.span * (exp * con) option
     | IllegalRec of string * exp

val p_exp = P.p_exp
val p_pat = P.p_pat

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
      | Unif (action, loc, c) =>
        (ErrorMsg.errorAt loc ("Unification variable blocks " ^ action);
         eprefaces' [("Con", p_con env c)])
      | WrongForm (variety, e, t) =>
        (ErrorMsg.errorAt (#2 e) ("Expression is not a " ^ variety);
         eprefaces' [("Expression", p_exp env e),
                     ("Type", p_con env t)])
      | IncompatibleCons (c1, c2) =>
        (ErrorMsg.errorAt (#2 c1) "Incompatible constructors";
         eprefaces' [("Have", p_con env c1),
                     ("Need", p_con env c2)])
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
      | Inexhaustive (loc, p) =>
        (ErrorMsg.errorAt loc "Inexhaustive 'case'";
         eprefaces' [("Missed case", p_pat env p)])
      | DuplicatePatField (loc, s) =>
        ErrorMsg.errorAt loc ("Duplicate record field " ^ s ^ " in pattern")
      | OutOfContext (loc, co) =>
        (ErrorMsg.errorAt loc "Type class wildcard occurs out of context";
         Option.app (fn (e, c) => eprefaces' [("Function", p_exp env e),
                                              ("Type", p_con env c)]) co)
      | Unresolvable (loc, c) =>
        (ErrorMsg.errorAt loc "Can't resolve type class instance";
         eprefaces' [("Class constraint", p_con env c)(*,
                     ("Class database", p_list (fn (c, rules) =>
                                                   box [P.p_con env c,
                                                        PD.string ":",
                                                        space,
                                                        p_list (fn (c, e) =>
                                                                   box [p_exp env e,
                                                                        PD.string ":",
                                                                        space,
                                                                        P.p_con env c]) rules])
                                        (E.listClasses env))*)])
      | IllegalRec (x, e) =>
        (ErrorMsg.errorAt (#2 e) "Illegal 'val rec' righthand side (must be a function abstraction)";
         eprefaces' [("Variable", PD.string x),
                     ("Expression", p_exp env e)])


datatype decl_error =
         KunifsRemain of decl list
       | CunifsRemain of decl list
       | Nonpositive of decl

fun lspan [] = ErrorMsg.dummySpan
  | lspan ((_, loc) :: _) = loc

val baseLen = 2000

fun p_decl env d =
    let
        val fname = OS.FileSys.tmpName ()
        val out' = TextIO.openOut fname
        val out = Print.openOut {dst = out', wid = 80}

        fun readFromFile () =
            let
                val inf = TextIO.openIn fname

                fun loop acc =
                    case TextIO.inputLine inf of
                        NONE => String.concat (rev acc)
                      | SOME line => loop (line :: acc)
            in
                loop []
                before TextIO.closeIn inf
            end
    in
        Print.fprint out (P.p_decl env d);
        TextIO.closeOut out';
        let
            val content = readFromFile ()
        in
            OS.FileSys.remove fname;
            Print.PD.string (if size content <= baseLen then
                                 content
                             else
                                 let
                                     val (befor, after) = Substring.position "<UNIF:" (Substring.full content)
                                 in
                                     if Substring.isEmpty after then
                                         raise Fail "No unification variables in rendering"
                                     else
                                         Substring.concat [Substring.full "\n.....\n",
                                                           if Substring.size befor <= baseLen then
                                                               befor
                                                           else
                                                               Substring.slice (befor, Substring.size befor - baseLen, SOME baseLen),
                                                           if Substring.size after <= baseLen then
                                                               after
                                                           else
                                                               Substring.slice (after, 0, SOME baseLen),
                                                           Substring.full "\n.....\n"]
                                 end)
        end
    end

fun declError env err =
    case err of
        KunifsRemain ds =>
        (ErrorMsg.errorAt (lspan ds) "Some kind unification variables are undetermined in declaration\n(look for them as \"<UNIF:...>\")";
         eprefaces' [("Decl", p_list_sep PD.newline (p_decl env) ds)])
      | CunifsRemain ds =>
        (ErrorMsg.errorAt (lspan ds) "Some constructor unification variables are undetermined in declaration\n(look for them as \"<UNIF:...>\")";
         eprefaces' [("Decl", p_list_sep PD.newline (p_decl env) ds)])
      | Nonpositive d =>
        (ErrorMsg.errorAt (#2 d) "Non-strictly-positive datatype declaration (could allow non-termination)";
         eprefaces' [("Decl", p_decl env d)])

datatype sgn_error =
         UnboundSgn of ErrorMsg.span * string
       | UnmatchedSgi of ErrorMsg.span * sgn_item
       | SgiWrongKind of ErrorMsg.span * sgn_item * kind * sgn_item * kind * kunify_error
       | SgiWrongCon of ErrorMsg.span * sgn_item * con * sgn_item * con * cunify_error
       | SgiMismatchedDatatypes of ErrorMsg.span * sgn_item * sgn_item
                                   * (con * con * cunify_error) option
       | SgnWrongForm of ErrorMsg.span * sgn * sgn
       | UnWhereable of sgn * string
       | WhereWrongKind of kind * kind * kunify_error
       | NotIncludable of sgn
       | DuplicateCon of ErrorMsg.span * string
       | DuplicateVal of ErrorMsg.span * string
       | DuplicateSgn of ErrorMsg.span * string
       | DuplicateStr of ErrorMsg.span * string
       | NotConstraintsable of sgn

val p_sgn_item = P.p_sgn_item
val p_sgn = P.p_sgn

fun sgnError env err =
    case err of
        UnboundSgn (loc, s) =>
        ErrorMsg.errorAt loc ("Unbound signature variable " ^ s)
      | UnmatchedSgi (loc, sgi) =>
        (ErrorMsg.errorAt loc "Unmatched signature item";
         eprefaces' [("Item", p_sgn_item env sgi)])
      | SgiWrongKind (loc, sgi1, k1, sgi2, k2, kerr) =>
        (ErrorMsg.errorAt loc "Kind unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Kind 1", p_kind env k1),
                     ("Kind 2", p_kind env k2)];
         kunifyError env kerr)
      | SgiWrongCon (loc, sgi1, c1, sgi2, c2, cerr) =>
        (ErrorMsg.errorAt loc "Constructor unification failure in signature matching:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2),
                     ("Con 1", p_con env c1),
                     ("Con 2", p_con env c2)];
         cunifyError env cerr)
      | SgiMismatchedDatatypes (loc, sgi1, sgi2, cerro) =>
        (ErrorMsg.errorAt loc "Mismatched 'datatype' specifications:";
         eprefaces' [("Have", p_sgn_item env sgi1),
                     ("Need", p_sgn_item env sgi2)];
         Option.app (fn (c1, c2, ue) =>
                        (eprefaces "Unification error"
                                   [("Con 1", p_con env c1),
                                    ("Con 2", p_con env c2)];
                         cunifyError env ue)) cerro)
      | SgnWrongForm (loc, sgn1, sgn2) =>
        (ErrorMsg.errorAt loc "Incompatible signatures:";
         eprefaces' [("Sig 1", p_sgn env sgn1),
                     ("Sig 2", p_sgn env sgn2)])
      | UnWhereable (sgn, x) =>
        (ErrorMsg.errorAt (#2 sgn) "Unavailable field for 'where'";
         eprefaces' [("Signature", p_sgn env sgn),
                     ("Field", PD.string x)])
      | WhereWrongKind (k1, k2, kerr) =>
        (ErrorMsg.errorAt (#2 k1) "Wrong kind for 'where'";
         eprefaces' [("Have", p_kind env k1),
                     ("Need", p_kind env k2)];
         kunifyError env kerr)
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
       | NotFunctor of sgn
       | FunctorRebind of ErrorMsg.span
       | UnOpenable of sgn
       | NotType of ErrorMsg.span * kind * (kind * kind * kunify_error)
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
      | NotType (loc, k, (k1, k2, ue)) =>
        (ErrorMsg.errorAt loc "'val' type kind is not 'Type'";
         eprefaces' [("Kind", p_kind env k),
                     ("Subkind 1", p_kind env k1),
                     ("Subkind 2", p_kind env k2)];
         kunifyError env ue)
      | DuplicateConstructor (x, loc) =>
        ErrorMsg.errorAt loc ("Duplicate datatype constructor " ^ x)
      | NotDatatype loc =>
        ErrorMsg.errorAt loc "Trying to import non-datatype as a datatype"

end
