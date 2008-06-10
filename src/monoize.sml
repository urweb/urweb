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

structure Monoize :> MONOIZE = struct

structure E = ErrorMsg
structure Env = CoreEnv

structure L = Core
structure L' = Mono

val dummyTyp = (L'.TNamed 0, E.dummySpan)

fun monoName env (all as (c, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported name constructor";
             Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
             "")
    in
        case c of
            L.CName s => s
          | _ => poly ()
    end

fun monoType env (all as (c, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported type constructor";
             Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
             dummyTyp)
    in
        case c of
            L.TFun (c1, c2) => (L'.TFun (monoType env c1, monoType env c2), loc)
          | L.TCFun _ => poly ()
          | L.TRecord (L.CRecord ((L.KType, _), xcs), _) =>
            (L'.TRecord (map (fn (x, t) => (monoName env x, monoType env t)) xcs), loc)
          | L.TRecord _ => poly ()

          | L.CRel _ => poly ()
          | L.CNamed n => (L'.TNamed n, loc)
          | L.CApp _ => poly ()
          | L.CAbs _ => poly ()

          | L.CName _ => poly ()

          | L.CRecord _ => poly ()
          | L.CConcat _ => poly ()
    end

val dummyExp = (L'.EPrim (Prim.Int 0), E.dummySpan)

fun monoExp env (all as (e, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported expression";
             Print.eprefaces' [("Expression", CorePrint.p_exp env all)];
             dummyExp)
    in
        case e of
            L.EPrim p => (L'.EPrim p, loc)
          | L.ERel n => (L'.ERel n, loc)
          | L.ENamed n => (L'.ENamed n, loc)
          | L.EApp (e1, e2) => (L'.EApp (monoExp env e1, monoExp env e2), loc)
          | L.EAbs (x, t, e) =>
            (L'.EAbs (x, monoType env t, monoExp (Env.pushERel env x t) e), loc)
          | L.ECApp _ => poly ()
          | L.ECAbs _ => poly ()

          | L.ERecord xes => (L'.ERecord (map (fn (x, e) => (monoName env x, monoExp env e)) xes), loc)
          | L.EField (e, x, _) => (L'.EField (monoExp env e, monoName env x), loc)
    end

fun monoDecl env (all as (d, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported declaration";
             Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
             NONE)
    in
        case d of
            L.DCon _ => NONE
          | L.DVal (x, n, t, e) => SOME (Env.pushENamed env x n t (SOME e),
                                         (L'.DVal (x, n, monoType env t, monoExp env e), loc))
    end

fun monoize env ds =
    let
        val (_, ds) = List.foldl (fn (d, (env, ds)) =>
                                     case monoDecl env d of
                                         NONE => (env, ds)
                                       | SOME (env, d) => (env, d :: ds)) (env, []) ds
    in
        rev ds
    end

end
