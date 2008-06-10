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
structure L = Elab
structure L' = Core

fun corifyKind (k, loc) =
    case k of
        L.KType => (L'.KType, loc)
      | L.KArrow (k1, k2) => (L'.KArrow (corifyKind k1, corifyKind k2), loc)
      | L.KName => (L'.KName, loc)
      | L.KRecord k => (L'.KRecord (corifyKind k), loc)

      | L.KError => raise Fail ("corifyKind: KError at " ^ EM.spanToString loc)
      | L.KUnif (_, ref (SOME k)) => corifyKind k
      | L.KUnif _ => raise Fail ("corifyKind: KUnif at " ^ EM.spanToString loc)

fun corifyCon (c, loc) =
    case c of
        L.TFun (t1, t2) => (L'.TFun (corifyCon t1, corifyCon t2), loc)
      | L.TCFun (_, x, k, t) => (L'.TCFun (x, corifyKind k, corifyCon t), loc)
      | L.TRecord c => (L'.TRecord (corifyCon c), loc)

      | L.CRel n => (L'.CRel n, loc)
      | L.CNamed n => (L'.CNamed n, loc)
      | L.CApp (c1, c2) => (L'.CApp (corifyCon c1, corifyCon c2), loc)
      | L.CAbs (x, k, c) => (L'.CAbs (x, corifyKind k, corifyCon c), loc)

      | L.CName s => (L'.CName s, loc)

      | L.CRecord (k, xcs) => (L'.CRecord (corifyKind k, map (fn (c1, c2) => (corifyCon c1, corifyCon c2)) xcs), loc)
      | L.CConcat (c1, c2) => (L'.CConcat (corifyCon c1, corifyCon c2), loc)

      | L.CError => raise Fail ("corifyCon: CError at " ^ EM.spanToString loc)
      | L.CUnif (_, _, ref (SOME c)) => corifyCon c
      | L.CUnif _ => raise Fail ("corifyCon: CUnif at " ^ EM.spanToString loc)

fun corifyExp (e, loc) =
    case e of
        L.EPrim p => (L'.EPrim p, loc)
      | L.ERel n => (L'.ERel n, loc)
      | L.ENamed n => (L'.ENamed n, loc)
      | L.EApp (e1, e2) => (L'.EApp (corifyExp e1, corifyExp e2), loc)
      | L.EAbs (x, dom, ran, e1) => (L'.EAbs (x, corifyCon dom, corifyCon ran, corifyExp e1), loc)
      | L.ECApp (e1, c) => (L'.ECApp (corifyExp e1, corifyCon c), loc)
      | L.ECAbs (_, x, k, e1) => (L'.ECAbs (x, corifyKind k, corifyExp e1), loc)

      | L.ERecord xes => (L'.ERecord (map (fn (c, e) => (corifyCon c, corifyExp e)) xes), loc)
      | L.EField (e1, c, {field, rest}) => (L'.EField (corifyExp e1, corifyCon c,
                                                       {field = corifyCon field, rest = corifyCon rest}), loc)

      | L.EError => raise Fail ("corifyExp: EError at " ^ EM.spanToString loc)

fun corifyDecl (d, loc : EM.span) =
    case d of
        L.DCon (x, n, k, c) => (L'.DCon (x, n, corifyKind k, corifyCon c), loc)
      | L.DVal (x, n, t, e) => (L'.DVal (x, n, corifyCon t, corifyExp e), loc)

val corify = map corifyDecl

end
