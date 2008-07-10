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

(* Pretty-printing monomorphic Laconic/Web *)

structure MonoPrint :> MONO_PRINT = struct

open Print.PD
open Print

open Mono

structure E = MonoEnv

val debug = ref false

fun p_typ' par env (t, _) =
    case t of
        TFun (t1, t2) => parenIf par (box [p_typ' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_typ env t2])
      | TRecord xcs => box [string "{",
                            p_list (fn (x, t) =>
                                       box [string x,
                                            space,
                                            string ":",
                                            space,
                                            p_typ env t]) xcs,
                            string "}"]
      | TNamed n =>
        if !debug then
            string (#1 (E.lookupTNamed env n) ^ "__" ^ Int.toString n)
        else
            string (#1 (E.lookupTNamed env n))
      | TFfi (m, x) => box [string "FFI(", string m, string ".", string x, string ")"]

and p_typ env = p_typ' false env

fun p_exp' par env (e, _) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n =>
        if !debug then
            string (#1 (E.lookupERel env n) ^ "_" ^ Int.toString n)
        else
            string (#1 (E.lookupERel env n))
      | ENamed n =>
        if !debug then
            string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
        else
            string (#1 (E.lookupENamed env n))
      | EFfi (m, x) => box [string "FFI(", string m, string ".", string x, string ")"]
      | EFfiApp (m, x, es) => box [string "FFI(",
                                   string m,
                                   string ".",
                                   string x,
                                   string "(",
                                   p_list (p_exp env) es,
                                   string "))"]
      | EApp (e1, e2) => parenIf par (box [p_exp env e1,
                                           space,
                                           p_exp' true env e2])
      | EAbs (x, t, _, e) => parenIf par (box [string "fn",
                                               space,
                                               string x,
                                               space,
                                               string ":",
                                               space,
                                               p_typ env t,
                                               space,
                                               string "=>",
                                               space,
                                               p_exp (E.pushERel env x t) e])

      | ERecord xes => box [string "{",
                            p_list (fn (x, e, _) =>
                                       box [string x,
                                            space,
                                            string "=",
                                            space,
                                            p_exp env e]) xes,
                            string "}"]
      | EField (e, x) =>
        box [p_exp' true env e,
             string ".",
             string x]


      | EStrcat (e1, e2) => box [p_exp' true env e1,
                                 space,
                                 string ".",
                                 space,
                                 p_exp' true env e2]

and p_exp env = p_exp' false env

fun p_decl env ((d, _) : decl) =
    case d of
        DVal (x, n, t, e) =>
        let
            val xp = if !debug then
                         box [string x,
                              string "__",
                              string (Int.toString n)]
                     else
                         string x        
        in
            box [string "val",
                 space,
                 xp,
                 space,
                 string ":",
                 space,
                 p_typ env t,
                 space,
                 string "=",
                 space,
                 p_exp env e]
        end
      | DPage (xcs, e) => box [string "page",
                               string "[",
                               p_list (fn (x, t) =>
                                          box [string x,
                                               space,
                                               string ":",
                                               space,
                                               p_typ env t]) xcs,
                               string "]",
                               space,
                               string "=",
                               space,
                               p_exp env e]
                          
fun p_file env file =
    let
        val (pds, _) = ListUtil.foldlMap (fn (d, env) =>
                                             (p_decl env d,
                                              E.declBinds env d))
                             env file
    in
        p_list_sep newline (fn x => x) pds
    end

end
