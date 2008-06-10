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

(* Pretty-printing flat-code Laconic/Web *)

structure FlatPrint :> FLAT_PRINT = struct

open Print.PD
open Print

open Flat

structure E = FlatEnv

val debug = ref false

val dummyTyp = (TNamed 0, ErrorMsg.dummySpan)

fun p_typ' par env (t, _) =
    case t of
        TFun (t1, t2) => parenIf par (box [p_typ' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_typ env t2])
      | TCode (t1, t2) => parenIf par (box [p_typ' true env t1,
                                            space,
                                            string "-->",
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

and p_typ env = p_typ' false env

fun p_exp' par env (e, _) =
    case e of
        EPrim p => Prim.p_t p
      | ERel n =>
        ((if !debug then
              string (#1 (E.lookupERel env n) ^ "_" ^ Int.toString n)
          else
              string (#1 (E.lookupERel env n)))
         handle E.UnboundRel _ => string ("UNBOUND" ^ Int.toString n))
      | ENamed n =>
        if !debug then
            string (#1 (E.lookupENamed env n) ^ "__" ^ Int.toString n)
        else
            string (#1 (E.lookupENamed env n))
      | ECode n => string ("code$" ^ Int.toString n)
      | EApp (e1, e2) => parenIf par (box [p_exp env e1,
                                           space,
                                           p_exp' true env e2])

      | ERecord xes => box [string "{",
                            p_list (fn (x, e) =>
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

      | ELet (xes, e) =>
        let
            val (env, pps) = foldl (fn ((x, e), (env, pps)) =>
                                       (E.pushERel env x dummyTyp,
                                        List.revAppend ([space,
                                                        string "val",
                                                        space,
                                                        string x,
                                                        space,
                                                        string "=",
                                                        space,
                                                        p_exp env e],
                                                        pps)))
                                   (env, []) xes
        in
            box [string "let",
                 space,
                 box (rev pps),
                 space,
                 string "in",
                 space,
                 p_exp env e,
                 space,
                 string "end"]
        end

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
      | DFun (n, x, dom, ran, e) =>
        let
            val xp = if !debug then
                         box [string x,
                              string "__",
                              string (Int.toString n)]
                     else
                         string x        
        in
            box [string "fun",
                 space,
                 string "code$",
                 string (Int.toString n),
                 space,
                 string "(",
                 xp,
                 space,
                 string ":",
                 space,
                 p_typ env dom,
                 string ")",
                 space,
                 string ":",
                 space,
                 p_typ env ran,
                 space,
                 string "=",
                 space,
                 p_exp (E.pushERel env x dom) e]

        end

fun p_file env file =
    let
        val (_, pds) = ListUtil.mapfoldl (fn (d, env) =>
                                             (E.declBinds env d,
                                              p_decl env d))
                             env file
    in
        p_list_sep newline (fn x => x) pds
    end

end
