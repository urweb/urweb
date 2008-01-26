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

(* Pretty-printing elaborated Laconic/Web *)

structure ElabPrint :> ELAB_PRINT = struct

open Print.PD
open Print

open Elab

structure E = ElabEnv

fun p_kind' par (k, _) =
    case k of
        KType => string "Type"
      | KArrow (k1, k2) => parenIf par (box [p_kind' true k1,
                                             space,
                                             string "->",
                                             space,
                                             p_kind k2])
      | KName => string "Name"
      | KRecord k => box [string "{", p_kind k, string "}"]

      | KError => string "<ERROR>"
      | KUnif (_, ref (SOME k)) => p_kind' par k
      | KUnif (s, _) => string ("<UNIF:" ^ s ^ ">")

and p_kind k = p_kind' false k

fun p_explicitness e =
    case e of
        Explicit => string "::"
      | Implicit => string ":::"

fun p_con' par env (c, _) =
    case c of
        TFun (t1, t2) => parenIf par (box [p_con' true env t1,
                                           space,
                                           string "->",
                                           space,
                                           p_con env t2])
      | TCFun (e, x, k, c) => parenIf par (box [string x,
                                                space,
                                                p_explicitness e,
                                                space,
                                                p_kind k,
                                                space,
                                                string "->",
                                                space,
                                                p_con (E.pushCRel env x k) c])
      | TRecord (CRecord (_, xcs), _) => box [string "{",
                                              p_list (fn (x, c) =>
                                                         box [p_con env x,
                                                              space,
                                                              string ":",
                                                              space,
                                                              p_con env c]) xcs,
                                              string "}"]
      | TRecord c => box [string "$",
                          p_con' true env c]

      | CRel n => string (#1 (E.lookupCRel env n) ^ "_" ^ Int.toString n)
      | CNamed n => string (#1 (E.lookupCNamed env n) ^ "__" ^ Int.toString n)

      | CApp (c1, c2) => parenIf par (box [p_con env c1,
                                           space,
                                           p_con' true env c2])
      | CAbs (e, x, k, c) => parenIf par (box [string "fn",
                                               space,
                                               string x,
                                               space,
                                               p_explicitness e,
                                               space,
                                               p_kind k,
                                               space,
                                               string "=>",
                                               space,
                                               p_con (E.pushCRel env x k) c])

      | CName s => box [string "#", string s]

      | CRecord (k, xcs) => parenIf par (box [string "[",
                                              p_list (fn (x, c) =>
                                                         box [p_con env x,
                                                              space,
                                                              string "=",
                                                              space,
                                                              p_con env c]) xcs,
                                              string "]::",
                                              p_kind k])
      | CConcat (c1, c2) => parenIf par (box [p_con' true env c1,
                                              space,
                                              string "++",
                                              space,
                                              p_con env c2])

      | CError => string "<ERROR>"
      | CUnif (_, ref (SOME c)) => p_con' par env c
      | CUnif (s, _) => string ("<UNIF:" ^ s ^ ">")
        
and p_con env = p_con' false env

fun p_decl env ((d, _) : decl) =
    case d of
        DCon (x, k, c) => box [string "con",
                                    space,
                                    string x,
                                    space,
                                    string "::",
                                    space,
                                    p_kind k,
                                    space,
                                    string "=",
                                    space,
                                    p_con env c]

fun p_file env file =
    let
        val (_, pds) = foldr (fn (d, (env, pds)) =>
                                 (ElabUtil.declBinds env d,
                                  p_decl env d :: pds))
                             (env, []) file
    in
        p_list_sep newline (fn x => x) pds
    end

end
