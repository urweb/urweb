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

structure ESpecialize :> ESPECIALIZE = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

structure ILK = struct
type ord_key = int list
val compare = Order.joinL Int.compare
end

structure ILM = BinaryMapFn(ILK)
structure IM = IntBinaryMap

type func = {
     name : string,
     args : int ILM.map,
     body : exp,
     typ : con,
     tag : string
}

type state = {
     maxName : int,
     funcs : func IM.map,
     decls : (string * int * con * exp * string) list
}

fun kind (k, st) = (k, st)
fun con (c, st) = (c, st)

fun exp (e, st : state) =
    let
        fun getApp e =
            case e of
                ENamed f => SOME (f, [], [])
              | EApp (e1, (ENamed x, _)) =>
                (case getApp (#1 e1) of
                     NONE => NONE
                   | SOME (f, xs, xs') => SOME (f, xs @ [x], xs'))
              | EApp (e1, e2) =>
                (case getApp (#1 e1) of
                     NONE => NONE
                   | SOME (f, xs, xs') => SOME (f, xs, xs' @ [e2]))
              | _ => NONE
    in
        case getApp e of
            NONE => (e, st)
          | SOME (_, [], _) => (e, st)
          | SOME (f, xs, xs') =>
            case IM.find (#funcs st, f) of
                NONE => (e, st)
              | SOME {name, args, body, typ, tag} =>
                case ILM.find (args, xs) of
                    SOME f' => (#1 (foldl (fn (e, arg) => (EApp (e, arg), ErrorMsg.dummySpan))
                                          (ENamed f', ErrorMsg.dummySpan) xs'),
                                st)
                  | NONE =>
                    let
                        fun subBody (body, typ, xs) =
                            case (#1 body, #1 typ, xs) of
                                (_, _, []) => SOME (body, typ)
                              | (EAbs (_, _, _, body'), TFun (_, typ'), x :: xs) =>
                                subBody (E.subExpInExp (0, (ENamed x, ErrorMsg.dummySpan)) body',
                                         typ',
                                         xs)
                              | _ => NONE
                    in
                        case subBody (body, typ, xs) of
                            NONE => (e, st)
                          | SOME (body', typ') =>
                            let
                                val f' = #maxName st
                                val funcs = IM.insert (#funcs st, f, {name = name,
                                                                      args = ILM.insert (args, xs, f'),
                                                                      body = body,
                                                                      typ = typ,
                                                                      tag = tag})
                                val st = {
                                    maxName = f' + 1,
                                    funcs = funcs,
                                    decls = #decls st
                                }

                                val (body', st) = specExp st body'
                                val e' = foldl (fn (e, arg) => (EApp (e, arg), ErrorMsg.dummySpan))
                                               (ENamed f', ErrorMsg.dummySpan) xs'
                            in
                                (#1 e',
                                 {maxName = #maxName st,
                                  funcs = #funcs st,
                                  decls = (name, f', typ', body', tag ^ "_espec") :: #decls st})
                            end
                    end
    end

and specExp st = U.Exp.foldMap {kind = kind, con = con, exp = exp} st

fun decl (d, st) = (d, st)

val specDecl = U.Decl.foldMap {kind = kind, con = con, exp = exp, decl = decl}

fun specialize file =
    let
        fun doDecl (d, st) =
            let
                val (d', st) = specDecl st d

                val funcs = #funcs st
                val funcs =
                    case #1 d of
                        DVal (x, n, c, e as (EAbs _, _), tag) =>
                        IM.insert (funcs, n, {name = x,
                                              args = ILM.empty,
                                              body = e,
                                              typ = c,
                                              tag = tag})
                      | DValRec vis =>
                        foldl (fn ((x, n, c, e, tag), funcs) =>
                                  IM.insert (funcs, n, {name = x,
                                                        args = ILM.empty,
                                                        body = e,
                                                        typ = c,
                                                        tag = tag}))
                              funcs vis
                      | _ => funcs

                val ds =
                    case #decls st of
                        [] => [d']
                      | vis => [(DValRec vis, ErrorMsg.dummySpan), d']
            in
                (ds, {maxName = #maxName st,
                      funcs = funcs,
                      decls = []})
            end

        val (ds, _) = ListUtil.foldlMapConcat doDecl
                      {maxName = U.File.maxName file + 1,
                       funcs = IM.empty,
                       decls = []}
                      file
    in
        ds
    end


end
