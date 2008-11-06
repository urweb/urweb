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

datatype skey =
         Named of int
       | App of skey * skey

structure K = struct
type ord_key = skey list
fun compare' (k1, k2) =
    case (k1, k2) of
        (Named n1, Named n2) => Int.compare (n1, n2)
      | (Named _, _) => LESS
      | (_, Named _) => GREATER

      | (App (x1, y1), App (x2, y2)) => Order.join (compare' (x1, x2), fn () => compare' (y1, y2))

val compare = Order.joinL compare'
end

structure KM = BinaryMapFn(K)
structure IM = IntBinaryMap

fun skeyIn (e, _) =
    case e of
        ENamed n => SOME (Named n)
      | EApp (e1, e2) =>
        (case (skeyIn e1, skeyIn e2) of
             (SOME k1, SOME k2) => SOME (App (k1, k2))
           | _ => NONE)
      | _ => NONE

fun skeyOut (k, loc) =
    case k of
        Named n => (ENamed n, loc)
      | App (k1, k2) => (EApp (skeyOut (k1, loc), skeyOut (k2, loc)), loc)

type func = {
     name : string,
     args : int KM.map,
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
              | EApp (e1, e2) =>
                (case getApp (#1 e1) of
                     NONE => NONE
                   | SOME (f, xs, xs') =>
                     let
                         val k =
                             if List.null xs' then
                                 skeyIn e2
                             else
                                 NONE
                     in
                         case k of
                             NONE => SOME (f, xs, xs' @ [e2])
                           | SOME k => SOME (f, xs @ [k], xs')
                     end)
              | _ => NONE
    in
        case getApp e of
            NONE => (e, st)
          | SOME (_, [], _) => (e, st)
          | SOME (f, xs, xs') =>
            case IM.find (#funcs st, f) of
                NONE => ((*print ("SHOT DOWN! " ^ Int.toString f ^ "\n");*) (e, st))
              | SOME {name, args, body, typ, tag} =>
                case KM.find (args, xs) of
                    SOME f' => ((*Print.prefaces "Pre-existing" [("e", CorePrint.p_exp CoreEnv.empty (e, ErrorMsg.dummySpan))];*)
                                (#1 (foldl (fn (e, arg) => (EApp (e, arg), ErrorMsg.dummySpan))
                                           (ENamed f', ErrorMsg.dummySpan) xs'),
                                 st))
                  | NONE =>
                    let
                        (*val () = Print.prefaces "New" [("e", CorePrint.p_exp CoreEnv.empty (e, ErrorMsg.dummySpan))]*)

                        fun subBody (body, typ, xs) =
                            case (#1 body, #1 typ, xs) of
                                (_, _, []) => SOME (body, typ)
                              | (EAbs (_, _, _, body'), TFun (_, typ'), x :: xs) =>
                                let
                                    val body'' = E.subExpInExp (0, skeyOut (x, #2 body)) body'
                                in
                                    (*Print.prefaces "espec" [("body'", CorePrint.p_exp CoreEnv.empty body'),
                                                            ("body''", CorePrint.p_exp CoreEnv.empty body'')];*)
                                    subBody (body'',
                                             typ',
                                             xs)
                                end
                              | _ => NONE
                    in
                        case subBody (body, typ, xs) of
                            NONE => (e, st)
                          | SOME (body', typ') =>
                            let
                                val f' = #maxName st
                                (*val () = print ("f' = " ^ Int.toString f' ^ "\n")*)
                                val funcs = IM.insert (#funcs st, f, {name = name,
                                                                      args = KM.insert (args, xs, f'),
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
                                  decls = (name, f', typ', body', tag) :: #decls st})
                            end
                    end
    end

and specExp st = U.Exp.foldMap {kind = kind, con = con, exp = exp} st

fun decl (d, st) = (d, st)

val specDecl = U.Decl.foldMap {kind = kind, con = con, exp = exp, decl = decl}

fun specialize' file =
    let
        fun doDecl (d, (st : state, changed)) =
            let
                val funcs = #funcs st
                val funcs = 
                    case #1 d of
                        DValRec vis =>
                        foldl (fn ((x, n, c, e, tag), funcs) =>
                                  IM.insert (funcs, n, {name = x,
                                                        args = KM.empty,
                                                        body = e,
                                                        typ = c,
                                                        tag = tag}))
                              funcs vis
                      | _ => funcs

                val st = {maxName = #maxName st,
                          funcs = funcs,
                          decls = []}

                val (d', st) = specDecl st d

                val funcs = #funcs st
                val funcs =
                    case #1 d of
                        DVal (x, n, c, e as (EAbs _, _), tag) =>
                        IM.insert (funcs, n, {name = x,
                                              args = KM.empty,
                                              body = e,
                                              typ = c,
                                              tag = tag})
                      | DVal (_, n, _, (ENamed n', _), _) =>
                        (case IM.find (funcs, n') of
                             NONE => funcs
                           | SOME v => IM.insert (funcs, n, v))
                      | _ => funcs

                val (changed, ds) =
                    case #decls st of
                        [] => (changed, [d'])
                      | vis =>
                        (true, case d' of
                                   (DValRec vis', _) => [(DValRec (vis @ vis'), ErrorMsg.dummySpan)]
                                 | _ => [(DValRec vis, ErrorMsg.dummySpan), d'])
            in
                (ds, ({maxName = #maxName st,
                       funcs = funcs,
                       decls = []}, changed))
            end

        val (ds, (_, changed)) = ListUtil.foldlMapConcat doDecl
                                                         ({maxName = U.File.maxName file + 1,
                                                           funcs = IM.empty,
                                                           decls = []}, false)
                                                         file
    in
        (changed, ds)
    end

fun specialize file =
    let
        val (changed, file) = specialize' file
    in
        if changed then
            specialize file
        else
            file
    end


end
