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

(* Remove unused definitions from a file *)

structure Shake :> SHAKE = struct

val sliceDb = ref false

open Core

structure U = CoreUtil

structure IS = IntBinarySet
structure IM = IntBinaryMap

type free = {
     con : IS.set,
     exp : IS.set
}

val dummyt = (TRecord (CRecord ((KType, ErrorMsg.dummySpan), []), ErrorMsg.dummySpan), ErrorMsg.dummySpan)
val dummye = (EPrim (Prim.String ""), ErrorMsg.dummySpan)

fun tupleC cs = (CTuple cs, ErrorMsg.dummySpan)
fun tupleE es = (ERecord (map (fn e => (dummyt, e, dummyt)) es), ErrorMsg.dummySpan)

fun shake file =
    let
        val usedVarsC = U.Con.fold {kind = fn (_, st) => st,
                                    con = fn (c, cs) =>
                                             case c of
                                                 CNamed n => IS.add (cs, n)
                                               | _ => cs}

        val usedVars = U.Exp.fold {kind = fn (_, st) => st,
                                   con = fn (c, st as (es, cs)) =>
                                            case c of
                                                CNamed n => (es, IS.add (cs, n))
                                              | _ => st,
                                   exp = fn (e, st as (es, cs)) =>
                                            case e of
                                                ENamed n => (IS.add (es, n), cs)
                                              | _ => st}

        val (usedE, usedC) =
            List.foldl
                (fn ((DExport (_, n, _), _), st as (usedE, usedC)) =>
                    if !sliceDb then
                        st
                    else
                        (IS.add (usedE, n), usedC)
                  | ((DTable (_, _, c, _, pe, pc, ce, cc), _), (usedE, usedC)) =>
                    let
                        val usedC = usedVarsC usedC c
                        val usedC = usedVarsC usedC pc
                        val usedC = usedVarsC usedC cc

                        val (usedE, usedC) = usedVars (usedE, usedC) pe
                        val (usedE, usedC) = usedVars (usedE, usedC) ce
                    in
                        (usedE, usedC)
                    end
                  | ((DView (_, _, _, e, c), _), (usedE, usedC)) =>
                    let
                        val usedC = usedVarsC usedC c
                    in
                        usedVars (usedE, usedC) e
                    end
                  | ((DTask (e1, e2), _), st) =>
                    if !sliceDb then
                        st
                    else
                        usedVars (usedVars st e1) e2
                  | ((DPolicy e1, _), st) =>
                    if !sliceDb then
                        st
                    else
                        usedVars st e1
                  | ((DOnError n, _), st as (usedE, usedC)) =>
                    if !sliceDb then
                        st
                    else
                        (IS.add (usedE, n), usedC)
                  | (_, acc) => acc) (IS.empty, IS.empty) file

        val (cdef, edef) = foldl (fn ((DCon (_, n, _, c), _), (cdef, edef)) => (IM.insert (cdef, n, [c]), edef)
                                   | ((DDatatype dts, _), (cdef, edef)) =>
                                     (foldl (fn ((_, n, _, xncs), cdef) =>
                                                IM.insert (cdef, n, List.mapPartial #3 xncs)) cdef dts, edef)
                                   | ((DVal (_, n, t, e, _), _), (cdef, edef)) => (cdef, IM.insert (edef, n, ([], t, e)))
                                   | ((DValRec vis, _), (cdef, edef)) =>
                                     let
                                         val all_ns = map (fn (_, n, _, _, _) => n) vis
                                     in
                                         (cdef, foldl (fn ((_, n, t, e, _), edef) =>
                                                          IM.insert (edef, n, (all_ns, t, e))) edef vis)
                                     end
                                   | ((DExport _, _), acc) => acc
                                   | ((DTable (_, n, c, _, e1, c1, e2, c2), _), (cdef, edef)) =>
                                     (cdef, IM.insert (edef, n, ([], tupleC [c, c1, c2], tupleE [e1, e2])))
                                   | ((DSequence (_, n, _), _), (cdef, edef)) =>
                                     (cdef, IM.insert (edef, n, ([], dummyt, dummye)))
                                   | ((DView (_, n, _, _, c), _), (cdef, edef)) =>
                                     (cdef, IM.insert (edef, n, ([], c, dummye)))
                                   | ((DDatabase _, _), acc) => acc
                                   | ((DCookie (_, n, c, _), _), (cdef, edef)) =>
                                     (cdef, IM.insert (edef, n, ([], c, dummye)))
                                   | ((DStyle (_, n, _), _), (cdef, edef)) =>
                                     (cdef, IM.insert (edef, n, ([], dummyt, dummye)))
                                   | ((DTask _, _), acc) => acc
                                   | ((DPolicy _, _), acc) => acc
                                   | ((DOnError _, _), acc) => acc)
                                 (IM.empty, IM.empty) file

        fun kind (_, s) = s

        fun con (c, s) =
            case c of
                CNamed n =>
                if IS.member (#con s, n) then
                    s
                else
                    let
                        val s' = {con = IS.add (#con s, n),
                                  exp = #exp s}
                    in
                        case IM.find (cdef, n) of
                            NONE => s'
                          | SOME cs => foldl (fn (c, s') => shakeCon s' c) s' cs
                    end
              | _ => s

        and shakeCon s = U.Con.fold {kind = kind, con = con} s

        (*val () = print "=====\nSHAKE\n=====\n"
        val current = ref 0*)

        fun exp (e, s) =
            let
                fun check n =
                    if IS.member (#exp s, n) then
                        s
                    else
                        let
                            val s' = {exp = IS.add (#exp s, n),
                                      con = #con s}
                        in
                            (*print ("Need " ^ Int.toString n ^ " <-- " ^ Int.toString (!current) ^ "\n");*)
                            case IM.find (edef, n) of
                                NONE => s'
                              | SOME (ns, t, e) =>
                                let
                                    (*val old = !current
                                    val () = current := n*)
                                    val s' = shakeExp (shakeCon s' t) e
                                in
                                    (*current := old;*)
                                    foldl (fn (n, s') => exp (ENamed n, s')) s' ns
                                end
                        end
            in
                case e of
                    ENamed n => check n
                  | EServerCall (n, _, _) => check n
                  | _ => s
            end

        and shakeExp s = U.Exp.fold {kind = kind, con = con, exp = exp} s

        val s = {con = usedC, exp = usedE}

        val s = IS.foldl (fn (n, s) =>
                             case IM.find (edef, n) of
                                 NONE => raise Fail "Shake: Couldn't find 'val'"
                               | SOME (ns, t, e) =>
                                 let
                                     (*val () = current := n*)
                                     val s = shakeExp (shakeCon s t) e
                                 in
                                     foldl (fn (n, s) => exp (ENamed n, s)) s ns
                                 end) s usedE

        val s = IS.foldl (fn (n, s) =>
                             case IM.find (cdef, n) of
                                 NONE => raise Fail ("Shake: Couldn't find 'con' " ^ Int.toString n)
                               | SOME cs => foldl (fn (c, s) => shakeCon s c) s cs) s usedC
    in
        List.filter (fn (DCon (_, n, _, _), _) => IS.member (#con s, n)
                      | (DDatatype dts, _) => List.exists (fn (_, n, _, _) => IS.member (#con s, n)) dts
                      | (DVal (_, n, _, _, _), _) => IS.member (#exp s, n)
                      | (DValRec vis, _) => List.exists (fn (_, n, _, _, _) => IS.member (#exp s, n)) vis
                      | (DExport _, _) => not (!sliceDb)
                      | (DView _, _) => true
                      | (DSequence _, _) => true
                      | (DTable _, _) => true
                      | (DDatabase _, _) => not (!sliceDb)
                      | (DCookie _, _) => not (!sliceDb)
                      | (DStyle _, _) => not (!sliceDb)
                      | (DTask _, _) => not (!sliceDb)
                      | (DPolicy _, _) => not (!sliceDb)
                      | (DOnError _, _) => not (!sliceDb)) file
    end

end
