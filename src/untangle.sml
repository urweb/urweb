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

structure Untangle :> UNTANGLE = struct

open Mono

structure U = MonoUtil
structure E = MonoEnv

structure IS = IntBinarySet
structure IM = IntBinaryMap

fun typ (k, s) = s

fun exp (e, s) =
    case e of
        ENamed n => IS.add (s, n)

      | _ => s

fun untangle (file : file) =
    let
        fun decl (dAll as (d, loc)) =
            case d of
                DValRec vis =>
                let
                    val thisGroup = foldl (fn ((_, n, _, _, _), thisGroup) =>
                                              IS.add (thisGroup, n)) IS.empty vis

                    val used = foldl (fn ((_, n, _, e, _), used) =>
                                       let
                                           val usedHere = U.Exp.fold {typ = typ,
                                                                      exp = exp} IS.empty e
                                       in
                                           IM.insert (used, n, IS.intersection (usedHere, thisGroup))
                                       end)
                               IM.empty vis

                    fun p_graph reachable =
                        IM.appi (fn (n, reachableHere) =>
                                    (print (Int.toString n);
                                     print ":";
                                     IS.app (fn n' => (print " ";
                                                       print (Int.toString n'))) reachableHere;
                                     print "\n")) reachable

                    (*val () = print "used:\n"
                    val () = p_graph used*)

                    fun expand reachable =
                        let
                            val changed = ref false

                            val reachable =
                                IM.mapi (fn (n, reachableHere) =>
                                            IS.foldl (fn (n', reachableHere) =>
                                                         let
                                                             val more = valOf (IM.find (reachable, n'))
                                                         in
                                                             if IS.isEmpty (IS.difference (more, reachableHere)) then
                                                                 reachableHere
                                                             else
                                                                 (changed := true;
                                                                  IS.union (more, reachableHere))
                                                         end)
                                                     reachableHere reachableHere) reachable
                        in
                            (reachable, !changed)
                        end

                    fun iterate reachable =
                        let
                            val (reachable, changed) = expand reachable
                        in
                            if changed then
                                iterate reachable
                            else
                                reachable
                        end

                    val reachable = iterate used

                    (*val () = print "reachable:\n"
                    val () = p_graph reachable*)

                    fun sccs (nodes, acc) =
                        case IS.find (fn _ => true) nodes of
                            NONE => acc
                          | SOME rep =>
                            let
                                val reachableHere = valOf (IM.find (reachable, rep))

                                val (nodes, scc) = IS.foldl (fn (node, (nodes, scc)) =>
                                                                if node = rep then
                                                                    (nodes, scc)
                                                                else
                                                                    let
                                                                        val reachableThere =
                                                                            valOf (IM.find (reachable, node))
                                                                    in
                                                                        if IS.member (reachableThere, rep) then
                                                                            (IS.delete (nodes, node),
                                                                             IS.add (scc, node))
                                                                        else
                                                                            (nodes, scc)
                                                                    end)
                                                   (IS.delete (nodes, rep), IS.singleton rep) reachableHere
                            in
                                sccs (nodes, scc :: acc)
                            end

                    val sccs = sccs (thisGroup, [])
                    (*val () = app (fn nodes => (print "SCC:";
                                               IS.app (fn i => (print " ";
                                                                print (Int.toString i))) nodes;
                                               print "\n")) sccs*)

                    fun depends nodes1 nodes2 =
                        let
                            val node1 = valOf (IS.find (fn _ => true) nodes1)
                            val node2 = valOf (IS.find (fn _ => true) nodes2)
                            val reachable1 = valOf (IM.find (reachable, node1))
                        in
                            IS.member (reachable1, node2)
                        end

                    fun findReady (sccs, passed) =
                        case sccs of
                            [] => raise Fail "Untangle: Unable to topologically sort 'val rec'"
                          | nodes :: sccs =>
                            if List.exists (depends nodes) passed
                               orelse List.exists (depends nodes) sccs then
                                findReady (sccs, nodes :: passed)
                            else
                                (nodes, List.revAppend (passed, sccs))

                    fun topo (sccs, acc) =
                        case sccs of
                            [] => rev acc
                          | _ =>
                            let
                                val (node, sccs) = findReady (sccs, [])
                            in
                                topo (sccs, node :: acc)
                            end

                    val sccs = topo (sccs, [])
                    (*val () = app (fn nodes => (print "SCC':";
                                               IS.app (fn i => (print " ";
                                                                print (Int.toString i))) nodes;
                                               print "\n")) sccs*)

                    fun isNonrec nodes =
                        case IS.find (fn _ => true) nodes of
                            NONE => NONE
                          | SOME node =>
                            let
                                val nodes = IS.delete (nodes, node)
                                val reachableHere = valOf (IM.find (reachable, node))
                            in
                                if IS.isEmpty nodes then
                                    if IS.member (reachableHere, node) then
                                        NONE
                                    else
                                        SOME node
                                else
                                    NONE
                            end

                    val ds = map (fn nodes =>
                                     case isNonrec nodes of
                                         SOME node =>
                                         let
                                             val vi = valOf (List.find (fn (_, n, _, _, _) => n = node) vis)
                                         in
                                             (DVal vi, loc)
                                         end
                                       | NONE =>
                                         (DValRec (List.filter (fn (_, n, _, _, _) => IS.member (nodes, n)) vis), loc))
                                 sccs
                in
                    ds
                end
              | _ => [dAll]
    in
        (ListUtil.mapConcat decl (#1 file), #2 file)
    end

end
