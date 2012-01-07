(* Copyright (c) 2010, Adam Chlipala
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

structure Css :> CSS = struct

structure IM = IntBinaryMap

structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

datatype inheritable = Block | List | Table | Caption | Td
datatype others = OBlock | OTable | OTd | Tr | NonReplacedInline | ReplacedInline | Width | Height

fun inheritableToString x =
    case x of
        Block => "B"
      | List => "L"
      | Table => "T"
      | Caption => "C"
      | Td => "D"

fun othersToString x =
    case x of
        OBlock => "b"
      | OTable => "t"
      | OTd => "d"
      | Tr => "-"
      | NonReplacedInline => "N"
      | ReplacedInline => "R"
      | Width => "W"
      | Height => "H"

type summary = inheritable list * others list

fun merge' (ls1, ls2) = foldl (fn (x, ls) => if List.exists (fn y => y = x) ls then ls else x :: ls) ls2 ls1
fun merge ((in1, ot1), (in2, ot2)) = (merge' (in1, in2), merge' (ot1, ot2))
fun mergePC {parent = (in1, ot1), child = in2} = (merge' (in1, in2), ot1)

val nada = ([], [])
val block = ([Block], [OBlock, Width, Height])
val inline = ([], [NonReplacedInline])
val list = ([Block, List], [OBlock, Width, Height])
val replaced = ([], [ReplacedInline, Width, Height])
val table = ([Block, Table], [OBlock, OTable, Width, Height])
val tr = ([Block], [OBlock, Tr, Height])
val td = ([Block, Td], [OBlock, OTd, Width])

val tags = [("span", inline),
            ("div", block),
            ("p", block),
            ("b", inline),
            ("i", inline),
            ("tt", inline),
            ("h1", block),
            ("h2", block),
            ("h3", block),
            ("h4", block),
            ("h5", block),
            ("h6", block),
            ("li", list),
            ("ol", list),
            ("ul", list),
            ("hr", block),
            ("a", inline),
            ("img", replaced),
            ("form", block),
            ("hidden", replaced),
            ("textbox", replaced),
            ("password", replaced),
            ("textarea", replaced),
            ("checkbox", replaced),
            ("upload", replaced),
            ("radio", replaced),
            ("select", replaced),
            ("submit", replaced),
            ("label", inline),
            ("ctextbox", replaced),
            ("button", replaced),
            ("ccheckbox", replaced),
            ("cselect", replaced),
            ("ctextarea", replaced),
            ("tabl", table),
            ("tr", tr),
            ("th", td),
            ("td", td)]

val tags = foldl (fn ((tag, css), tags) =>
                     SM.insert (tags, tag, css)) SM.empty tags

open Core

fun summarize file =
    let
        fun decl ((d, _), st as (globals, classes)) =
            let
                fun getTag (e, _) =
                    case e of
                        EFfi ("Basis", tag) => SOME tag
                      | ECApp (e, _) => getTag e
                      | EApp (e, _) => getTag e
                      | _ => NONE

                fun exp ((e, _), classes) =
                    case e of
                        EPrim _ => ([], classes)
                      | ERel _ => ([], classes)
                      | ENamed n =>
                        (case IM.find (globals, n) of
                             NONE => []
                           | SOME (_, sm) => sm,
                         classes)
                      | ECon (_, _, _, NONE) => ([], classes)
                      | ECon (_, _, _, SOME e) => exp (e, classes)
                      | EFfi _ => ([], classes)
                      | EFfiApp (_, _, es) => expList (map #1 es, classes)

                      | EApp (
                        (EApp (
                         (EApp (
                          (EApp (
                           (EApp (
                            (ECApp (
                             (ECApp (
                              (ECApp (
                               (ECApp (
                                (ECApp (
                                 (ECApp (
                                  (ECApp (
                                   (ECApp (
                                    (EFfi ("Basis", "tag"),
                                     _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _),
                            (ECon (_, _, _, SOME (ENamed class, _)), _)), _),
                           _), _),
                          attrs), _),
                         tag), _),
                        xml) =>
                        let
                            val (sm, classes) = exp (xml, classes)
                            val (sm', classes) = exp (attrs, classes)
                            val sm = merge' (sm, sm')
                        in
                            case getTag tag of
                                NONE => (sm, classes)
                              | SOME tag =>
                                case SM.find (tags, tag) of
                                    NONE => (sm, classes)
                                  | SOME sm' =>
                                    let
                                        val sm'' = mergePC {parent = sm', child = sm}
                                        val old = Option.getOpt (IM.find (classes, class), nada)
                                        val classes = IM.insert (classes, class, merge (old, sm''))
                                    in
                                        (merge' (#1 sm', sm), classes)
                                    end
                        end

                      | EApp (
                        (EApp (
                         (EApp (
                          (EApp (
                           (EApp (
                            (ECApp (
                             (ECApp (
                              (ECApp (
                               (ECApp (
                                (ECApp (
                                 (ECApp (
                                  (ECApp (
                                   (ECApp (
                                    (EFfi ("Basis", "tag"),
                                     _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _),
                            _), _),
                           _), _),
                          attrs), _),
                         tag), _),
                        xml) =>
                        let
                            val (sm, classes) = exp (xml, classes)
                            val (sm', classes) = exp (attrs, classes)
                            val sm = merge' (sm, sm')
                        in
                            case getTag tag of
                                NONE => (sm, classes)
                              | SOME tag =>
                                case SM.find (tags, tag) of
                                    NONE => (sm, classes)
                                  | SOME sm' => (merge' (#1 sm', sm), classes)
                        end

                      | EApp (e1, e2) =>
                        let
                            val (sm1, classes) = exp (e1, classes)
                            val (sm2, classes) = exp (e2, classes)
                        in
                            (merge' (sm1, sm2), classes)
                        end
                      | EAbs (_, _, _, e) => exp (e, classes)
                      | ECApp (e, _) => exp (e, classes)
                      | ECAbs (_, _, e) => exp (e, classes)
                      | EKAbs (_, e) => exp (e, classes)
                      | EKApp (e, _) => exp (e, classes)
                      | ERecord xets => expList (map #2 xets, classes)
                      | EField (e, _, _) => exp (e, classes)
                      | EConcat (e1, _, e2, _) =>
                        let
                            val (sm1, classes) = exp (e1, classes)
                            val (sm2, classes) = exp (e2, classes)
                        in
                            (merge' (sm1, sm2), classes)
                        end
                      | ECut (e, _, _) => exp (e, classes)
                      | ECutMulti (e, _, _) => exp (e, classes)
                      | ECase (e, pes, _) =>
                        let
                            val (sm, classes) = exp (e, classes)
                            val (sms, classes) = expList (map #2 pes, classes)
                        in
                            (merge' (sm, sms), classes)
                        end
                      | EWrite e => exp (e, classes)
                      | EClosure (_, es) => expList (es, classes)
                      | ELet (_, _, e1, e2) =>
                        let
                            val (sm1, classes) = exp (e1, classes)
                            val (sm2, classes) = exp (e2, classes)
                        in
                            (merge' (sm1, sm2), classes)
                        end
                      | EServerCall (_, es, _) => expList (es, classes)

                and expList (es, classes) = foldl (fn (e, (sm, classes)) =>
                                                      let
                                                          val (sm', classes) = exp (e, classes)
                                                      in
                                                          (merge' (sm, sm'), classes)
                                                      end) ([], classes) es
            in
                case d of
                    DCon _ => st
                  | DDatatype _ => st
                  | DVal (_, n, _, e, _) =>
                    let
                        val (sm, classes) = exp (e, classes)
                    in
                        (IM.insert (globals, n, (NONE, sm)), classes)
                    end
                  | DValRec vis =>
                    let
                        val (sm, classes) = foldl (fn ((_, _, _, e, _),
                                                       (sm, classes)) =>
                                                      let
                                                          val (sm', classes) = exp (e, classes)
                                                      in
                                                          (merge' (sm', sm), classes)
                                                      end) ([], classes) vis
                    in
                        (foldl (fn ((_, n, _, _, _), globals) => IM.insert (globals, n, (NONE, sm))) globals vis,
                         classes)
                    end
                  | DExport _ => st
                  | DTable _ => st
                  | DSequence _ => st
                  | DView _ => st
                  | DDatabase _ => st
                  | DCookie _ => st
                  | DStyle (_, n, s) => (IM.insert (globals, n, (SOME s, [])), classes)
                  | DTask _ => st
                  | DPolicy _ => st
                  | DOnError _ => st
            end

        val (globals, classes) = foldl decl (IM.empty, IM.empty) file
    in
        {Overall = IM.foldl (fn ((_, sm), sm') => merge' (sm, sm')) [] globals,
         Classes = ListMergeSort.sort (fn ((s1, _), (s2, _)) => String.compare (s1, s2) = GREATER)
                                      (List.mapPartial (fn (i, sm) =>
                                                           case IM.find (globals, i) of
                                                               SOME (SOME s, _) => SOME (s, sm)
                                                             | _ => NONE) (IM.listItemsi classes))}
    end

type report = {Overall : inheritable list,
               Classes : (string * summary) list}

end
