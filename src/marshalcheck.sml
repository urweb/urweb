(* Copyright (c) 2009-2010, Adam Chlipala
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

structure MarshalCheck :> MARSHAL_CHECK = struct

open Core

structure U = CoreUtil
structure E = ErrorMsg

structure PK = struct
open Order
type ord_key = string * string
fun compare ((m1, x1), (m2, x2)) =
    join (String.compare (m1, m2),
          fn () => String.compare (x1, x2))
end

structure PS = BinarySetFn(PK)
structure PS = struct
open PS
fun toString' (m, x) = m ^ "." ^ x
fun toString set =
    case PS.listItems set of
        [] => "{}"
      | [x] => toString' x
      | x :: xs => List.foldl (fn (x, s) => s ^ ", " ^ toString' x) (toString' x) xs
end

structure IM = IntBinaryMap

fun check file =
    let
        fun kind (_, st) = st

        fun con cmap (c, st) =
            case c of
                CFfi mx =>
                if Settings.mayClientToServer mx then
                    st
                else
                    PS.add (st, mx)
              | CNamed n =>
                (case IM.find (cmap, n) of
                     NONE => st
                   | SOME st' => PS.union (st, st'))
              | _ => st

        fun sins cmap = U.Con.fold {kind = kind, con = con cmap} PS.empty
    in
        ignore (foldl (fn ((d, _), (cmap, emap)) =>
                          case d of
                              DCon (_, n, _, c) => (IM.insert (cmap, n, sins cmap c), emap)
                            | DDatatype dts =>
                              (foldl (fn ((_, n, _, xncs), cmap) =>
                                         IM.insert (cmap, n, foldl (fn ((_, _, co), s) =>
                                                                       case co of
                                                                           NONE => s
                                                                         | SOME c => PS.union (s, sins cmap c))
                                                                   PS.empty xncs)) cmap dts,
                               emap)

                            | DVal (_, n, t, _, tag) => (cmap, IM.insert (emap, n, (t, tag)))
                            | DValRec vis => (cmap,
                                              foldl (fn ((_, n, t, _, tag), emap) => IM.insert (emap, n, (t, tag)))
                                                    emap vis)

                            | DExport (_, n, _) =>
                              (case IM.find (emap, n) of
                                   NONE => raise Fail "MarshalCheck: Unknown export"
                                 | SOME (t, tag) =>
                                   let
                                       fun makeS (t, _) =
                                           case t of
                                               TFun (dom, ran) =>
                                               (case #1 dom of
                                                    CFfi ("Basis", "postBody") => makeS ran
                                                  | CApp ((CFfi ("Basis", "option"), _), (CFfi ("Basis", "queryString"), _)) => makeS ran
                                                  | _ => PS.union (sins cmap dom, makeS ran))
                                             | _ => PS.empty
                                       val s = makeS t
                                   in
                                       if PS.isEmpty s then
                                           ()
                                       else
                                           E.error ("Input to exported function '"
                                                    ^ tag ^ "' involves one or more disallowed types: "
                                                    ^ PS.toString s);
                                       (cmap, emap)
                                   end)

                            | DCookie (_, _, t, tag) =>
                              let
                                  val s = sins cmap t
                              in
                                  if PS.isEmpty s then
                                      ()
                                  else
                                      E.error ("Cookie '" ^ tag ^ "' includes one or more disallowed types: "
                                               ^ PS.toString s);
                                  (cmap, emap)
                              end

                            | _ => (cmap, emap))
                      (IM.empty, IM.empty) file)
    end

end
