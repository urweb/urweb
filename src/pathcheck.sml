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

structure PathCheck :> PATH_CHECK = struct

open Mono

structure E = ErrorMsg

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

fun checkDecl ((d, loc), (funcs, rels, cookies, styles)) =
    let
        fun doFunc s =
            (if SS.member (funcs, s) then
                 E.errorAt loc ("Duplicate function path " ^ s)
             else
                 ();
             (SS.add (funcs, s), rels, cookies, styles))

        fun doRel s =
            (if SS.member (rels, s) then
                 E.errorAt loc ("Duplicate table/sequence path " ^ s)
             else
                 ();
             (funcs, SS.add (rels, s), cookies, styles))

        fun doCookie s =
            (if SS.member (cookies, s) then
                 E.errorAt loc ("Duplicate cookie path " ^ s)
             else
                 ();
             (funcs, rels, SS.add (cookies, s), styles))

        fun doStyle s =
            (if SS.member (styles, s) then
                 E.errorAt loc ("Duplicate style path " ^ s)
             else
                 ();
             (funcs, rels, cookies, SS.add (styles, s)))
    in
        case d of
            DExport (_, s, _, _, _, _) => doFunc s
            
          | DTable (s, _, pe, ce) =>
            let
                fun constraints (e, rels) =
                    case #1 e of
                        ERecord [(s', _, _)] =>
                        let
                            val s' = s ^ "_" ^ s'
                        in
                            if SS.member (rels, s') then
                                E.errorAt loc ("Duplicate constraint path " ^ s')
                            else
                                ();
                            SS.add (rels, s')
                        end
                      | EStrcat (e1, e2) => constraints (e2, constraints (e1, rels))
                      | _ => rels

                val rels = #2 (doRel s)
                val rels = case #1 pe of
                               EPrim (Prim.String "") => rels
                             | _ =>
                               let
                                   val s' = s ^ "_Pkey"
                               in
                                   if SS.member (rels, s') then
                                       E.errorAt loc ("Duplicate primary key constraint path " ^ s')
                                   else
                                       ();
                                   SS.add (rels, s')
                               end
            in
                (funcs, constraints (ce, rels), cookies, styles)
            end
          | DSequence s => doRel s

          | DCookie s => doCookie s
          | DStyle s => doStyle s

          | _ => (funcs, rels, cookies, styles)
    end

fun check ds = ignore (foldl checkDecl (SS.empty, SS.empty, SS.empty, SS.empty) ds)

end
