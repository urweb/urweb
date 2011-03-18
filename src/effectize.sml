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

structure Effective :> EFFECTIZE = struct

open Core

structure U = CoreUtil

structure IM = IntBinaryMap
structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

fun effectful x = Settings.isEffectful x andalso not (Settings.isClientOnly x)

fun effectize file =
    let
        fun expOnload evs e =
            case e of
                EFfi f => effectful f
              | EFfiApp (m, x, _) => effectful (m, x)
              | ENamed n => IM.inDomain (evs, n)
              | EServerCall (n, _, _) => IM.inDomain (evs, n)
              | _ => false

        fun couldWriteOnload evs = U.Exp.exists {kind = fn _ => false,
                                                 con = fn _ => false,
                                                 exp = expOnload evs}

        fun exp evs e =
            case e of
                EFfi f => effectful f
              | EFfiApp (m, x, _) => effectful (m, x)
              | ENamed n => IM.inDomain (evs, n)
              | ERecord xets => List.exists (fn ((CName "Onload", _), e, _) => couldWriteOnload evs e
                                              | _ => false) xets
              | _ => false

        fun couldWrite evs = U.Exp.exists {kind = fn _ => false,
                                           con = fn _ => false,
                                           exp = exp evs}

        fun exp writers readers pushers e =
            case e of
                ENamed n => IM.inDomain (pushers, n)
              | EServerCall (n, _, _) => IM.inDomain (writers, n) andalso IM.inDomain (readers, n)
              | _ => false

        fun couldWriteWithRpc writers readers pushers = U.Exp.exists {kind = fn _ => false,
                                                                      con = fn _ => false,
                                                                      exp = exp writers readers pushers}

        fun exp evs e =
            case e of
                EFfi ("Basis", "getCookie") => true
              | ENamed n => IM.inDomain (evs, n)
              | EServerCall (n, _, _) => IM.inDomain (evs, n)
              | _ => false

        fun couldReadCookie evs = U.Exp.exists {kind = fn _ => false,
                                                con = fn _ => false,
                                                exp = exp evs}

        val dejs = U.Exp.map {kind = fn x => x,
                              con = fn c => c,
                              exp = fn ERecord xets => ERecord (List.filter (fn ((CName x, _), _ , _) => x = "Onload" orelse not (String.isPrefix "On" x)
                                                                              | _ => true) xets)
                                     | e => e}

        fun doDecl (d, evs as (writers, readers, pushers)) =
            case #1 d of
                DVal (x, n, t, e, s) =>
                let
                    val e' = dejs e
                in
                    (d, (if couldWrite writers e' then
                             IM.insert (writers, n, (#2 d, s))
                         else
                             writers,
                         if couldReadCookie readers e' then
                             IM.insert (readers, n, (#2 d, s))
                         else
                             readers,
                         if couldWriteWithRpc writers readers pushers e then
                             IM.insert (pushers, n, (#2 d, s))
                         else
                             pushers))
                end
              | DValRec vis =>
                let
                    fun oneRound evs =
                        foldl (fn ((_, n, _, e, s), (changed, (writers, readers, pushers))) =>
                                  let
                                      val e' = dejs e

                                      val (changed, writers) =
                                          if couldWrite writers e' andalso not (IM.inDomain (writers, n)) then
                                              (true, IM.insert (writers, n, (#2 d, s)))
                                          else
                                              (changed, writers)

                                      val (changed, readers) =
                                          if couldReadCookie readers e' andalso not (IM.inDomain (readers, n)) then
                                              (true, IM.insert (readers, n, (#2 d, s)))
                                          else
                                              (changed, readers)

                                      val (changed, pushers) =
                                          if couldWriteWithRpc writers readers pushers e
                                             andalso not (IM.inDomain (pushers, n)) then
                                              (true, IM.insert (pushers, n, (#2 d, s)))
                                          else
                                              (changed, pushers)
                                  in
                                      (changed, (writers, readers, pushers))
                                  end) (false, evs) vis

                    fun loop evs =
                        let
                            val (b, evs) = oneRound evs
                        in
                            if b then
                                loop evs
                            else
                                evs
                        end
                in
                    (d, loop (writers, readers, pushers))
                end
              | DExport (Link, n, t) =>
                (case IM.find (writers, n) of
                     NONE => ()
                   | SOME (loc, s) =>
                     if Settings.isSafeGet s then
                         ()
                     else
                         ErrorMsg.errorAt loc ("A link (" ^ s
                                               ^ ") could cause side effects; try implementing it with a form instead");
                 ((DExport (Link, n, IM.inDomain (pushers, n)), #2 d), evs))
              | DExport (Action _, n, _) =>
                ((DExport (Action (if IM.inDomain (writers, n) then
                                       if IM.inDomain (readers, n) then
                                           ReadCookieWrite
                                       else
                                           ReadWrite
                                   else
                                       ReadOnly), n, IM.inDomain (pushers, n)), #2 d),
                 evs)
              | DExport (Rpc _, n, _) =>
                ((DExport (Rpc (if IM.inDomain (writers, n) then
                                    if IM.inDomain (readers, n) then
                                        ReadCookieWrite
                                    else
                                        ReadWrite
                                else
                                    ReadOnly), n, IM.inDomain (pushers, n)), #2 d),
                 evs)
              | DExport (Extern _, n, _) =>
                ((DExport (Extern (if IM.inDomain (writers, n) then
                                       if IM.inDomain (readers, n) then
                                           ReadCookieWrite
                                       else
                                           ReadWrite
                                   else
                                       ReadOnly), n, IM.inDomain (pushers, n)), #2 d),
                 evs)
              | _ => (d, evs)

        val (file, _) = ListUtil.foldlMap doDecl (IM.empty, IM.empty, IM.empty) file
    in
        file
    end

end
