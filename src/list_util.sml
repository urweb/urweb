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

structure ListUtil :> LIST_UTIL = struct

structure S = Search

fun mapConcat f =
    let
        fun mc acc ls =
            case ls of
                [] => rev acc
              | h :: t => mc (List.revAppend (f h, acc)) t
    in
        mc []
    end

fun mapfold f =
    let
        fun mf ls s =
            case ls of
                nil => S.Continue (nil, s)
              | h :: t =>
                case f h s of
                    S.Return x => S.Return x
                  | S.Continue (h', s) =>
                    case mf t s of
                        S.Return x => S.Return x
                      | S.Continue (t', s) => S.Continue (h' :: t', s)
    in
        mf
    end

fun mapfoldB f =
    let
        fun mf ctx ls s =
            case ls of
                nil => S.Continue (nil, s)
              | h :: t =>
                let
                    val (ctx, r) = f (ctx, h)
                in
                    case r s of
                        S.Return x => S.Return x
                      | S.Continue (h', s) =>
                        case mf ctx t s of
                            S.Return x => S.Return x
                          | S.Continue (t', s) => S.Continue (h' :: t', s)
                end
    in
        mf
    end

fun foldlMap f s =
    let
        fun fm (ls', s) ls =
            case ls of
                nil => (rev ls', s)
              | h :: t =>
                let
                    val (h', s') = f (h, s)
                in
                    fm (h' :: ls', s') t
                end
    in
        fm ([], s)
    end

fun foldlMapConcat f s =
    let
        fun fm (ls', s) ls =
            case ls of
                nil => (rev ls', s)
              | h :: t =>
                let
                    val (h', s') = f (h, s)
                in
                    fm (List.revAppend (h', ls'), s') t
                end
    in
        fm ([], s)
    end

fun foldlMapPartial f s =
    let
        fun fm (ls', s) ls =
            case ls of
                nil => (rev ls', s)
              | h :: t =>
                let
                    val (h', s') = f (h, s)
                    val ls' = case h' of
                                  NONE => ls'
                                | SOME h' => h' :: ls'
                in
                    fm (ls', s') t
                end
    in
        fm ([], s)
    end

fun foldlMapiPartial f s =
    let
        fun fm (n, ls', s) ls =
            case ls of
                nil => (rev ls', s)
              | h :: t =>
                let
                    val (h', s') = f (n, h, s)
                    val ls' = case h' of
                                  NONE => ls'
                                | SOME h' => h' :: ls'
                in
                    fm (n + 1, ls', s') t
                end
    in
        fm (0, [], s)
    end

fun foldlMapAbort f s =
    let
        fun fm (ls', s) ls =
            case ls of
                nil => SOME (rev ls', s)
              | h :: t =>
                case f (h, s) of
                    NONE => NONE
                  | SOME (h', s') => fm (h' :: ls', s') t
    in
        fm ([], s)
    end

fun search f =
    let
        fun s ls =
            case ls of
                [] => NONE
              | h :: t =>
                case f h of
                    NONE => s t
                  | v => v
    in
        s
    end

fun searchi f =
    let
        fun s n ls =
            case ls of
                [] => NONE
              | h :: t =>
                case f (n, h) of
                    NONE => s (n + 1) t
                  | v => v
    in
        s 0
    end

fun mapi f =
    let
        fun m i acc ls =
            case ls of
                [] => rev acc
              | h :: t => m (i + 1) (f (i, h) :: acc) t
    in
        m 0 []
    end

fun mapiPartial f =
    let
        fun m i acc ls =
            case ls of
                [] => rev acc
              | h :: t =>
                m (i + 1) (case f (i, h) of
                               NONE => acc
                             | SOME v => v :: acc) t
    in
        m 0 []
    end

fun appi f =
    let
        fun m i ls =
            case ls of
                [] => ()
              | h :: t => (f (i, h); m (i + 1) t)
    in
        m 0
    end

fun foldli f =
    let
        fun m i acc ls =
            case ls of
                [] => acc
              | h :: t => m (i + 1) (f (i, h, acc)) t
    in
        m 0
    end

fun foldri f i ls =
    let
        val len = length ls
    in
        foldli (fn (n, x, s) => f (len - n - 1, x, s)) i (rev ls)
    end

fun foldliMap f s =
    let
        fun fm (n, ls', s) ls =
            case ls of
                nil => (rev ls', s)
              | h :: t =>
                let
                    val (h', s') = f (n, h, s)
                in
                    fm (n + 1, h' :: ls', s') t
                end
    in
        fm (0, [], s)
    end

fun appn f n =
    let
        fun iter m =
            if m >= n then
                ()
            else
                (f m;
                 iter (m + 1))
    in
        iter 0
    end

end
