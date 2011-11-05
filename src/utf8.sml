(* Copyright (c) 2011, Adam Chlipala
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

(* UTF-8 conversion *)

structure Utf8 :> UTF8 = struct

fun byte n = str (chr (Word.toInt n))

fun encode n =
    if n <= 0 then
        raise Fail "Invalid character to UTF-8-encode"
    else if n <= 0x7F then
        str (chr n)
    else if n <= 0x7FF then
        let
            val w = Word.fromInt n
            val b1 = Word.orb (Word.fromInt (128 + 64), Word.>> (w, Word.fromInt 6))
            val b2 = Word.orb (Word.fromInt 128, Word.andb (w, Word.fromInt 63))
        in
            byte b1 ^ byte b2
        end
    else if n <= 0xFFFF then
        let
            val w = Word.fromInt n
            val b1 = Word.orb (Word.fromInt (128 + 64 + 32), Word.>> (w, Word.fromInt 12))
            val b2 = Word.orb (Word.fromInt 128, Word.andb (Word.>> (w, Word.fromInt 6), Word.fromInt 63))
            val b3 = Word.orb (Word.fromInt 128, Word.andb (w, Word.fromInt 63))
        in
            byte b1 ^ byte b2 ^ byte b3
        end
    else
        raise Fail "Exceeded supported range for UTF-8 characters"

end
