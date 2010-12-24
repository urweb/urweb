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

(* Generic printing support code *)

structure Print :> PRINT = struct

structure SM = TextIOPP
structure PD = PPDescFn(SM)

val openOut = SM.openOut

type 'a printer = 'a -> PD.pp_desc

fun box ds = PD.hovBox (PD.PPS.Rel 1, ds)
fun parenIf b d =
    if b then
        box [PD.string "(", d, PD.string ")"]
    else
        d
val space = PD.space 1

val out = SM.openOut {dst = TextIO.stdOut, wid = 70}
val err = SM.openOut {dst = TextIO.stdErr, wid = 70}

fun p_list_sep sep f ls =
    case ls of
        [] => PD.string ""
      | [x] => f x
      | x :: rest =>
        let
            val tokens = foldr (fn (x, tokens) =>
                                   sep :: PD.cut :: f x :: tokens)
                               [] rest
        in
            box (f x :: tokens)
        end
fun p_list f = p_list_sep (box [PD.string ",", space]) f

fun p_list_sepi sep f ls =
    case ls of
        [] => PD.string ""
      | [x] => f 0 x
      | x :: rest =>
        let
            val tokens = ListUtil.foldri (fn (n, x, tokens) =>
                                             sep :: PD.cut :: f (n + 1) x :: tokens)
                                         [] rest
        in
            box (f 0 x :: tokens)
        end

fun fprint f d = (PD.description (f, d);
                  PD.PPS.flushStream f)
val print = fprint out
val eprint = fprint err

fun fpreface f (s, d) =
    fprint f (PD.hovBox (PD.PPS.Rel 0,
		         [PD.string s, PD.space 1, d, PD.newline]))

val preface = fpreface out
val epreface = fpreface err

fun fprefaces f s ls =
    let
        val len = foldl (fn ((s, _), best) =>
                            Int.max (size s, best)) 0 ls
    in
        fprint f (PD.string s);
        fprint f PD.newline;
        app (fn (s, d) =>
                let
                    val s = CharVector.tabulate (len - size s,
                                                 fn _ => #" ")
                            ^ s ^ ": "
                in
                    fpreface f (s, d)
                end) ls
    end

val prefaces = fprefaces out
val eprefaces = fprefaces err

fun fprefaces' f ls =
    let
        val len = foldl (fn ((s, _), best) =>
                            Int.max (size s, best)) 0 ls
    in
        app (fn (s, d) =>
                let
                    val s = CharVector.tabulate (len - size s,
                                                 fn _ => #" ")
                            ^ s ^ ": "
                in
                    fpreface f (s, d)
                end) ls
    end

val prefaces' = fprefaces' out
val eprefaces' = fprefaces' err

end
