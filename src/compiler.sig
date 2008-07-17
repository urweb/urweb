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

(* Laconic/Web main compiler interface *)

signature COMPILER = sig

    type job = string list
    val compile : job -> unit

    val parseLig : string -> Source.sgn_item list option
    val testLig : string -> unit

    val parseLac : string -> Source.file option
    val testLac : string -> unit

    val parse : job -> Source.file option
    val elaborate : job -> Elab.file option
    val explify : job -> Expl.file option
    val corify : job -> Core.file option
    val shake' : job -> Core.file option
    val tag : job -> Core.file option
    val reduce : job -> Core.file option
    val shake : job -> Core.file option
    val monoize : job -> Mono.file option
    val mono_opt' : job -> Mono.file option
    val untangle : job -> Mono.file option
    val mono_opt : job -> Mono.file option
    val cjrize : job -> Cjr.file option

    val testParse : job -> unit
    val testElaborate : job -> unit
    val testExplify : job -> unit
    val testCorify : job -> unit
    val testShake' : job -> unit
    val testTag : job -> unit
    val testReduce : job -> unit
    val testShake : job -> unit
    val testMonoize : job -> unit
    val testMono_opt' : job -> unit
    val testUntangle : job -> unit
    val testMono_opt : job -> unit
    val testCjrize : job -> unit

end
