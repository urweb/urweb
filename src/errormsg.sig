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

signature ERROR_MSG = sig

    type pos = {line : int,
                char : int}
           
    type span = {file : string,
                 first : pos,
                 last : pos}

    type 'a located = 'a * span

    val posToString : pos -> string
    val spanToString : span -> string

    val dummyPos : pos
    val dummySpan : span

    val resetPositioning : string -> unit
    val newline : int -> unit
    val lastLineStart : unit -> int
    val posOf : int -> pos
    val spanOf : int * int -> span

    (* To monitor in which modules the elaboration phase finds errors *)
    val startElabStructure : string -> unit
    val stopElabStructureAndGetErrored : string -> bool (* Did the module elab encounter errors? *)

    val resetStructureTracker: unit -> unit
    val resetErrors : unit -> unit
    val anyErrors : unit -> bool
    val error : string -> unit
    val errorAt : span -> string -> unit
    val errorAt' : int * int -> string -> unit
    val readErrorLog: unit ->
                      { span: span
                      , message: string } list
    val withOnError : (unit -> unit) -> (unit -> 'a) -> 'a
end
