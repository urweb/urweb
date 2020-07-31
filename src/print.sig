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

(* Pretty-printing *)

signature PRINT = sig
    structure PD : PP_DESC
                       where type PPS.token = string
          and type PPS.device = TextIOPP.device
          and type PPS.stream = TextIOPP.stream

    type 'a printer = 'a -> PD.pp_desc

    val box : PD.pp_desc list -> PD.pp_desc
    val vbox : PD.pp_desc list -> PD.pp_desc
    val parenIf : bool -> PD.pp_desc -> PD.pp_desc
    val space : PD.pp_desc
    val indent : int -> PD.pp_desc

    val p_list_sep : PD.pp_desc -> 'a printer -> 'a list printer
    val p_list : 'a printer -> 'a list printer

    val p_list_sepi : PD.pp_desc -> (int -> 'a printer) -> 'a list printer

    val fprint : PD.PPS.stream -> PD.pp_desc -> unit
    val print : PD.pp_desc -> unit
    val eprint : PD.pp_desc -> unit

    val fpreface : PD.PPS.stream -> string * PD.pp_desc -> unit
    val preface : string * PD.pp_desc -> unit
    val epreface : string * PD.pp_desc -> unit

    val fprefaces : PD.PPS.stream -> string -> (string * PD.pp_desc) list -> unit
    val prefaces : string -> (string * PD.pp_desc) list -> unit
    val eprefaces : string -> (string * PD.pp_desc) list -> unit

    val fprefaces' : PD.PPS.stream -> (string * PD.pp_desc) list -> unit
    val prefaces' : (string * PD.pp_desc) list -> unit
    val eprefaces' : (string * PD.pp_desc) list -> unit

    val openOut : {dst : TextIO.outstream, wid : int} -> PD.PPS.stream
end
