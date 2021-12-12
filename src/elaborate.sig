(* Copyright (c) 2008, 2012, Adam Chlipala
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

signature ELABORATE = sig

    val elabFile : Source.sgn_item list -> Time.time
                   -> Source.decl list -> Source.sgn_item list -> Time.time
                   -> ElabEnv.env
                   -> (ElabEnv.env -> ElabEnv.env) (* Adapt env after stdlib but before elaborate *)
                   -> Source.file
                   -> Elab.file

    val resolveClass : ElabEnv.env -> Elab.con -> Elab.exp option

    val dumpTypes : bool ref
    (* After elaboration (successful or failed), should I output a mapping from
     * all identifiers to their kinds/types? *)

    val dumpTypesOnError : bool ref
    (* Like above, but only used if there are compile errors. *)

    val unifyMore : bool ref
    (* Run all phases of type inference, even if an error is detected by an
     * early phase. *)

    val incremental : bool ref
    val verbose : bool ref

    val dopen: ElabEnv.env
               -> { str: int
                  , strs: string list
                  , sgn: Elab.sgn }
               -> (Elab.decl list * ElabEnv.env)

    val elabSgn: (ElabEnv.env * Disjoint.env)
                 -> Source.sgn
                 -> (Elab.sgn * Disjoint.goal list)

    structure Blames : sig
                  type t
              end

    datatype constraint =
             Disjoint of Blames.t * Disjoint.goal
             | TypeClass of Blames.t * ElabEnv.env * Elab.con * Elab.exp option ref * ErrorMsg.span

    val elabStr: (ElabEnv.env * Disjoint.env)
                 -> Source.str
                 -> (Elab.str * Elab.sgn * constraint list)

    val subSgn: ElabEnv.env -> ErrorMsg.span -> Elab.sgn -> Elab.sgn -> unit
end
