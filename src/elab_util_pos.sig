(* Copyright (c) 2008-2010, 2012, Adam Chlipala
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

(* This is identical to ELAB_UTIL, but keeps source spans around *)
(* Maybe these modules can be unified? *)

signature ELAB_UTIL_POS = sig 
    
    val mliftConInCon : (int -> Elab.con -> Elab.con) ref

    structure Decl : sig 
                  datatype binder =
                           RelK of string
                           | RelC of string * Elab.kind
                           | NamedC of string * int * Elab.kind * Elab.con option
                           | RelE of string * Elab.con
                           | NamedE of string * Elab.con
                           | Str of string * int * Elab.sgn
                           | Sgn of string * int * Elab.sgn
                  
                  val fold : {kind : Elab.kind * 'state -> 'state,
                              con : Elab.con * 'state -> 'state,
                              exp : Elab.exp * 'state -> 'state,
                              sgn_item : Elab.sgn_item * 'state -> 'state,
                              sgn : Elab.sgn * 'state -> 'state,
                              str : Elab.str * 'state -> 'state,
                              decl : Elab.decl * 'state -> 'state}
                             -> 'state -> Elab.decl -> 'state

                  val foldB : {kind : 'context * Elab.kind * 'state -> 'state,
                              con : 'context * Elab.con * 'state -> 'state,
                              exp : 'context * Elab.exp * 'state -> 'state,
                              sgn_item : 'context * Elab.sgn_item * 'state -> 'state,
                              sgn : 'context * Elab.sgn * 'state -> 'state,
                              str : 'context * Elab.str * 'state -> 'state,
                              decl : 'context * Elab.decl * 'state -> 'state,
                              bind: 'context * binder -> 'context
                              }
                             -> 'context -> 'state -> Elab.decl -> 'state
              end

end
