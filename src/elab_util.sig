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

signature ELAB_UTIL = sig

structure Kind : sig
    val mapfold : (Elab.kind', 'state, 'abort) Search.mapfolder
                  -> (Elab.kind, 'state, 'abort) Search.mapfolder
    val exists : (Elab.kind' -> bool) -> Elab.kind -> bool
end

structure Con : sig
    datatype binder =
             Rel of string * Elab.kind
           | Named of string * Elab.kind

    val mapfoldB : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.con, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder}
                  -> (Elab.con, 'state, 'abort) Search.mapfolder

    val mapB : {kind : Elab.kind' -> Elab.kind',
                con : 'context -> Elab.con' -> Elab.con',
                bind : 'context * binder -> 'context}
               -> 'context -> (Elab.con -> Elab.con)
    val exists : {kind : Elab.kind' -> bool,
                  con : Elab.con' -> bool} -> Elab.con -> bool
end

structure Exp : sig
    datatype binder =
             RelC of string * Elab.kind
           | NamedC of string * Elab.kind
           | RelE of string * Elab.con
           | NamedE of string * Elab.con

    val mapfoldB : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Elab.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder,
                   exp : (Elab.exp', 'state, 'abort) Search.mapfolder}
                  -> (Elab.exp, 'state, 'abort) Search.mapfolder
    val exists : {kind : Elab.kind' -> bool,
                  con : Elab.con' -> bool,
                  exp : Elab.exp' -> bool} -> Elab.exp -> bool
end

val declBinds : ElabEnv.env -> Elab.decl -> ElabEnv.env

end
