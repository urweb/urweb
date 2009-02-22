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

signature EXPL_UTIL = sig

structure Kind : sig
    val mapfoldB : {kind : ('context, Expl.kind', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * string -> 'context}
                   -> ('context, Expl.kind, 'state, 'abort) Search.mapfolderB
    val mapfold : (Expl.kind', 'state, 'abort) Search.mapfolder
                  -> (Expl.kind, 'state, 'abort) Search.mapfolder
    val exists : (Expl.kind' -> bool) -> Expl.kind -> bool
    val mapB : {kind : 'context -> Expl.kind' -> Expl.kind',
                bind : 'context * string -> 'context}
               -> 'context -> (Expl.kind -> Expl.kind)
end

structure Con : sig
    datatype binder =
             RelK of string
           | RelC of string * Expl.kind
           | NamedC of string * Expl.kind

    val mapfoldB : {kind : ('context, Expl.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Expl.con', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Expl.con, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Expl.kind', 'state, 'abort) Search.mapfolder,
                   con : (Expl.con', 'state, 'abort) Search.mapfolder}
                  -> (Expl.con, 'state, 'abort) Search.mapfolder

    val mapB : {kind : 'context -> Expl.kind' -> Expl.kind',
                con : 'context -> Expl.con' -> Expl.con',
                bind : 'context * binder -> 'context}
               -> 'context -> (Expl.con -> Expl.con)
    val map : {kind : Expl.kind' -> Expl.kind',
               con : Expl.con' -> Expl.con'}
              -> Expl.con -> Expl.con
    val exists : {kind : Expl.kind' -> bool,
                  con : Expl.con' -> bool} -> Expl.con -> bool
end

structure Exp : sig
    datatype binder =
             RelK of string
           | RelC of string * Expl.kind
           | NamedC of string * Expl.kind
           | RelE of string * Expl.con
           | NamedE of string * Expl.con

    val mapfoldB : {kind : ('context, Expl.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Expl.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Expl.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Expl.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Expl.kind', 'state, 'abort) Search.mapfolder,
                   con : (Expl.con', 'state, 'abort) Search.mapfolder,
                   exp : (Expl.exp', 'state, 'abort) Search.mapfolder}
                  -> (Expl.exp, 'state, 'abort) Search.mapfolder
    val exists : {kind : Expl.kind' -> bool,
                  con : Expl.con' -> bool,
                  exp : Expl.exp' -> bool} -> Expl.exp -> bool
end

structure Sgn : sig
    datatype binder =
             RelK of string
           | RelC of string * Expl.kind
           | NamedC of string * Expl.kind
           | Sgn of string * Expl.sgn
           | Str of string * Expl.sgn

    val mapfoldB : {kind : ('context, Expl.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Expl.con', 'state, 'abort) Search.mapfolderB,
                    sgn_item : ('context, Expl.sgn_item', 'state, 'abort) Search.mapfolderB,
                    sgn : ('context, Expl.sgn', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Expl.sgn, 'state, 'abort) Search.mapfolderB


    val mapfold : {kind : (Expl.kind', 'state, 'abort) Search.mapfolder,
                   con : (Expl.con', 'state, 'abort) Search.mapfolder,
                   sgn_item : (Expl.sgn_item', 'state, 'abort) Search.mapfolder,
                   sgn : (Expl.sgn', 'state, 'abort) Search.mapfolder}
                  -> (Expl.sgn, 'state, 'abort) Search.mapfolder

    val map : {kind : Expl.kind' -> Expl.kind',
               con : Expl.con' -> Expl.con',
               sgn_item : Expl.sgn_item' -> Expl.sgn_item',
               sgn : Expl.sgn' -> Expl.sgn'}
              -> Expl.sgn -> Expl.sgn

end

end
