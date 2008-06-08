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

signature CORE_UTIL = sig

structure Kind : sig
    val mapfold : (Core.kind', 'state, 'abort) Search.mapfolder
                  -> (Core.kind, 'state, 'abort) Search.mapfolder
    val map : (Core.kind' -> Core.kind') -> Core.kind -> Core.kind
    val exists : (Core.kind' -> bool) -> Core.kind -> bool
end

structure Con : sig
    datatype binder =
             Rel of string * Core.kind
           | Named of string * int * Core.kind * Core.con option

    val mapfoldB : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.con, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                   con : (Core.con', 'state, 'abort) Search.mapfolder}
                  -> (Core.con, 'state, 'abort) Search.mapfolder

    val map : {kind : Core.kind' -> Core.kind',
               con : Core.con' -> Core.con'}
              -> Core.con -> Core.con

    val mapB : {kind : Core.kind' -> Core.kind',
                con : 'context -> Core.con' -> Core.con',
                bind : 'context * binder -> 'context}
               -> 'context -> (Core.con -> Core.con)
    val exists : {kind : Core.kind' -> bool,
                  con : Core.con' -> bool} -> Core.con -> bool
end

structure Exp : sig
    datatype binder =
             RelC of string * Core.kind
           | NamedC of string * int * Core.kind * Core.con option
           | RelE of string * Core.con
           | NamedE of string * int * Core.con * Core.exp option

    val mapfoldB : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Core.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                   con : (Core.con', 'state, 'abort) Search.mapfolder,
                   exp : (Core.exp', 'state, 'abort) Search.mapfolder}
                  -> (Core.exp, 'state, 'abort) Search.mapfolder

    val map : {kind : Core.kind' -> Core.kind',
               con : Core.con' -> Core.con',
               exp : Core.exp' -> Core.exp'}
              -> Core.exp -> Core.exp
    val exists : {kind : Core.kind' -> bool,
                  con : Core.con' -> bool,
                  exp : Core.exp' -> bool} -> Core.exp -> bool
end

structure Decl : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Core.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('context, Core.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.decl, 'state, 'abort) Search.mapfolderB
end

structure File : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Core.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('context, Core.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.file, 'state, 'abort) Search.mapfolderB

    val mapB : {kind : Core.kind' -> Core.kind',
                con : 'context -> Core.con' -> Core.con',
                exp : 'context -> Core.exp' -> Core.exp',
                decl : 'context -> Core.decl' -> Core.decl',
                bind : 'context * binder -> 'context}
               -> 'context -> Core.file -> Core.file
end

end
