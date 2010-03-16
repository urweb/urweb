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
    val compare : Core.kind * Core.kind -> order

    val mapfoldB : {kind : ('context, Core.kind', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * string -> 'context}
                   -> ('context, Core.kind, 'state, 'abort) Search.mapfolderB
    val mapfold : (Core.kind', 'state, 'abort) Search.mapfolder
                  -> (Core.kind, 'state, 'abort) Search.mapfolder
    val map : (Core.kind' -> Core.kind') -> Core.kind -> Core.kind
    val exists : (Core.kind' -> bool) -> Core.kind -> bool
    val mapB : {kind : 'context -> Core.kind' -> Core.kind',
                bind : 'context * string -> 'context}
               -> 'context -> (Core.kind -> Core.kind)
end

structure Con : sig
    val compare : Core.con * Core.con -> order

    datatype binder =
             RelK of string
           | RelC of string * Core.kind
           | NamedC of string * int * Core.kind * Core.con option

    val mapfoldB : {kind : ('context, Core.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.con, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                   con : (Core.con', 'state, 'abort) Search.mapfolder}
                  -> (Core.con, 'state, 'abort) Search.mapfolder

    val map : {kind : Core.kind' -> Core.kind',
               con : Core.con' -> Core.con'}
              -> Core.con -> Core.con

    val mapB : {kind : 'context -> Core.kind' -> Core.kind',
                con : 'context -> Core.con' -> Core.con',
                bind : 'context * binder -> 'context}
               -> 'context -> (Core.con -> Core.con)

    val fold : {kind : Core.kind' * 'state -> 'state,
                con : Core.con' * 'state -> 'state}
               -> 'state -> Core.con -> 'state
                                        
    val exists : {kind : Core.kind' -> bool,
                  con : Core.con' -> bool} -> Core.con -> bool

    val existsB : {kind : 'context * Core.kind' -> bool,
                   con : 'context * Core.con' -> bool,
                   bind : 'context * binder -> 'context}
                  -> 'context -> Core.con -> bool
                                                          
    val foldMap : {kind : Core.kind' * 'state -> Core.kind' * 'state,
                   con : Core.con' * 'state -> Core.con' * 'state}
                  -> 'state -> Core.con -> Core.con * 'state
end

structure Exp : sig
    val compare : Core.exp * Core.exp -> order

    datatype binder =
             RelK of string
           | RelC of string * Core.kind
           | NamedC of string * int * Core.kind * Core.con option
           | RelE of string * Core.con
           | NamedE of string * int * Core.con * Core.exp option * string

    val mapfoldB : {kind : ('context, Core.kind', 'state, 'abort) Search.mapfolderB,
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
    val mapB : {kind : 'context -> Core.kind' -> Core.kind',
                con : 'context -> Core.con' -> Core.con',
                exp : 'context -> Core.exp' -> Core.exp',
                bind : 'context * binder -> 'context}
               -> 'context -> (Core.exp -> Core.exp)

    val fold : {kind : Core.kind' * 'state -> 'state,
                con : Core.con' * 'state -> 'state,
                exp : Core.exp' * 'state -> 'state}
               -> 'state -> Core.exp -> 'state

    val foldB : {kind : 'context * Core.kind' * 'state -> 'state,
                 con : 'context * Core.con' * 'state -> 'state,
                 exp : 'context * Core.exp' * 'state -> 'state,
                 bind : 'context * binder -> 'context}
                -> 'context -> 'state -> Core.exp -> 'state
                                        
    val exists : {kind : Core.kind' -> bool,
                  con : Core.con' -> bool,
                  exp : Core.exp' -> bool} -> Core.exp -> bool

    val existsB : {kind : 'context * Core.kind' -> bool,
                   con : 'context * Core.con' -> bool,
                   exp : 'context * Core.exp' -> bool,
                   bind : 'context * binder -> 'context}
                  -> 'context -> Core.exp -> bool

    val foldMap : {kind : Core.kind' * 'state -> Core.kind' * 'state,
                   con : Core.con' * 'state -> Core.con' * 'state,
                   exp : Core.exp' * 'state -> Core.exp' * 'state}
                  -> 'state -> Core.exp -> Core.exp * 'state
    val foldMapB : {kind : 'context * Core.kind' * 'state -> Core.kind' * 'state,
                    con : 'context * Core.con' * 'state -> Core.con' * 'state,
                    exp : 'context * Core.exp' * 'state -> Core.exp' * 'state,
                    bind : 'context * binder -> 'context}
                   -> 'context -> 'state -> Core.exp -> Core.exp * 'state
end

structure Decl : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {kind : ('context, Core.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Core.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('context, Core.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.decl, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                   con : (Core.con', 'state, 'abort) Search.mapfolder,
                   exp : (Core.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Core.decl', 'state, 'abort) Search.mapfolder}
                  -> (Core.decl, 'state, 'abort) Search.mapfolder

    val fold : {kind : Core.kind' * 'state -> 'state,
                con : Core.con' * 'state -> 'state,
                exp : Core.exp' * 'state -> 'state,
                decl : Core.decl' * 'state -> 'state}
               -> 'state -> Core.decl -> 'state

    val foldMap : {kind : Core.kind' * 'state -> Core.kind' * 'state,
                   con : Core.con' * 'state -> Core.con' * 'state,
                   exp : Core.exp' * 'state -> Core.exp' * 'state,
                   decl : Core.decl' * 'state -> Core.decl' * 'state}
                  -> 'state -> Core.decl -> Core.decl * 'state
    val foldMapB : {kind : 'context * Core.kind' * 'state -> Core.kind' * 'state,
                    con : 'context * Core.con' * 'state -> Core.con' * 'state,
                    exp : 'context * Core.exp' * 'state -> Core.exp' * 'state,
                    decl : 'context * Core.decl' * 'state -> Core.decl' * 'state,
                    bind : 'context * binder -> 'context}
                   -> 'context -> 'state -> Core.decl -> Core.decl * 'state

    val exists : {kind : Core.kind' -> bool,
                  con : Core.con' -> bool,
                  exp : Core.exp' -> bool,
                  decl : Core.decl' -> bool} -> Core.decl -> bool
end

structure File : sig
    val maxName : Core.file -> int

    datatype binder = datatype Exp.binder

    val mapfoldB : {kind : ('context, Core.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Core.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Core.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('context, Core.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Core.file, 'state, 'abort) Search.mapfolderB

    val mapfold : {kind : (Core.kind', 'state, 'abort) Search.mapfolder,
                   con : (Core.con', 'state, 'abort) Search.mapfolder,
                   exp : (Core.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Core.decl', 'state, 'abort) Search.mapfolder}
                  -> (Core.file, 'state, 'abort) Search.mapfolder

    val mapB : {kind : 'context -> Core.kind' -> Core.kind',
                con : 'context -> Core.con' -> Core.con',
                exp : 'context -> Core.exp' -> Core.exp',
                decl : 'context -> Core.decl' -> Core.decl',
                bind : 'context * binder -> 'context}
               -> 'context -> Core.file -> Core.file

    val map : {kind : Core.kind' -> Core.kind',
               con : Core.con' -> Core.con',
               exp : Core.exp' -> Core.exp',
               decl : Core.decl' -> Core.decl'}
              -> Core.file -> Core.file

    val fold : {kind : Core.kind' * 'state -> 'state,
                con : Core.con' * 'state -> 'state,
                exp : Core.exp' * 'state -> 'state,
                decl : Core.decl' * 'state -> 'state}
               -> 'state -> Core.file -> 'state

    val foldMap : {kind : Core.kind' * 'state -> Core.kind' * 'state,
                   con : Core.con' * 'state -> Core.con' * 'state,
                   exp : Core.exp' * 'state -> Core.exp' * 'state,
                   decl : Core.decl' * 'state -> Core.decl' * 'state}
                  -> 'state -> Core.file -> Core.file * 'state
end

end
