(* Copyright (c) 2008-2010, Adam Chlipala
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

val classifyDatatype : (string * int * 'a option) list -> Elab.datatype_kind

val mliftConInCon : (int -> Elab.con -> Elab.con) ref

structure Kind : sig
    val mapfoldB : {kind : ('context, Elab.kind', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * string -> 'context}
                   -> ('context, Elab.kind, 'state, 'abort) Search.mapfolderB
    val mapfold : (Elab.kind', 'state, 'abort) Search.mapfolder
                  -> (Elab.kind, 'state, 'abort) Search.mapfolder
    val exists : (Elab.kind' -> bool) -> Elab.kind -> bool
    val mapB : {kind : 'context -> Elab.kind' -> Elab.kind',
                bind : 'context * string -> 'context}
               -> 'context -> (Elab.kind -> Elab.kind)
end

structure Con : sig
    datatype binder =
             RelK of string
           | RelC of string * Elab.kind
           | NamedC of string * int * Elab.kind * Elab.con option

    val mapfoldB : {kind : ('context, Elab.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.con, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder}
                  -> (Elab.con, 'state, 'abort) Search.mapfolder

    val mapB : {kind : 'context -> Elab.kind' -> Elab.kind',
                con : 'context -> Elab.con' -> Elab.con',
                bind : 'context * binder -> 'context}
               -> 'context -> (Elab.con -> Elab.con)
    val map : {kind : Elab.kind' -> Elab.kind',
               con : Elab.con' -> Elab.con'}
              -> Elab.con -> Elab.con
    val existsB : {kind : 'context * Elab.kind' -> bool,
                  con : 'context * Elab.con' -> bool,
                   bind : 'context * binder -> 'context}
                  -> 'context -> Elab.con -> bool
    val exists : {kind : Elab.kind' -> bool,
                  con : Elab.con' -> bool} -> Elab.con -> bool

    val foldB : {kind : 'context * Elab.kind' * 'state -> 'state,
                 con : 'context * Elab.con' * 'state -> 'state,
                 bind : 'context * binder -> 'context}
                -> 'context -> 'state -> Elab.con -> 'state
    val fold : {kind : Elab.kind' * 'state -> 'state,
                 con : Elab.con' * 'state -> 'state}
                -> 'state -> Elab.con -> 'state
end

structure Exp : sig
    datatype binder =
             RelK of string
           | RelC of string * Elab.kind
           | NamedC of string * int * Elab.kind * Elab.con option
           | RelE of string * Elab.con
           | NamedE of string * Elab.con

    val mapfoldB : {kind : ('context, Elab.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Elab.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder,
                   exp : (Elab.exp', 'state, 'abort) Search.mapfolder}
                  -> (Elab.exp, 'state, 'abort) Search.mapfolder
    val mapB : {kind : 'context -> Elab.kind' -> Elab.kind',
                con : 'context -> Elab.con' -> Elab.con',
                exp : 'context -> Elab.exp' -> Elab.exp',
                bind : 'context * binder -> 'context}
               -> 'context -> (Elab.exp -> Elab.exp)
    val exists : {kind : Elab.kind' -> bool,
                  con : Elab.con' -> bool,
                  exp : Elab.exp' -> bool} -> Elab.exp -> bool

    val foldB : {kind : 'context * Elab.kind' * 'state -> 'state,
                 con : 'context * Elab.con' * 'state -> 'state,
                 exp : 'context * Elab.exp' * 'state -> 'state,
                 bind : 'context * binder -> 'context}
                -> 'context -> 'state -> Elab.exp -> 'state
end

structure Sgn : sig
    datatype binder =
             RelK of string
           | RelC of string * Elab.kind
           | NamedC of string * int * Elab.kind * Elab.con option
           | Str of string * int * Elab.sgn
           | Sgn of string * int * Elab.sgn

    val mapfoldB : {kind : ('context, Elab.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    sgn_item : ('context, Elab.sgn_item', 'state, 'abort) Search.mapfolderB,
                    sgn : ('context, Elab.sgn', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.sgn, 'state, 'abort) Search.mapfolderB


    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder,
                   sgn_item : (Elab.sgn_item', 'state, 'abort) Search.mapfolder,
                   sgn : (Elab.sgn', 'state, 'abort) Search.mapfolder}
                  -> (Elab.sgn, 'state, 'abort) Search.mapfolder

    val map : {kind : Elab.kind' -> Elab.kind',
               con : Elab.con' -> Elab.con',
               sgn_item : Elab.sgn_item' -> Elab.sgn_item',
               sgn : Elab.sgn' -> Elab.sgn'}
              -> Elab.sgn -> Elab.sgn

    val mapB : {kind : 'context -> Elab.kind' -> Elab.kind',
                con : 'context -> Elab.con' -> Elab.con',
                sgn_item : 'context -> Elab.sgn_item' -> Elab.sgn_item',
                sgn : 'context -> Elab.sgn' -> Elab.sgn',
                bind : 'context * binder -> 'context}
               -> 'context -> Elab.sgn -> Elab.sgn
                              
end

structure Decl : sig
    datatype binder =
             RelK of string
           | RelC of string * Elab.kind
           | NamedC of string * int * Elab.kind * Elab.con option
           | RelE of string * Elab.con
           | NamedE of string * Elab.con
           | Str of string * int * Elab.sgn
           | Sgn of string * int * Elab.sgn

    val mapfoldB : {kind : ('context, Elab.kind', 'state, 'abort) Search.mapfolderB,
                    con : ('context, Elab.con', 'state, 'abort) Search.mapfolderB,
                    exp : ('context, Elab.exp', 'state, 'abort) Search.mapfolderB,
                    sgn_item : ('context, Elab.sgn_item', 'state, 'abort) Search.mapfolderB,
                    sgn : ('context, Elab.sgn', 'state, 'abort) Search.mapfolderB,
                    str : ('context, Elab.str', 'state, 'abort) Search.mapfolderB,
                    decl : ('context, Elab.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'context * binder -> 'context}
                   -> ('context, Elab.decl, 'state, 'abort) Search.mapfolderB
    val mapfold : {kind : (Elab.kind', 'state, 'abort) Search.mapfolder,
                   con : (Elab.con', 'state, 'abort) Search.mapfolder,
                   exp : (Elab.exp', 'state, 'abort) Search.mapfolder,
                   sgn_item : (Elab.sgn_item', 'state, 'abort) Search.mapfolder,
                   sgn : (Elab.sgn', 'state, 'abort) Search.mapfolder,
                   str : (Elab.str', 'state, 'abort) Search.mapfolder,
                   decl : (Elab.decl', 'state, 'abort) Search.mapfolder}
                  -> (Elab.decl, 'state, 'abort) Search.mapfolder
    val exists : {kind : Elab.kind' -> bool,
                  con : Elab.con' -> bool,
                  exp : Elab.exp' -> bool,
                  sgn_item : Elab.sgn_item' -> bool,
                  sgn : Elab.sgn' -> bool,
                  str : Elab.str' -> bool,
                  decl : Elab.decl' -> bool}
                 -> Elab.decl -> bool
    val search : {kind : Elab.kind' -> 'a option,
                  con : Elab.con' -> 'a option,
                  exp : Elab.exp' -> 'a option,
                  sgn_item : Elab.sgn_item' -> 'a option,
                  sgn : Elab.sgn' -> 'a option,
                  str : Elab.str' -> 'a option,
                  decl : Elab.decl' -> 'a option}
                 -> Elab.decl -> 'a option

    val foldMapB : {kind : 'context * Elab.kind' * 'state -> Elab.kind' * 'state,
                    con : 'context * Elab.con' * 'state -> Elab.con' * 'state,
                    exp : 'context * Elab.exp' * 'state -> Elab.exp' * 'state,
                    sgn_item : 'context * Elab.sgn_item' * 'state -> Elab.sgn_item' * 'state,
                    sgn : 'context * Elab.sgn' * 'state -> Elab.sgn' * 'state,
                    str : 'context * Elab.str' * 'state -> Elab.str' * 'state,
                    decl : 'context * Elab.decl' * 'state -> Elab.decl' * 'state,
                    bind : 'context * binder -> 'context}
                   -> 'context -> 'state -> Elab.decl -> Elab.decl * 'state

    val map : {kind : Elab.kind' -> Elab.kind',
               con : Elab.con' -> Elab.con',
               exp : Elab.exp' -> Elab.exp',
               sgn_item : Elab.sgn_item' -> Elab.sgn_item',
               sgn : Elab.sgn' -> Elab.sgn',
               str : Elab.str' -> Elab.str',
               decl : Elab.decl' -> Elab.decl'}
              -> Elab.decl -> Elab.decl

    val mapB : {kind : 'context -> Elab.kind' -> Elab.kind',
                con : 'context -> Elab.con' -> Elab.con',
                exp : 'context -> Elab.exp' -> Elab.exp',
                sgn_item : 'context -> Elab.sgn_item' -> Elab.sgn_item',
                sgn : 'context -> Elab.sgn' -> Elab.sgn',
                str : 'context -> Elab.str' -> Elab.str',
                decl : 'context -> Elab.decl' -> Elab.decl',
                bind : 'context * binder -> 'context}
               -> 'context -> Elab.decl -> Elab.decl
end

structure File : sig
    val maxName : Elab.file -> int

    val findDecl : (Elab.decl -> bool) -> Elab.file -> Elab.decl option
end

end
