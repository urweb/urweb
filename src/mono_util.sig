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

signature MONO_UTIL = sig

structure Typ : sig
    val compare : Mono.typ * Mono.typ -> order
    val sortFields : (string * Mono.typ) list -> (string * Mono.typ) list

    val mapfold : (Mono.typ', 'state, 'abort) Search.mapfolder
                  -> (Mono.typ, 'state, 'abort) Search.mapfolder

    val map : (Mono.typ' -> Mono.typ')
              -> Mono.typ -> Mono.typ

    val fold : (Mono.typ' * 'state -> 'state)
              -> 'state -> Mono.typ -> 'state

    val exists : (Mono.typ' -> bool) -> Mono.typ -> bool
end

structure Exp : sig
    datatype binder =
             Datatype of string * int * (string * int * Mono.typ option) list
           | RelE of string * Mono.typ
           | NamedE of string * int * Mono.typ * Mono.exp option * string

    val mapfoldB : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Mono.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Mono.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Mono.exp', 'state, 'abort) Search.mapfolder}
                  -> (Mono.exp, 'state, 'abort) Search.mapfolder

    val map : {typ : Mono.typ' -> Mono.typ',
               exp : Mono.exp' -> Mono.exp'}
              -> Mono.exp -> Mono.exp
    val mapB : {typ : Mono.typ' -> Mono.typ',
                exp : 'typtext -> Mono.exp' -> Mono.exp',
                bind : 'typtext * binder -> 'typtext}
               -> 'typtext -> (Mono.exp -> Mono.exp)

    val fold : {typ : Mono.typ' * 'state -> 'state,
                exp : Mono.exp' * 'state -> 'state}
               -> 'state -> Mono.exp -> 'state
                                        
    val exists : {typ : Mono.typ' -> bool,
                  exp : Mono.exp' -> bool} -> Mono.exp -> bool

    val existsB : {typ : Mono.typ' -> bool,
                   exp : 'context * Mono.exp' -> bool,
                   bind : 'context * binder -> 'context} -> 'context -> Mono.exp -> bool

    val foldB : {typ : Mono.typ' * 'state -> 'state,
                 exp : 'context * Mono.exp' * 'state -> 'state,
                 bind : 'context * binder -> 'context}
                -> 'context -> 'state -> Mono.exp -> 'state
end

structure Decl : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Mono.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('typtext, Mono.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Mono.decl, 'state, 'abort) Search.mapfolderB
    val mapfold : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Mono.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Mono.decl', 'state, 'abort) Search.mapfolder}
                  -> (Mono.decl, 'state, 'abort) Search.mapfolder

    val fold : {typ : Mono.typ' * 'state -> 'state,
                exp : Mono.exp' * 'state -> 'state,
                decl : Mono.decl' * 'state -> 'state}
               -> 'state -> Mono.decl -> 'state

    val map : {typ : Mono.typ' -> Mono.typ',
               exp : Mono.exp' -> Mono.exp',
               decl : Mono.decl' -> Mono.decl'}
              -> Mono.decl -> Mono.decl

    val foldMapB : {typ : Mono.typ' * 'state -> Mono.typ' * 'state,
                    exp : 'context * Mono.exp' * 'state -> Mono.exp' * 'state,
                    decl : 'context * Mono.decl' * 'state -> Mono.decl' * 'state,
                    bind : 'context * binder -> 'context}
                   -> 'context -> 'state -> Mono.decl -> Mono.decl * 'state
end

structure File : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Mono.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('typtext, Mono.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Mono.file, 'state, 'abort) Search.mapfolderB

    val mapfold : {typ : (Mono.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Mono.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Mono.decl', 'state, 'abort) Search.mapfolder}
                  -> (Mono.file, 'state, 'abort) Search.mapfolder

    val mapB : {typ : Mono.typ' -> Mono.typ',
                exp : 'typtext -> Mono.exp' -> Mono.exp',
                decl : 'typtext -> Mono.decl' -> Mono.decl',
                bind : 'typtext * binder -> 'typtext}
               -> 'typtext -> Mono.file -> Mono.file

    val map : {typ : Mono.typ' -> Mono.typ',
                exp : Mono.exp' -> Mono.exp',
                decl : Mono.decl' -> Mono.decl'}
              -> Mono.file -> Mono.file

    val fold : {typ : Mono.typ' * 'state -> 'state,
                exp : Mono.exp' * 'state -> 'state,
                decl : Mono.decl' * 'state -> 'state}
               -> 'state -> Mono.file -> 'state

    val maxName : Mono.file -> int
end

end
