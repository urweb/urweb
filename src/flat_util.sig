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

signature FLAT_UTIL = sig

structure Typ : sig
    val mapfold : (Flat.typ', 'state, 'abort) Search.mapfolder
                  -> (Flat.typ, 'state, 'abort) Search.mapfolder

    val map : (Flat.typ' -> Flat.typ')
              -> Flat.typ -> Flat.typ

    val fold : (Flat.typ' * 'state -> 'state)
              -> 'state -> Flat.typ -> 'state

    val exists : (Flat.typ' -> bool) -> Flat.typ -> bool
end

structure Exp : sig
    datatype binder =
             NamedT of string * int * Flat.typ option
           | RelE of string * Flat.typ
           | NamedE of string * int * Flat.typ * Flat.exp option

    val mapfoldB : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Flat.exp', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Flat.exp, 'state, 'abort) Search.mapfolderB
    val mapfold : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Flat.exp', 'state, 'abort) Search.mapfolder}
                  -> (Flat.exp, 'state, 'abort) Search.mapfolder

    val map : {typ : Flat.typ' -> Flat.typ',
               exp : Flat.exp' -> Flat.exp'}
              -> Flat.exp -> Flat.exp
    val mapB : {typ : Flat.typ' -> Flat.typ',
                exp : 'typtext -> Flat.exp' -> Flat.exp',
                bind : 'typtext * binder -> 'typtext}
               -> 'typtext -> (Flat.exp -> Flat.exp)

    val fold : {typ : Flat.typ' * 'state -> 'state,
                exp : Flat.exp' * 'state -> 'state}
               -> 'state -> Flat.exp -> 'state
                                        
    val exists : {typ : Flat.typ' -> bool,
                  exp : Flat.exp' -> bool} -> Flat.exp -> bool
end

structure Decl : sig
    datatype binder = datatype Exp.binder

    val mapfoldB : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Flat.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('typtext, Flat.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Flat.decl, 'state, 'abort) Search.mapfolderB
    val mapfold : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Flat.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Flat.decl', 'state, 'abort) Search.mapfolder}
                  -> (Flat.decl, 'state, 'abort) Search.mapfolder

    val fold : {typ : Flat.typ' * 'state -> 'state,
                exp : Flat.exp' * 'state -> 'state,
                decl : Flat.decl' * 'state -> 'state}
               -> 'state -> Flat.decl -> 'state
end

structure File : sig
    datatype binder =
             NamedT of string * int * Flat.typ option
           | RelE of string * Flat.typ
           | NamedE of string * int * Flat.typ * Flat.exp option
           | F of int * string * Flat.typ * Flat.typ * Flat.exp

    val mapfoldB : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                    exp : ('typtext, Flat.exp', 'state, 'abort) Search.mapfolderB,
                    decl : ('typtext, Flat.decl', 'state, 'abort) Search.mapfolderB,
                    bind : 'typtext * binder -> 'typtext}
                   -> ('typtext, Flat.file, 'state, 'abort) Search.mapfolderB

    val mapfold : {typ : (Flat.typ', 'state, 'abort) Search.mapfolder,
                   exp : (Flat.exp', 'state, 'abort) Search.mapfolder,
                   decl : (Flat.decl', 'state, 'abort) Search.mapfolder}
                  -> (Flat.file, 'state, 'abort) Search.mapfolder

    val mapB : {typ : Flat.typ' -> Flat.typ',
                exp : 'typtext -> Flat.exp' -> Flat.exp',
                decl : 'typtext -> Flat.decl' -> Flat.decl',
                bind : 'typtext * binder -> 'typtext}
               -> 'typtext -> Flat.file -> Flat.file

    val fold : {typ : Flat.typ' * 'state -> 'state,
                exp : Flat.exp' * 'state -> 'state,
                decl : Flat.decl' * 'state -> 'state}
               -> 'state -> Flat.file -> 'state
end

end
