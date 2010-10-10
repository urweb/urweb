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

signature ELAB_ENV = sig

    val liftConInCon : int -> Elab.con -> Elab.con
    val mliftConInCon : int -> Elab.con -> Elab.con

    val liftConInExp : int -> Elab.exp -> Elab.exp
    val liftExpInExp : int -> Elab.exp -> Elab.exp

    val subExpInExp : (int * Elab.exp) -> Elab.exp -> Elab.exp

    type env

    val empty : env

    exception UnboundRel of int
    exception UnboundNamed of int

    datatype 'a var =
             NotBound
           | Rel of int * 'a
           | Named of int * 'a

    val pushKRel : env -> string -> env
    val lookupKRel : env -> int -> string
    val lookupK : env -> string -> int option

    val pushCRel : env -> string -> Elab.kind -> env
    val lookupCRel : env -> int -> string * Elab.kind

    val pushCNamed : env -> string -> Elab.kind -> Elab.con option -> env * int
    val pushCNamedAs : env -> string -> int -> Elab.kind -> Elab.con option -> env
    val lookupCNamed : env -> int -> string * Elab.kind * Elab.con option

    val lookupC : env -> string -> Elab.kind var

    val pushDatatype : env -> int -> string list -> (string * int * Elab.con option) list -> env
    type datatyp
    val lookupDatatype : env -> int -> datatyp
    val lookupDatatypeConstructor : datatyp -> int -> string * Elab.con option
    val datatypeArgs : datatyp -> string list
    val constructors : datatyp -> (string * int * Elab.con option) list

    val lookupConstructor : env -> string -> (Elab.datatype_kind * int * string list * Elab.con option * int) option

    val pushClass : env -> int -> env
    val isClass : env -> Elab.con -> bool
    val resolveClass : (Elab.con -> Elab.con) -> (Elab.con * Elab.con -> bool)
                       -> env -> Elab.con -> Elab.exp option
    val listClasses : env -> (Elab.con * (Elab.con * Elab.exp) list) list

    val pushERel : env -> string -> Elab.con -> env
    val lookupERel : env -> int -> string * Elab.con

    val pushENamed : env -> string -> Elab.con -> env * int
    val pushENamedAs : env -> string -> int -> Elab.con -> env
    val lookupENamed : env -> int -> string * Elab.con
    val checkENamed : env -> int -> bool

    val lookupE : env -> string -> Elab.con var

    val pushSgnNamed : env -> string -> Elab.sgn -> env * int
    val pushSgnNamedAs : env -> string -> int -> Elab.sgn -> env
    val lookupSgnNamed : env -> int -> string * Elab.sgn

    val lookupSgn : env -> string -> (int * Elab.sgn) option

    val pushStrNamed : env -> string -> Elab.sgn -> env * int
    val pushStrNamedAs : env -> string -> int -> Elab.sgn -> env
    val lookupStrNamed : env -> int -> string * Elab.sgn

    val lookupStr : env -> string -> (int * Elab.sgn) option

    val edeclBinds : env -> Elab.edecl -> env
    val declBinds : env -> Elab.decl -> env
    val sgiBinds : env -> Elab.sgn_item -> env

    val hnormSgn : env -> Elab.sgn -> Elab.sgn

    val projectCon : env -> { sgn : Elab.sgn, str : Elab.str, field : string } -> (Elab.kind * Elab.con option) option
    val projectDatatype : env -> { sgn : Elab.sgn, str : Elab.str, field : string }
                          -> (string list * (string * int * Elab.con option) list) option
    val projectConstructor : env -> { sgn : Elab.sgn, str : Elab.str, field : string }
                             -> (Elab.datatype_kind * int * string list * Elab.con option * Elab.con) option
    val projectVal : env -> { sgn : Elab.sgn, str : Elab.str, field : string } -> Elab.con option
    val projectSgn : env -> { sgn : Elab.sgn, str : Elab.str, field : string } -> Elab.sgn option
    val projectStr : env -> { sgn : Elab.sgn, str : Elab.str, field : string } -> Elab.sgn option
    val projectConstraints : env -> { sgn : Elab.sgn, str : Elab.str } -> (Elab.con * Elab.con) list option

    val newNamed : unit -> int

    val chaseMpath : env -> (int * string list) -> Elab.str * Elab.sgn

    val patBinds : env -> Elab.pat -> env
    val patBindsN : Elab.pat -> int

    exception Bad of Elab.con * Elab.con

end
