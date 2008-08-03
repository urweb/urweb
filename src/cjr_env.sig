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

signature CJR_ENV = sig

    type env

    val empty : env

    exception UnboundRel of int
    exception UnboundNamed of int
    exception UnboundF of int
    exception UnboundStruct of int

    val pushDatatype : env -> string -> int -> (string * int * Cjr.typ option) list -> env
    val lookupDatatype : env -> int -> string * (string * int * Cjr.typ option) list

    val lookupConstructor : env -> int -> string * Cjr.typ option * int

    val pushERel : env -> string -> Cjr.typ -> env
    val lookupERel : env -> int -> string * Cjr.typ
    val listERels : env -> (string * Cjr.typ) list
    val countERels : env -> int

    val pushENamed : env -> string -> int -> Cjr.typ -> env
    val lookupENamed : env -> int -> string * Cjr.typ

    val pushStruct : env -> int -> (string * Cjr.typ) list -> env
    val lookupStruct : env -> int -> (string * Cjr.typ) list

    val declBinds : env -> Cjr.decl -> env

    val classifyDatatype : (string * int * Cjr.typ option) list -> Cjr.datatype_kind
                                                 
end
