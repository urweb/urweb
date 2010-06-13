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

signature EXPL_ENV = sig

    exception SynUnif
    val liftConInCon : int -> Expl.con -> Expl.con

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

    val pushCRel : env -> string -> Expl.kind -> env
    val lookupCRel : env -> int -> string * Expl.kind

    val pushCNamed : env -> string -> int -> Expl.kind -> Expl.con option -> env
    val lookupCNamed : env -> int -> string * Expl.kind * Expl.con option

    val pushERel : env -> string -> Expl.con -> env
    val lookupERel : env -> int -> string * Expl.con

    val pushENamed : env -> string -> int -> Expl.con -> env
    val lookupENamed : env -> int -> string * Expl.con

    val pushSgnNamed : env -> string -> int -> Expl.sgn -> env
    val lookupSgnNamed : env -> int -> string * Expl.sgn

    val pushStrNamed : env -> string -> int -> Expl.sgn -> env
    val lookupStrNamed : env -> int -> string * Expl.sgn

    val declBinds : env -> Expl.decl -> env
    val sgiBinds : env -> Expl.sgn_item -> env

    val patBinds : env -> Expl.pat -> env

end
