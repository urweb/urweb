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

    type env

    val empty : env

    exception UnboundRel of int
    exception UnboundNamed of int

    datatype 'a var =
             NotBound
           | Rel of int * 'a
           | Named of int * 'a

    val pushCRel : env -> string -> Elab.kind -> env
    val lookupCRel : env -> int -> string * Elab.kind

    val pushCNamed : env -> string -> Elab.kind -> Elab.con option -> env * int
    val pushCNamedAs : env -> string -> int -> Elab.kind -> Elab.con option -> env
    val lookupCNamed : env -> int -> string * Elab.kind * Elab.con option

    val lookupC : env -> string -> Elab.kind var

    val pushERel : env -> string -> Elab.con -> env
    val lookupERel : env -> int -> string * Elab.con

    val pushENamed : env -> string -> Elab.con -> env * int
    val pushENamedAs : env -> string -> int -> Elab.con -> env
    val lookupENamed : env -> int -> string * Elab.con
                                                 
    val lookupE : env -> string -> Elab.con var

end
