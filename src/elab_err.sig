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

signature ELAB_ERR = sig

    datatype kind_error =
             UnboundKind of ErrorMsg.span * string

    val kindError : ElabEnv.env -> kind_error -> unit

    datatype kunify_error =
             KOccursCheckFailed of Elab.kind * Elab.kind
           | KIncompatible of Elab.kind * Elab.kind

    val kunifyError : ElabEnv.env -> kunify_error -> unit

    datatype con_error =
             UnboundCon of ErrorMsg.span * string
           | UnboundDatatype of ErrorMsg.span * string
           | UnboundStrInCon of ErrorMsg.span * string
           | WrongKind of Elab.con * Elab.kind * Elab.kind * kunify_error
           | DuplicateField of ErrorMsg.span * string
           | ProjBounds of Elab.con * int
           | ProjMismatch of Elab.con * Elab.kind

    val conError : ElabEnv.env -> con_error -> unit

    datatype cunify_error =
             CKind of Elab.kind * Elab.kind * kunify_error
           | COccursCheckFailed of Elab.con * Elab.con
           | CIncompatible of Elab.con * Elab.con
           | CExplicitness of Elab.con * Elab.con
           | CKindof of Elab.kind * Elab.con * string
           | CRecordFailure of Elab.con * Elab.con * (Elab.con * Elab.con * Elab.con * cunify_error option) option
           | TooLifty of ErrorMsg.span * ErrorMsg.span
           | TooUnify of Elab.con * Elab.con
           | TooDeep

    val cunifyError : ElabEnv.env -> cunify_error -> unit

    datatype exp_error =
             UnboundExp of ErrorMsg.span * string
           | UnboundStrInExp of ErrorMsg.span * string
           | Unify of Elab.exp * Elab.con * Elab.con * cunify_error
           | Unif of string * ErrorMsg.span * Elab.con
           | WrongForm of string * Elab.exp * Elab.con
           | IncompatibleCons of Elab.con * Elab.con
           | DuplicatePatternVariable of ErrorMsg.span * string
           | PatUnify of Elab.pat * Elab.con * Elab.con * cunify_error
           | UnboundConstructor of ErrorMsg.span * string list * string
           | PatHasArg of ErrorMsg.span
           | PatHasNoArg of ErrorMsg.span
           | Inexhaustive of ErrorMsg.span * Elab.pat
           | DuplicatePatField of ErrorMsg.span * string
           | Unresolvable of ErrorMsg.span * Elab.con
           | OutOfContext of ErrorMsg.span * (Elab.exp * Elab.con) option
           | IllegalRec of string * Elab.exp

    val expError : ElabEnv.env -> exp_error -> unit

    datatype decl_error =
             KunifsRemain of Elab.decl list
           | CunifsRemain of Elab.decl list
           | Nonpositive of Elab.decl

    val declError : ElabEnv.env -> decl_error -> unit

    datatype sgn_error =
             UnboundSgn of ErrorMsg.span * string
           | UnmatchedSgi of ErrorMsg.span * Elab.sgn_item
           | SgiWrongKind of ErrorMsg.span * Elab.sgn_item * Elab.kind * Elab.sgn_item * Elab.kind * kunify_error
           | SgiWrongCon of ErrorMsg.span * Elab.sgn_item * Elab.con * Elab.sgn_item * Elab.con * cunify_error
           | SgiMismatchedDatatypes of ErrorMsg.span * Elab.sgn_item * Elab.sgn_item
                                       * (Elab.con * Elab.con * cunify_error) option
           | SgnWrongForm of ErrorMsg.span * Elab.sgn * Elab.sgn
           | UnWhereable of Elab.sgn * string
           | WhereWrongKind of Elab.kind * Elab.kind * kunify_error
           | NotIncludable of Elab.sgn
           | DuplicateCon of ErrorMsg.span * string
           | DuplicateVal of ErrorMsg.span * string
           | DuplicateSgn of ErrorMsg.span * string
           | DuplicateStr of ErrorMsg.span * string
           | NotConstraintsable of Elab.sgn

    val sgnError : ElabEnv.env -> sgn_error -> unit

    datatype str_error =
             UnboundStr of ErrorMsg.span * string
           | NotFunctor of Elab.sgn
           | FunctorRebind of ErrorMsg.span
           | UnOpenable of Elab.sgn
           | NotType of ErrorMsg.span * Elab.kind * (Elab.kind * Elab.kind * kunify_error)
           | DuplicateConstructor of string * ErrorMsg.span
           | NotDatatype of ErrorMsg.span

    val strError : ElabEnv.env -> str_error -> unit

end
