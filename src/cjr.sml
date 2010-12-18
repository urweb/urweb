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

structure Cjr = struct

type 'a located = 'a ErrorMsg.located

datatype datatype_kind = datatype DatatypeKind.datatype_kind

datatype typ' =
         TFun of typ * typ
       | TRecord of int
       | TDatatype of datatype_kind * int * (string * int * typ option) list ref
       | TFfi of string * string
       | TOption of typ
       | TList of typ * int

withtype typ = typ' located

datatype patCon =
         PConVar of int
       | PConFfi of {mod : string, datatyp : string, con : string, arg : typ option}

datatype pat' =
         PWild
       | PVar of string * typ
       | PPrim of Prim.t
       | PCon of datatype_kind * patCon * pat option
       | PRecord of (string * pat * typ) list
       | PNone of typ
       | PSome of typ * pat

withtype pat = pat' located

datatype failure_mode = datatype Settings.failure_mode

datatype exp' =
         EPrim of Prim.t
       | ERel of int
       | ENamed of int
       | ECon of datatype_kind * patCon * exp option
       | ENone of typ
       | ESome of typ * exp
       | EFfi of string * string
       | EFfiApp of string * string * exp list
       | EApp of exp * exp list

       | EUnop of string * exp
       | EBinop of string * exp * exp

       | ERecord of int * (string * exp) list
       | EField of exp * string

       | ECase of exp * (pat * exp) list * { disc : typ, result : typ }

       | EError of exp * typ
       | EReturnBlob of {blob : exp, mimeType : exp, t : typ}
       | ERedirect of exp * typ

       | EWrite of exp
       | ESeq of exp * exp
       | ELet of string * typ * exp * exp

       | EQuery of { exps : (string * typ) list,
                     tables : (string * (string * typ) list) list,
                     rnum : int,
                     state : typ,
                     query : exp,
                     body : exp,
                     initial : exp,
                     prepared : {id : int, query : string, nested : bool} option }
       | EDml of { dml : exp,
                   prepared : {id : int, dml : string} option,
                   mode : failure_mode }
       | ENextval of { seq : exp,
                       prepared : {id : int, query : string} option }
       | ESetval of { seq : exp, count : exp }
       | EUnurlify of exp * typ * bool

withtype exp = exp' located

datatype task = Initialize | ClientLeaves | Periodic of Int64.int

datatype decl' =
         DStruct of int * (string * typ) list
       | DDatatype of (datatype_kind * string * int * (string * int * typ option) list) list
       | DDatatypeForward of datatype_kind * string * int
       | DVal of string * int * typ * exp
       | DFun of string * int * (string * typ) list * typ * exp
       | DFunRec of (string * int * (string * typ) list * typ * exp) list

       | DTable of string * (string * typ) list * string * (string * string) list
       | DSequence of string
       | DView of string * (string * typ) list * string
       | DDatabase of {name : string, expunge : int, initialize : int}
       | DPreparedStatements of (string * int) list

       | DJavaScript of string
       | DCookie of string
       | DStyle of string

       | DTask of task * string (* first arg name *) * string * exp
       | DOnError of int

withtype decl = decl' located

datatype sidedness =
         ServerOnly
       | ServerAndPull
       | ServerAndPullAndPush

datatype effect = datatype Export.effect
datatype export_kind = datatype Export.export_kind

type file = decl list * (export_kind * string * int * typ list * typ * sidedness * bool) list

end
