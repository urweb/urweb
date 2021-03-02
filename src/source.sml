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

structure Source = struct

type 'a located = 'a ErrorMsg.located

datatype kind' =
         KType
       | KArrow of kind * kind
       | KName
       | KRecord of kind
       | KUnit
       | KTuple of kind list
       | KWild

       | KFun of string * kind
       | KVar of string

withtype kind = kind' located

datatype explicitness =
         Explicit
       | Implicit

datatype con' =
         CAnnot of con * kind

       | TFun of con * con
       | TCFun of explicitness * string * kind * con
       | TRecord of con
       | TDisjoint of con * con * con

       | CVar of string list * string
       | CApp of con * con
       | CAbs of string * kind option * con

       | CKAbs of string * con
       | TKFun of string * con

       | CName of string

       | CRecord of (con * con) list
       | CConcat of con * con
       | CMap

       | CUnit

       | CTuple of con list
       | CProj of con * int

       | CWild of kind

withtype con = con' located

datatype inference =
         Infer
       | DontInfer
       | TypesOnly

datatype sgn_item' =
         SgiConAbs of string * kind
       | SgiCon of string * kind option * con
       | SgiDatatype of (string * string list * (string * con option) list) list
       | SgiDatatypeImp of string * string list * string
       | SgiVal of string * con
       | SgiTable of string * con * exp * exp
       | SgiStr of string * sgn
       | SgiSgn of string * sgn
       | SgiInclude of sgn
       | SgiConstraint of con * con
       | SgiClassAbs of string * kind
       | SgiClass of string * kind * con

and sgn' =
    SgnConst of sgn_item list
  | SgnVar of string
  | SgnFun of string * sgn * sgn
  | SgnWhere of sgn * string list * string * con
  | SgnProj of string * string list * string

and pat' =
    PVar of string
  | PPrim of Prim.t
  | PCon of string list * string * pat option
  | PRecord of (string * pat) list * bool
  | PAnnot of pat * con

and exp' =
    EAnnot of exp * con

  | EPrim of Prim.t
  | EVar of string list * string * inference
  | EApp of exp * exp
  | EAbs of string * con option * exp
  | ECApp of exp * con
  | ECAbs of explicitness * string * kind * exp
  | EDisjoint of con * con * exp
  | EDisjointApp of exp

  | EKAbs of string * exp

  | ERecord of (con * exp) list * bool
  | EField of exp * con
  | EConcat of exp * exp
  | ECut of exp * con
  | ECutMulti of exp * con

  | EWild

  | ECase of exp * (pat * exp) list

  | ELet of edecl list * exp

and edecl' =
    EDVal of pat * exp
  | EDValRec of (string * con option * exp) list

withtype sgn_item = sgn_item' located
and sgn = sgn' located
and pat = pat' located
and exp = exp' located
and edecl = edecl' located

datatype ffi_mode =
         Effectful
       | BenignEffectful
       | ClientOnly
       | ServerOnly
       | JsFunc of string

datatype decl' =
         DCon of string * kind option * con
       | DDatatype of (string * string list * (string * con option) list) list
       | DDatatypeImp of string * string list * string
       | DVal of pat * exp
       | DValRec of (string * con option * exp) list
       | DSgn of string * sgn
       | DStr of string * sgn option * Time.time option * str * bool (* did this module come from the '-root' directive? *)
       | DFfiStr of string * sgn * Time.time option
       | DOpen of string * string list
       | DConstraint of con * con
       | DOpenConstraints of string * string list
       | DExport of str
       | DTable of string * con * exp * exp
       | DSequence of string
       | DView of string * exp
       | DIndex of exp (* table *) * exp (* record assigning columns to their indexing modes *)
                   * con option (* row for which columns are included *)
       | DDatabase of string
       | DCookie of string * con
       | DStyle of string
       | DTask of exp * exp
       | DPolicy of exp
       | DOnError of string * string list * string
       | DFfi of string * ffi_mode list * con

     and str' =
         StrConst of decl list
       | StrVar of string
       | StrProj of str * string
       | StrFun of string * sgn * sgn option * str
       | StrApp of str * str

withtype decl = decl' located
     and str = str' located

type file = decl list

end
