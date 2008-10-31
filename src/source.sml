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

withtype kind = kind' located

datatype explicitness =
         Explicit
       | Implicit

datatype con' =
         CAnnot of con * kind

       | TFun of con * con
       | TCFun of explicitness * string * kind * con
       | TRecord of con

       | CVar of string list * string
       | CApp of con * con
       | CAbs of string * kind option * con
       | CDisjoint of con * con * con

       | CName of string

       | CRecord of (con * con) list
       | CConcat of con * con
       | CFold

       | CUnit

       | CTuple of con list
       | CProj of con * int

       | CWild of kind

withtype con = con' located

datatype sgn_item' =
         SgiConAbs of string * kind
       | SgiCon of string * kind option * con
       | SgiDatatype of string * string list * (string * con option) list
       | SgiDatatypeImp of string * string list * string
       | SgiVal of string * con
       | SgiStr of string * sgn
       | SgiSgn of string * sgn
       | SgiInclude of sgn
       | SgiConstraint of con * con
       | SgiTable of string * con
       | SgiSequence of string
       | SgiClassAbs of string
       | SgiClass of string * con

and sgn' =
    SgnConst of sgn_item list
  | SgnVar of string
  | SgnFun of string * sgn * sgn
  | SgnWhere of sgn * string * con
  | SgnProj of string * string list * string

withtype sgn_item = sgn_item' located
and sgn = sgn' located

datatype pat' =
         PWild
       | PVar of string
       | PPrim of Prim.t
       | PCon of string list * string * pat option
       | PRecord of (string * pat) list * bool

withtype pat = pat' located

datatype inference =
         Infer
       | DontInfer
       | TypesOnly

datatype exp' =
         EAnnot of exp * con

       | EPrim of Prim.t
       | EVar of string list * string * inference
       | EApp of exp * exp
       | EAbs of string * con option * exp
       | ECApp of exp * con
       | ECAbs of explicitness * string * kind * exp
       | EDisjoint of con * con * exp

       | ERecord of (con * exp) list
       | EField of exp * con
       | EConcat of exp * exp
       | ECut of exp * con
       | EFold

       | EWild

       | ECase of exp * (pat * exp) list

withtype exp = exp' located

datatype decl' =
         DCon of string * kind option * con
       | DDatatype of string * string list * (string * con option) list
       | DDatatypeImp of string * string list * string
       | DVal of string * con option * exp
       | DValRec of (string * con option * exp) list
       | DSgn of string * sgn
       | DStr of string * sgn option * str
       | DFfiStr of string * sgn
       | DOpen of string * string list
       | DConstraint of con * con
       | DOpenConstraints of string * string list
       | DExport of str
       | DTable of string * con
       | DSequence of string
       | DClass of string * con
       | DDatabase of string

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
