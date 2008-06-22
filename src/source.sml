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
       | CAbs of string * kind * con

       | CName of string

       | CRecord of (con * con) list
       | CConcat of con * con

       | CWild of kind

withtype con = con' located

datatype sgn_item' =
         SgiConAbs of string * kind
       | SgiCon of string * kind option * con
       | SgiVal of string * con
       | SgiStr of string * sgn
       | SgiSgn of string * sgn
       | SgiInclude of sgn

and sgn' =
    SgnConst of sgn_item list
  | SgnVar of string
  | SgnFun of string * sgn * sgn
  | SgnWhere of sgn * string * con
  | SgnProj of string * string list * string

withtype sgn_item = sgn_item' located
and sgn = sgn' located

datatype exp' =
         EAnnot of exp * con

       | EPrim of Prim.t
       | EVar of string list * string
       | EApp of exp * exp
       | EAbs of string * con option * exp
       | ECApp of exp * con
       | ECAbs of explicitness * string * kind * exp

       | ERecord of (con * exp) list
       | EField of exp * con

withtype exp = exp' located

datatype decl' =
         DCon of string * kind option * con
       | DVal of string * con option * exp
       | DSgn of string * sgn
       | DStr of string * sgn option * str
       | DFfiStr of string * sgn
       | DOpen of string * string list

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
