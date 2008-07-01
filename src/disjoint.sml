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

structure Disjoint :> DISJOINT = struct

open Elab
open ElabOps

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

structure IS = IntBinarySet
structure IM = IntBinaryMap

type name_ineqs = {
     namesC : SS.set,
     namesR : IS.set,
     namesN : IS.set
}

val name_default = {
    namesC = SS.empty,
    namesR = IS.empty,
    namesN = IS.empty
}

type row_ineqs = {
     namesC : SS.set,
     namesR : IS.set,
     namesN : IS.set,
     rowsR : IS.set,
     rowsN : IS.set
}

val row_default = {
    namesC = SS.empty,
    namesR = IS.empty,
    namesN = IS.empty,
    rowsR = IS.empty,
    rowsN = IS.empty
}

fun nameToRow_ineqs {namesC, namesR, namesN} =
    {namesC = namesC,
     namesR = namesR,
     namesN = namesN,
     rowsR = IS.empty,
     rowsN = IS.empty}

type env = {
     namesR : name_ineqs IM.map,
     namesN : name_ineqs IM.map,
     rowsR : row_ineqs IM.map,
     rowsN : row_ineqs IM.map
}

val empty = {
    namesR = IM.empty,
    namesN = IM.empty,
    rowsR = IM.empty,
    rowsN = IM.empty
}

datatype piece =
         NameC of string
       | NameR of int
       | NameN of int
       | RowR of int
       | RowN of int
       | Unknown

fun nameToRow (c, loc) =
    (CRecord ((KUnit, loc), [((c, loc), (CUnit, loc))]), loc)

fun pieceToRow (p, loc) =
    case p of
        NameC s => nameToRow (CName s, loc)
      | NameR n => nameToRow (CRel n, loc)
      | NameN n => nameToRow (CNamed n, loc)
      | RowR n => (CRel n, loc)
      | RowN n => (CRel n, loc)
      | Unknown => raise Fail "Unknown to row"

fun decomposeRow env c =
    let
        fun decomposeName (c, acc) =
            case #1 (hnormCon env c) of
                CName s => NameC s :: acc
              | CRel n => NameR n :: acc
              | CNamed n => NameN n :: acc
              | _ => Unknown :: acc
                     
        fun decomposeRow (c, acc) =
            case #1 (hnormCon env c) of
                CRecord (_, xcs) => foldl (fn ((x, _), acc) => decomposeName (x, acc)) acc xcs
              | CConcat (c1, c2) => decomposeRow (c1, decomposeRow (c2, acc))
              | CRel n => RowR n :: acc
              | CNamed n => RowN n :: acc
              | _ => Unknown :: acc
    in
        decomposeRow (c, [])
    end

fun assertPiece_name (ps, ineqs : name_ineqs) =
    {namesC = foldl (fn (p', namesC) =>
                        case p' of
                            NameC s => SS.add (namesC, s)
                          | _ => namesC) (#namesC ineqs) ps,
     namesR = foldl (fn (p', namesR) =>
                        case p' of
                            NameR n => IS.add (namesR, n)
                          | _ => namesR) (#namesR ineqs) ps,
     namesN = foldl (fn (p', namesN) =>
                        case p' of
                            NameN n => IS.add (namesN, n)
                          | _ => namesN) (#namesN ineqs) ps}

fun assertPiece_row (ps, ineqs : row_ineqs) =
    {namesC = foldl (fn (p', namesC) =>
                        case p' of
                            NameC s => SS.add (namesC, s)
                          | _ => namesC) (#namesC ineqs) ps,
     namesR = foldl (fn (p', namesR) =>
                        case p' of
                            NameR n => IS.add (namesR, n)
                          | _ => namesR) (#namesR ineqs) ps,
     namesN = foldl (fn (p', namesN) =>
                        case p' of
                            NameN n => IS.add (namesN, n)
                          | _ => namesN) (#namesN ineqs) ps,
     rowsR = foldl (fn (p', rowsR) =>
                        case p' of
                            RowR n => IS.add (rowsR, n)
                          | _ => rowsR) (#rowsR ineqs) ps,
     rowsN = foldl (fn (p', rowsN) =>
                        case p' of
                            RowN n => IS.add (rowsN, n)
                          | _ => rowsN) (#rowsN ineqs) ps}

fun assertPiece ps (p, denv) =
    case p of
        Unknown => denv
      | NameC _ => denv

      | NameR n =>
        let
            val ineqs = Option.getOpt (IM.find (#namesR denv, n), name_default)
            val ineqs = assertPiece_name (ps, ineqs)
        in
            {namesR = IM.insert (#namesR denv, n, ineqs),
             namesN = #namesN denv,
             rowsR = #rowsR denv,
             rowsN = #rowsN denv}
        end

      | NameN n =>
        let
            val ineqs = Option.getOpt (IM.find (#namesN denv, n), name_default)
            val ineqs = assertPiece_name (ps, ineqs)
        in
            {namesR = #namesR denv,
             namesN = IM.insert (#namesN denv, n, ineqs),
             rowsR = #rowsR denv,
             rowsN = #rowsN denv}
        end

      | RowR n =>
        let
            val ineqs = Option.getOpt (IM.find (#rowsR denv, n), row_default)
            val ineqs = assertPiece_row (ps, ineqs)
        in
            {namesR = #namesR denv,
             namesN = #namesN denv,
             rowsR = IM.insert (#rowsR denv, n, ineqs),
             rowsN = #rowsN denv}
        end

      | RowN n =>
        let
            val ineqs = Option.getOpt (IM.find (#rowsN denv, n), row_default)
            val ineqs = assertPiece_row (ps, ineqs)
        in
            {namesR = #namesR denv,
             namesN = #namesN denv,
             rowsR = #rowsR denv,
             rowsN = IM.insert (#rowsN denv, n, ineqs)}
        end

fun assert env denv (c1, c2) =
    let
        val ps1 = decomposeRow env c1
        val ps2 = decomposeRow env c2

        val denv = foldl (assertPiece ps2) denv ps1
    in
        foldl (assertPiece ps1) denv ps2
    end

fun nameEnter {namesC, namesR, namesN} =
    {namesC = namesC,
     namesR = IS.map (fn n => n + 1) namesR,
     namesN = namesN}

fun rowEnter {namesC, namesR, namesN, rowsR, rowsN} =
    {namesC = namesC,
     namesR = IS.map (fn n => n + 1) namesR,
     namesN = namesN,
     rowsR = IS.map (fn n => n + 1) rowsR,
     rowsN = rowsN}

fun enter {namesR, namesN, rowsR, rowsN} =
    {namesR = IM.foldli (fn (n, ineqs, namesR) => IM.insert (namesR, n+1, nameEnter ineqs)) IM.empty namesR,
     namesN = IM.map nameEnter namesN,
     rowsR = IM.foldli (fn (n, ineqs, rowsR) => IM.insert (rowsR, n+1, rowEnter ineqs)) IM.empty rowsR,
     rowsN = IM.map rowEnter rowsN}

fun getIneqs (denv : env) p =
    case p of
        Unknown => raise Fail "getIneqs: Unknown"
      | NameC _ => raise Fail "getIneqs: NameC"
      | NameR n => nameToRow_ineqs (Option.getOpt (IM.find (#namesR denv, n), name_default))
      | NameN n => nameToRow_ineqs (Option.getOpt (IM.find (#namesN denv, n), name_default))
      | RowR n => Option.getOpt (IM.find (#rowsR denv, n), row_default)
      | RowN n => Option.getOpt (IM.find (#rowsN denv, n), row_default)

fun prove1' denv (p1, p2) =
    let
        val {namesC, namesR, namesN, rowsR, rowsN} = getIneqs denv p1
    in
        case p2 of
            Unknown => raise Fail "prove1': Unknown"
          | NameC s => SS.member (namesC, s)
          | NameR n => IS.member (namesR, n)
          | NameN n => IS.member (namesN, n)
          | RowR n => IS.member (rowsR, n)
          | RowN n => IS.member (rowsN, n)
    end

fun prove1 denv (p1, p2) =
    case (p1, p2) of
        (NameC s1, NameC s2) => s1 <> s2
      | (_, RowR _) => prove1' denv (p2, p1)
      | (_, RowN _) => prove1' denv (p2, p1)
      | _ => prove1' denv (p1, p2)

fun prove env denv (c1, c2, loc) =
    let
        val ps1 = decomposeRow env c1
        val ps2 = decomposeRow env c2

        val hasUnknown = List.exists (fn p => p = Unknown)
    in
        if hasUnknown ps1 orelse hasUnknown ps2 then
            (ErrorMsg.errorAt loc "Structure of row is too complicated to prove disjointness";
             Print.eprefaces' [("Row 1", ElabPrint.p_con env c1),
                               ("Row 2", ElabPrint.p_con env c2)];
             [])
        else
            foldl (fn (p1, rem) =>
                      foldl (fn (p2, rem) =>
                                if prove1 denv (p1, p2) then
                                    rem
                                else
                                    (pieceToRow (p1, loc), pieceToRow (p2, loc)) :: rem) rem ps2)
            [] ps1
    end

end
