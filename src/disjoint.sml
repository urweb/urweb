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

datatype piece_fst =
         NameC of string
       | NameR of int
       | NameN of int
       | NameM of int * string list * string
       | RowR of int
       | RowN of int
       | RowM of int * string list * string

type piece = piece_fst * int list

fun p2s p =
    case p of
        NameC s => "NameC(" ^ s ^ ")"
      | NameR n => "NameR(" ^ Int.toString n ^ ")"
      | NameN n => "NameN(" ^ Int.toString n ^ ")"
      | NameM (n, _, s) => "NameR(" ^ Int.toString n ^ ", " ^ s ^ ")"
      | RowR n => "RowR(" ^ Int.toString n ^ ")"
      | RowN n => "RowN(" ^ Int.toString n ^ ")"
      | RowM (n, _, s) => "RowR(" ^ Int.toString n ^ ", " ^ s ^ ")"

fun pp p = print (p2s p ^ "\n")

fun rp2s (p, ns) = String.concatWith " " (p2s p :: map Int.toString ns)

structure PK = struct

type ord_key = piece

open Order

fun compare' (p1, p2) =
    case (p1, p2) of
        (NameC s1, NameC s2) => String.compare (s1, s2)
      | (NameR n1, NameR n2) => Int.compare (n1, n2)
      | (NameN n1, NameN n2) => Int.compare (n1, n2)
      | (NameM (n1, ss1, s1), NameM (n2, ss2, s2)) =>
        join (Int.compare (n1, n2),
           fn () => join (String.compare (s1, s2), fn () =>
                                                      joinL String.compare (ss1, ss2)))
      | (RowR n1, RowR n2) => Int.compare (n1, n2)
      | (RowN n1, RowN n2) => Int.compare (n1, n2)
      | (RowM (n1, ss1, s1), RowM (n2, ss2, s2)) =>
        join (Int.compare (n1, n2),
           fn () => join (String.compare (s1, s2), fn () =>
                                                      joinL String.compare (ss1, ss2)))

      | (NameC _, _) => LESS
      | (_, NameC _) => GREATER

      | (NameR _, _) => LESS
      | (_, NameR _) => GREATER

      | (NameN _, _) => LESS
      | (_, NameN _) => GREATER

      | (NameM _, _) => LESS
      | (_, NameM _) => GREATER

      | (RowR _, _) => LESS
      | (_, RowR _) => GREATER

      | (RowN _, _) => LESS
      | (_, RowN _) => GREATER

fun compare ((p1, ns1), (p2, ns2)) =
    join (compare' (p1, p2),
          fn () => joinL Int.compare (ns1, ns2))

end

structure PS = BinarySetFn(PK)
structure PM = BinaryMapFn(PK)

type env = PS.set PM.map

fun p_env x =
    (print "\nDENV:\n";
     PM.appi (fn (p1, ps) =>
                 PS.app (fn p2 =>
                            print (rp2s p1 ^ " ~ " ^ rp2s p2 ^ "\n")) ps) x)
     
structure E = ElabEnv

type goal = ErrorMsg.span * E.env * env * Elab.con * Elab.con

val empty = PM.empty

fun nameToRow (c, loc) =
    (CRecord ((KUnit, loc), [((c, loc), (CUnit, loc))]), loc)

fun pieceToRow' (p, loc) =
    case p of
        NameC s => nameToRow (CName s, loc)
      | NameR n => nameToRow (CRel n, loc)
      | NameN n => nameToRow (CNamed n, loc)
      | NameM (n, xs, x) => nameToRow (CModProj (n, xs, x), loc)
      | RowR n => (CRel n, loc)
      | RowN n => (CNamed n, loc)
      | RowM (n, xs, x) => (CModProj (n, xs, x), loc)

fun pieceToRow ((p, ns), loc) =
    foldl (fn (n, c) => (CProj (c, n), loc)) (pieceToRow' (p, loc)) ns

datatype piece' =
         Piece of piece
       | Unknown of con

fun pieceEnter' p =
    case p of
        NameR n => NameR (n + 1)
      | RowR n => RowR (n + 1)
      | _ => p

fun pieceEnter (p, n) = (pieceEnter' p, n)

fun enter denv =
    PM.foldli (fn (p, pset, denv') =>
                  PM.insert (denv', pieceEnter p, PS.map pieceEnter pset))
    PM.empty denv

val lowercase = CharVector.map Char.toLower

fun prove1 denv (p1, p2) =
    case (p1, p2) of
        ((NameC s1, _), (NameC s2, _)) => lowercase s1 <> lowercase s2
      | _ =>
        case PM.find (denv, p1) of
            NONE => false
          | SOME pset => PS.member (pset, p2)

val proved = ref 0
fun reset () = (ElabOps.reset ();
                proved := 0)

fun decomposeRow env c =
    let
        val loc = #2 c

        fun decomposeProj c =
            let
                val c = hnormCon env c
            in
                case #1 c of
                    CProj (c, n) =>
                    let
                        val (c', ns) = decomposeProj c
                    in
                        (c', ns @ [n])
                    end
                  | _ => (c, [])
            end

        fun decomposeName (c, acc) =
            let
                val (cAll as (c, _), ns) = decomposeProj c
            in
                case c of
                    CName s => Piece (NameC s, ns) :: acc
                  | CRel n => Piece (NameR n, ns) :: acc
                  | CNamed n => Piece (NameN n, ns) :: acc
                  | CModProj (m1, ms, x) => Piece (NameM (m1, ms, x), ns) :: acc
                  | _ => Unknown cAll :: acc
            end

        fun decomposeRow' (c, acc) =
            let
                fun default () =
                    let
                        val (cAll as (c, _), ns) = decomposeProj c
                    in
                        case c of
                            CRecord (_, xcs) => foldl (fn ((x, _), acc) => decomposeName (x, acc)) acc xcs
                          | CConcat (c1, c2) => decomposeRow' (c1, decomposeRow' (c2, acc))
                          | CRel n => Piece (RowR n, ns) :: acc
                          | CNamed n => Piece (RowN n, ns) :: acc
                          | CModProj (m1, ms, x) => Piece (RowM (m1, ms, x), ns) :: acc
                          | _ => Unknown cAll :: acc
                    end
            in
                case #1 (hnormCon env c) of
                    CApp (
                    (CApp ((CMap _, _), _), _),
                    r) => decomposeRow' (r, acc)
                  | _ => default ()
            end
    in
        decomposeRow' (c, [])
    end

and assert env denv (c1, c2) =
    let
        val ps1 = decomposeRow env c1
        val ps2 = decomposeRow env c2

        val unUnknown = List.mapPartial (fn Unknown _ => NONE | Piece p => SOME p)
        val ps1 = unUnknown ps1
        val ps2 = unUnknown ps2

        (*val () = print "APieces1:\n"
        val () = app pp ps1
        val () = print "APieces2:\n"
        val () = app pp ps2*)

        fun assertPiece ps (p, denv) =
            let
                val pset = Option.getOpt (PM.find (denv, p), PS.empty)
                val ps = case p of
                             (NameC _, _) => List.filter (fn (NameC _, _) => false | _ => true) ps
                           | _ => ps
                val pset = PS.addList (pset, ps)
            in
                PM.insert (denv, p, pset)
            end

        val denv = foldl (assertPiece ps2) denv ps1
    in
        foldl (assertPiece ps1) denv ps2
    end

and prove env denv (c1, c2, loc) =
    let
        val () = proved := !proved + 1
        val ps1 = decomposeRow env c1
        val ps2 = decomposeRow env c2

        val hasUnknown = List.exists (fn Unknown _ => true | _ => false)
        val unUnknown = List.mapPartial (fn Unknown _ => NONE | Piece p => SOME p)
    in
        if (hasUnknown ps1 andalso not (List.null ps2))
           orelse (hasUnknown ps2 andalso not (List.null ps1)) then
            [(loc, env, denv, c1, c2)]
        else
            let
                val ps1 = unUnknown ps1
                val ps2 = unUnknown ps2
            in
                (*print "Pieces1:\n";
                app pp ps1;
                print "Pieces2:\n";
                app pp ps2;*)

                foldl (fn (p1, rem) =>
                          foldl (fn (p2, rem) =>
                                    if prove1 denv (p1, p2) then
                                        rem
                                    else
                                        (loc, env, denv, pieceToRow (p1, loc), pieceToRow (p2, loc)) :: rem) rem ps2)
                      [] ps1
            end
    end

end
