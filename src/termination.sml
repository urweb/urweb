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

structure Termination :> TERMINATION = struct

open Elab

structure E = ElabEnv
structure IM = IntBinaryMap
structure IS = IntBinarySet

datatype pedigree =
         Func of int
       | Arg of int * int * con
       | Subarg of int * int * con
       | Rabble

fun p2s p =
    case p of
        Func i => "Func" ^ Int.toString i
      | Arg (i, j, _) => "Arg" ^ Int.toString i ^ "," ^ Int.toString j
      | Subarg (i, j, _) => "Subarg" ^ Int.toString i ^ "," ^ Int.toString j
      | Rabble => "Rabble"

fun declOk' env (d, loc) =
    case d of
        DValRec vis =>
        let
            val nfns = length vis

            val fenv = ListUtil.foldli (fn (i, (_, j, _, _), fenv) => IM.insert (fenv, j, i)) IM.empty vis

            fun namesEq ((c1, _), (c2, _)) =
                case (c1, c2) of
                    (CName s1, CName s2) => s1 = s2
                  | (CRel n1, CRel n2) => n1 = n2
                  | (CNamed n1, CNamed n2) => n1 = n2
                  | (CModProj n1, CModProj n2) => n1 = n2
                  | _ => false

            fun patCon pc =
                let
                    fun unravel (t, _) =
                        case t of
                            TCFun (_, _, _, t) => unravel t
                          | TFun (dom, _) => dom
                          | _ => raise Fail "Termination: Unexpected constructor type"
                in
                    case pc of
                        PConVar i =>
                        let
                            val (_, t) = E.lookupENamed env i
                        in
                            unravel t
                        end
                      | PConProj (m1, ms, x) =>
                        let
                            val (str, sgn) = E.chaseMpath env (m1, ms)
                        in
                            case E.projectVal env {str = str, sgn = sgn, field = x} of
                                NONE => raise Fail "Termination: Bad constructor projection"
                              | SOME t => unravel t
                        end
                end

            fun pat penv (p, (pt, _)) =
                let
                    fun con (i, j, pc, pt') = pat penv (Subarg (i, j, patCon pc), pt')

                    fun record (i, j, t, xps) =
                        case t of
                            (TRecord (CRecord (_, xts), _), _) =>
                            foldl (fn ((x, pt', _), penv) =>
                                      let
                                          val p' =
                                              case List.find (fn (x', _) =>
                                                                 namesEq ((CName x, ErrorMsg.dummySpan), x')) xts of
                                                  NONE => Rabble
                                                | SOME (_, t) => Subarg (i, j, t)
                                      in
                                          pat penv (p', pt')
                                      end) penv xps
                          | _ => foldl (fn ((_, pt', _), penv) => pat penv (Rabble, pt')) penv xps
                in
                    case (p, pt) of
                        (_, PWild) => penv
                      | (_, PVar _) => p :: penv
                      | (_, PPrim _) => penv
                      | (_, PCon (_, _, _, NONE)) => penv
                      | (Arg (i, j, _), PCon (_, pc, _, SOME pt')) => con (i, j, pc, pt')
                      | (Subarg (i, j, _), PCon (_, pc, _, SOME pt')) => con (i, j, pc, pt')
                      | (_, PCon (_, _, _, SOME pt')) => pat penv (Rabble, pt')
                      | (Arg (i, j, t), PRecord xps) => record (i, j, t, xps)
                      | (Subarg (i, j, t), PRecord xps) => record (i, j, t, xps)
                      | (_, PRecord xps) => foldl (fn ((_, pt', _), penv) => pat penv (Rabble, pt')) penv xps
                end

            fun exp parent (penv, calls) e =
                let
                    val default = (Rabble, calls)

                    fun apps () =
                        let
                            fun combiner calls e =
                                case #1 e of
                                    EApp ((ECApp (
                                           (ECApp (
                                            (ECApp (
                                             (ECApp (
                                              (ECApp (
                                               (ECApp (
                                                (ECApp (
                                                 (ECApp (
                                                  (EModProj (m, [], "tag"), _),
                                                  _), _),
                                                 _), _),
                                                _), _),
                                               _), _),
                                              _), _),
                                             _), _),
                                            _), _),
                                           _), _),
                                          (ERecord xets, _)) =>
                                    let
                                        val checkName =
                                            case E.lookupStrNamed env m of
                                                  ("Basis", _) => (fn x : con => case #1 x of
                                                                                     CName s => s = "Link"
                                                                                                orelse s = "Action"
                                                                                   | _ => false)
                                                | _ => (fn _ => false)

                                        val calls = foldl (fn ((x, e, _), calls) =>
                                                              if checkName x then
                                                                  calls
                                                              else
                                                                  #2 (exp parent (penv, calls) e)) calls xets
                                    in
                                        (Rabble, [Rabble], calls)
                                    end

                                  | EApp (e1, e2) =>
                                    let
                                        val (p1, ps, calls) = combiner calls e1
                                        val (p2, calls) = exp parent (penv, calls) e2

                                        val p = case p1 of
                                                    Rabble => Rabble
                                                  | Arg _ => Rabble
                                                  | Subarg (i, j, (TFun (_, ran), _)) => Subarg (i, j, ran)
                                                  | Subarg _ => Rabble
                                                  | Func _ => Rabble
                                    in
                                        (p, ps @ [p2], calls)
                                    end
                                  | ECApp (e, _) =>
                                    let
                                        val (p, ps, calls) = combiner calls e

                                        val p = case p of
                                                    Rabble => Rabble
                                                  | Arg _ => Rabble
                                                  | Subarg (i, j, (TCFun (_, _, _, ran), _)) => Subarg (i, j, ran)
                                                  | Subarg _ => Rabble
                                                  | Func _ => Rabble
                                    in
                                        (p, ps, calls)
                                    end
                                  | EKApp (e, _) => combiner calls e
                                  | _ =>
                                    let
                                        val (p, calls) = exp parent (penv, calls) e
                                    in
                                        (*Print.prefaces "Head" [("e", ElabPrint.p_exp env e)];
                                        print (p2s p ^ "\n");*)
                                        (p, [p], calls)
                                    end

                            val (p, ps, calls) = combiner calls e

                            val calls =
                                case ps of
                                    [] => raise Fail "Termination: Empty ps"
                                  | f :: ps =>
                                    case f of
                                        Func i => (parent, i, ps) :: calls
                                      | _ => calls
                        in
                            (p, calls)
                        end
                in
                    case #1 e of
                        EPrim _ => default
                      | ERel n => (List.nth (penv, n), calls)
                      | ENamed n =>
                        let
                            val p = case IM.find (fenv, n) of
                                        NONE => Rabble
                                      | SOME n' => Func n'
                        in
                            (p, calls)
                        end
                      | EModProj _ => default

                      | EApp _ => apps ()
                      | EAbs (_, _, _, e) => 
                        let
                            val (_, calls) = exp parent (Rabble :: penv, calls) e
                        in
                            (Rabble, calls)
                        end
                      | ECApp _ => apps ()
                      | ECAbs (_, _, _, e) =>
                        let
                            val (_, calls) = exp parent (penv, calls) e
                        in
                            (Rabble, calls)
                        end
                      | EKApp _ => apps ()
                      | EKAbs (_, e) =>
                        let
                            val (_, calls) = exp parent (penv, calls) e
                        in
                            (Rabble, calls)
                        end

                      | ERecord xets =>
                        let
                            val calls = foldl (fn ((_, e, _), calls) => #2 (exp parent (penv, calls) e)) calls xets
                        in
                            (Rabble, calls)
                        end
                      | EField (e, x, _) =>
                        let
                            val (p, calls) = exp parent (penv, calls) e
                            val p =
                                case p of
                                    Subarg (i, j, (TRecord (CRecord (_, xts), _), _)) =>
                                    (case List.find (fn (x', _) => namesEq (x, x')) xts of
                                         NONE => Rabble
                                       | SOME (_, t) => Subarg (i, j, t))
                                  | _ => Rabble
                        in
                            (p, calls)
                        end
                      | ECut (e, _, _) =>
                        let
                            val (_, calls) = exp parent (penv, calls) e
                        in
                            (Rabble, calls)
                        end
                      | ECutMulti (e, _, _) =>
                        let
                            val (_, calls) = exp parent (penv, calls) e
                        in
                            (Rabble, calls)
                        end
                      | EConcat (e1, _, e2, _) =>
                        let
                            val (_, calls) = exp parent (penv, calls) e1
                            val (_, calls) = exp parent (penv, calls) e2
                        in
                            (Rabble, calls)
                        end

                      | ECase (e, pes, _) =>
                        let
                            val (p, calls) = exp parent (penv, calls) e

                            val calls = foldl (fn ((pt, e), calls) =>
                                                  let
                                                      val penv = pat penv (p, pt)
                                                      val (_, calls) = exp parent (penv, calls) e
                                                  in
                                                      calls
                                                  end) calls pes
                        in
                            (Rabble, calls)
                        end

                      | EError => (Rabble, calls)
                      | EUnif (ref (SOME e)) => exp parent (penv, calls) e
                      | EUnif (ref NONE) => (Rabble, calls)

                      | ELet (eds, e, _) =>
                        let
                            fun extPenv ((ed, _), penv) =
                                case ed of
                                    EDVal _ => Rabble :: penv
                                  | EDValRec vis => foldl (fn (_, penv) => Rabble :: penv) penv vis
                        in
                            exp parent (foldl extPenv penv eds, calls) e
                        end
                end

            fun doVali (i, (_, f, _, e), calls) =
                let
                    fun unravel (e, j, penv) =
                        case #1 e of
                            EAbs (_, t, _, e) =>
                            unravel (e, j + 1, Arg (i, j, t) :: penv)
                          | ECAbs (_, _, _, e) =>
                            unravel (e, j, penv)
                          | _ => (j, #2 (exp f (penv, calls) e))
                in
                    unravel (e, 0, [])
                end

            val (ns, calls) = ListUtil.foldliMap doVali [] vis

            fun isRecursive (from, to, _) =
                let
                    fun search (at, soFar) =
                        at = from
                        orelse List.exists (fn (from', to', _) =>
                                               from' = at
                                               andalso not (IS.member (soFar, to'))
                                               andalso search (to', IS.add (soFar, to')))
                                           calls
                in
                    search (to, IS.empty)
                end

            val calls = List.filter isRecursive calls

            fun search (ns, choices) =
                case ns of
                    [] =>
                    let
                        val choices = rev choices
                    in
                        List.all (fn (_, f, args) =>
                                     let
                                         val recArg = List.nth (choices, f)
                                                      
                                         fun isDatatype (t, _) =
                                             case t of
                                                 CNamed _ => true
                                               | CModProj _ => true
                                               | CApp (t, _) => isDatatype t
                                               | _ => false
                                     in
                                         length args > recArg andalso
                                         case List.nth (args, recArg) of
                                             Subarg (i, j, t) => isDatatype t andalso j = List.nth (choices, i)
                                           | _ => false
                                     end) calls
                    end
                  | n :: ns' =>
                    let
                        fun search' i =
                            i < n andalso (search (ns', i :: choices) orelse search' (i + 1))
                    in
                        search' 0
                    end
        in
            if search (ns, []) then
                ()
            else
                ErrorMsg.errorAt loc "Can't prove termination of recursive function(s)"
        end

      | DStr (_, _, _, (StrConst ds, _)) => ignore (foldl declOk env ds)

      | _ => ()

and declOk (d, env) =
    (declOk' env d;
     E.declBinds env d)

fun check ds = ignore (foldl declOk E.empty ds)

end
