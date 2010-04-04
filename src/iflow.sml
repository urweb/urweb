(* Copyright (c) 2010, Adam Chlipala
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

structure Iflow :> IFLOW = struct

open Mono

structure SS = BinarySetFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

val writers = ["htmlifyInt_w",
               "htmlifyFloat_w",
               "htmlifyString_w",
               "htmlifyBool_w",
               "htmlifyTime_w",
               "attrifyInt_w",
               "attrifyFloat_w",
               "attrifyString_w",
               "attrifyChar_w",
               "urlifyInt_w",
               "urlifyFloat_w",
               "urlifyString_w",
               "urlifyBool_w"]

val writers = SS.addList (SS.empty, writers)

type lvar = int

datatype exp =
         Const of Prim.t
       | Var of int
       | Lvar of lvar
       | Func of string * exp list
       | Recd of (string * exp) list
       | Proj of exp * string
       | Finish

datatype reln =
         Sql of string
       | Eq

datatype prop =
         True
       | False
       | Unknown
       | And of prop * prop
       | Or of prop * prop
       | Reln of reln * exp list
       | Select of int * lvar * lvar * prop * exp

local
    val count = ref 0
in
fun newLvar () =
    let
        val n = !count
    in
        count := n + 1;
        n
    end
end

fun subExp (v, lv) =
    let
        fun sub e =
            case e of
                Const _ => e
              | Var v' => if v' = v then Lvar lv else e
              | Lvar _ => e
              | Func (f, es) => Func (f, map sub es)
              | Recd xes => Recd (map (fn (x, e) => (x, sub e)) xes)
              | Proj (e, s) => Proj (sub e, s)
              | Finish => Finish
    in
        sub
    end

fun subProp (v, lv) =
    let
        fun sub p =
            case p of
                True => p
              | False => p
              | Unknown => p
              | And (p1, p2) => And (sub p1, sub p2)
              | Or (p1, p2) => Or (sub p1, sub p2)
              | Reln (r, es) => Reln (r, map (subExp (v, lv)) es)
              | Select (v1, lv1, lv2, p, e) => Select (v1, lv1, lv2, sub p, subExp (v, lv) e)
    in
        sub
    end

fun eq' (e1, e2) =
    case (e1, e2) of
        (Const p1, Const p2) => Prim.equal (p1, p2)
      | (Var n1, Var n2) => n1 = n2
      | (Lvar n1, Lvar n2) => n1 = n2
      | (Func (f1, es1), Func (f2, es2)) => f1 = f2 andalso ListPair.allEq eq' (es1, es2)
      | (Recd xes1, Recd xes2) => ListPair.allEq (fn ((x1, e1), (x2, e2)) => x1 = x2 andalso eq' (e1, e2)) (xes1, xes2)
      | (Proj (e1, s1), Proj (e2, s2)) => eq' (e1, e2) andalso s1 = s2
      | (Finish, Finish) => true
      | _ => false

fun isKnown e =
    case e of
        Const _ => true
      | Func (_, es) => List.all isKnown es
      | Recd xes => List.all (isKnown o #2) xes
      | Proj (e, _) => isKnown e
      | _ => false

fun isFinish e =
    case e of
        Finish => true
      | _ => false

fun simplify e =
    case e of
        Const _ => e
      | Var _ => e
      | Lvar _ => e
      | Func (f, es) =>
        let
            val es = map simplify es
        in
            if List.exists isFinish es then
                Finish
            else
                Func (f, es)
        end
      | Recd xes =>
        let
            val xes = map (fn (x, e) => (x, simplify e)) xes
        in
            if List.exists (isFinish o #2) xes then
                Finish
            else
                Recd xes
        end
      | Proj (e, s) =>
        (case simplify e of
             Recd xes =>
             getOpt (ListUtil.search (fn (x, e') => if x = s then SOME e' else NONE) xes, Recd xes)
           | e' =>
             if isFinish e' then
                 Finish
             else
                 Proj (e', s))
      | Finish => Finish

fun eq (e1, e2) = eq' (simplify e1, simplify e2)

fun decomp or =
    let
        fun decomp p k =
            case p of
                True => k []
              | False => true
              | Unknown => k []
              | And (p1, p2) => 
                decomp p1 (fn ps1 =>
                              decomp p2 (fn ps2 =>
                                            k (ps1 @ ps2)))
              | Or (p1, p2) =>
                or (decomp p1 k, fn () => decomp p2 k)
              | Reln x => k [x]
              | Select _ => k []
    in
        decomp
    end

fun rimp ((r1 : reln, es1), (r2, es2)) =
    r1 = r2 andalso ListPair.allEq eq (es1, es2)

fun imp (p1, p2) =
    decomp (fn (e1, e2) => e1 andalso e2 ()) p1
    (fn hyps =>
        decomp (fn (e1, e2) => e1 orelse e2 ()) p2
        (fn goals =>
            List.all (fn r2 => List.exists (fn r1 => rimp (r1, r2)) hyps) goals))

fun patCon pc =
    case pc of
        PConVar n => "C" ^ Int.toString n
      | PConFfi {mod = m, datatyp = d, con = c, ...} => m ^ "." ^ d ^ "." ^ c

exception Summaries of (string * exp * prop * (exp * prop) list) list

datatype chunk =
         String of string
       | Exp of Mono.exp

fun chunkify e =
    case #1 e of
        EPrim (Prim.String s) => [String s]
      | EStrcat (e1, e2) => chunkify e1 @ chunkify e2
      | _ => [Exp e]

type 'a parser = chunk list -> ('a * chunk list) option

fun always v chs = SOME (v, chs)

fun parse p chs =
    case p chs of
        SOME (v, []) => SOME v
      | _ => NONE

fun const s chs =
    case chs of
        String s' :: chs => if String.isPrefix s s' then
                                SOME ((), if size s = size s' then
                                              chs
                                          else
                                              String (String.extract (s', size s, NONE)) :: chs)
                            else
                                NONE
      | _ => NONE

fun follow p1 p2 chs =
    case p1 chs of
        NONE => NONE
      | SOME (v1, chs) =>
        case p2 chs of
            NONE => NONE
          | SOME (v2, chs) => SOME ((v1, v2), chs)

fun wrap p f chs =
    case p chs of
        NONE => NONE
      | SOME (v, chs) => SOME (f v, chs)

fun alt p1 p2 chs =
    case p1 chs of
        NONE => p2 chs
      | v => v

fun skip cp chs =
    case chs of
        String "" :: chs => skip cp chs
      | String s :: chs' => if cp (String.sub (s, 0)) then
                                skip cp (String (String.extract (s, 1, NONE)) :: chs')
                            else
                                SOME ((), chs)
      | _ => SOME ((), chs)

fun keep cp chs =
    case chs of
        String "" :: chs => keep cp chs
      | String s :: chs' =>
        let
            val (befor, after) = Substring.splitl cp (Substring.full s)
        in
            if Substring.isEmpty befor then
                NONE
            else
                SOME (Substring.string befor,
                      if Substring.isEmpty after then
                          chs'
                      else
                          String (Substring.string after) :: chs')
        end
      | _ => NONE

fun ws p = wrap (follow p (skip (fn ch => ch = #" "))) #1

fun list p chs =
    (alt (wrap (follow p (follow (ws (const ",")) (list p)))
               (fn (v, ((), ls)) => v :: ls))
         (alt (wrap (ws p) (fn v => [v]))
              (always []))) chs

val ident = keep (fn ch => Char.isAlphaNum ch orelse ch = #"_")

val t_ident = wrap ident (fn s => if String.isPrefix "T_" s then
                                      String.extract (s, 2, NONE)
                                  else
                                      raise Fail "Iflow: Bad table variable")
val uw_ident = wrap ident (fn s => if String.isPrefix "uw_" s then
                                       String.extract (s, 3, NONE)
                                   else
                                       raise Fail "Iflow: Bad uw_* variable")

val sitem = wrap (follow t_ident
                         (follow (const ".")
                                 uw_ident))
                 (fn (t, ((), f)) => (t, f))

val select = wrap (follow (const "SELECT ") (list sitem))
                  (fn ((), ls) => ls)

val fitem = wrap (follow uw_ident
                         (follow (const " AS ")
                                 t_ident))
                 (fn (t, ((), f)) => (t, f))

val from = wrap (follow (const "FROM ") (list fitem))
                (fn ((), ls) => ls)

val query = wrap (follow select from)
            (fn (fs, ts) => {Select = fs, From = ts})

fun queryProp rv e =
    case parse query (chunkify e) of
        NONE => Unknown
      | SOME r =>
        foldl (fn ((t, v), p) =>
                  And (p,
                       Reln (Sql t,
                             [Recd (foldl (fn ((v', f), fs) =>
                                              if v' = v then
                                                  (f, Proj (Proj (Lvar rv, v), f)) :: fs
                                             else
                                                 fs) [] (#Select r))])))
              True (#From r)

fun evalExp env (e : Mono.exp, st as (nv, p, sent)) =
    let
        fun default () =
            (Var nv, (nv+1, p, sent))

        fun addSent (p, e, sent) =
            if isKnown e then
                sent
            else
                (e, p) :: sent
    in
        case #1 e of
            EPrim p => (Const p, st)
          | ERel n => (List.nth (env, n), st)
          | ENamed _ => default ()
          | ECon (_, pc, NONE) => (Func (patCon pc, []), st)
          | ECon (_, pc, SOME e) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Func (patCon pc, [e]), st)
            end
          | ENone _ => (Func ("None", []), st)
          | ESome (_, e) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Func ("Some", [e]), st)
            end
          | EFfi _ => default ()
          | EFfiApp (m, s, es) =>
            if m = "Basis" andalso SS.member (writers, s) then
                let
                    val (es, st) = ListUtil.foldlMap (evalExp env) st es
                in
                    (Func ("unit", []), (#1 st, p, foldl (fn (e, sent) => addSent (#2 st, e, sent)) sent es))
                end
            else if Settings.isEffectful (m, s) andalso not (Settings.isBenignEffectful (m, s)) then
                default ()
            else
                let
                    val (es, st) = ListUtil.foldlMap (evalExp env) st es
                in
                    (Func (m ^ "." ^ s, es), st)
                end
          | EApp _ => default ()
          | EAbs _ => default ()
          | EUnop (s, e1) =>
            let
                val (e1, st) = evalExp env (e1, st)
            in
                (Func (s, [e1]), st)
            end
          | EBinop (s, e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
                val (e2, st) = evalExp env (e2, st)
            in
                (Func (s, [e1, e2]), st)
            end
          | ERecord xets =>
            let
                val (xes, st) = ListUtil.foldlMap (fn ((x, e, _), st) =>
                                                      let
                                                          val (e, st) = evalExp env (e, st)
                                                      in
                                                          ((x, e), st)
                                                      end) st xets
            in
                (Recd xes, st)
            end
          | EField (e, s) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Proj (e, s), st)
            end
          | ECase _ => default ()
          | EStrcat (e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
                val (e2, st) = evalExp env (e2, st)
            in
                (Func ("cat", [e1, e2]), st)
            end
          | EError _ => (Finish, st)
          | EReturnBlob {blob = b, mimeType = m, ...} =>
            let
                val (b, st) = evalExp env (b, st)
                val (m, st) = evalExp env (m, st)
            in
                (Finish, (#1 st, p, addSent (#2 st, b, addSent (#2 st, m, sent))))
            end
          | ERedirect (e, _) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Finish, (#1 st, p, addSent (#2 st, e, sent)))
            end
          | EWrite e =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Func ("unit", []), (#1 st, p, addSent (#2 st, e, sent)))
            end
          | ESeq (e1, e2) =>
            let
                val (_, st) = evalExp env (e1, st)
            in
                evalExp env (e2, st)
            end
          | ELet (_, _, e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
            in
                evalExp (e1 :: env) (e2, st)
            end
          | EClosure (n, es) =>
            let
                val (es, st) = ListUtil.foldlMap (evalExp env) st es
            in
                (Func ("Cl" ^ Int.toString n, es), st)
            end

          | EQuery {query = q, body = b, initial = i, ...} =>
            let
                val (_, st) = evalExp env (q, st)
                val (i, st) = evalExp env (i, st)

                val r = #1 st
                val acc = #1 st + 1
                val st' = (#1 st + 2, #2 st, #3 st)

                val (b, st') = evalExp (Var acc :: Var r :: env) (b, st')

                val r' = newLvar ()
                val acc' = newLvar ()
                val qp = queryProp r' q

                val doSubExp = subExp (r, r') o subExp (acc, acc')
                val doSubProp = subProp (r, r') o subProp (acc, acc')

                val p = doSubProp (#2 st')
                val p = And (p, qp)
                val p = Select (r, r', acc', p, doSubExp b)
            in
                (Var r, (#1 st + 1, And (#2 st, p), map (fn (e, p) => (doSubExp e, And (qp, doSubProp p))) (#3 st')))
            end
          | EDml _ => default ()
          | ENextval _ => default ()
          | ESetval _ => default ()

          | EUnurlify _ => default ()
          | EJavaScript _ => default ()
          | ESignalReturn _ => default ()
          | ESignalBind _ => default ()
          | ESignalSource _ => default ()
          | EServerCall _ => default ()
          | ERecv _ => default ()
          | ESleep _ => default ()
          | ESpawn _ => default ()
    end

fun check file =
    let
        fun decl ((d, _), summaries) =
            case d of
                DVal (x, _, _, e, _) =>
                let
                    fun deAbs (e, env, nv) =
                        case #1 e of
                            EAbs (_, _, _, e) => deAbs (e, Var nv :: env, nv + 1)
                          | _ => (e, env, nv)

                    val (e, env, nv) = deAbs (e, [], 0)

                    val (e, (_, p, sent)) = evalExp env (e, (nv, True, []))
                in
                    (x, e, p, sent) :: summaries
                end
              | _ => summaries
    in
        raise Summaries (foldl decl [] file)
    end

end
