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

structure IS = IntBinarySet
structure IM = IntBinaryMap

structure SK = struct
type ord_key = string
val compare = String.compare
end

structure SS = BinarySetFn(SK)
structure SM = BinaryMapFn(SK)

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
               "urlifyBool_w",
               "set_cookie"]

val writers = SS.addList (SS.empty, writers)

type lvar = int

datatype func =
         DtCon0 of string
       | DtCon1 of string
       | UnCon of string
       | Other of string

datatype exp =
         Const of Prim.t
       | Var of int
       | Lvar of lvar
       | Func of func * exp list
       | Recd of (string * exp) list
       | Proj of exp * string
       | Finish

datatype reln =
         Known
       | Sql of string
       | PCon0 of string
       | PCon1 of string
       | Eq
       | Ne
       | Lt
       | Le
       | Gt
       | Ge

datatype prop =
         True
       | False
       | Unknown
       | And of prop * prop
       | Or of prop * prop
       | Reln of reln * exp list
       | Cond of exp * prop

val unif = ref (IM.empty : exp IM.map)

fun reset () = unif := IM.empty
fun save () = !unif
fun restore x = unif := x

local
    open Print
    val string = PD.string
in

fun p_func f =
    string (case f of
                DtCon0 s => s
              | DtCon1 s => s
              | UnCon s => "un" ^ s
              | Other s => s)

fun p_exp e =
    case e of
        Const p => Prim.p_t p
      | Var n => string ("x" ^ Int.toString n)
      | Lvar n =>
        (case IM.find (!unif, n) of
             NONE => string ("X" ^ Int.toString n)
           | SOME e => p_exp e)
      | Func (f, es) => box [p_func f,
                             string "(",
                             p_list p_exp es,
                             string ")"]
      | Recd xes => box [string "{",
                         p_list (fn (x, e) => box [string x,
                                                   space,
                                                   string "=",
                                                   space,
                                                   p_exp e]) xes,
                         string "}"]
      | Proj (e, x) => box [p_exp e,
                            string ("." ^ x)]
      | Finish => string "FINISH"

fun p_bop s es =
    case es of
        [e1, e2] => box [p_exp e1,
                         space,
                         string s,
                         space,
                         p_exp e2]
      | _ => raise Fail "Iflow.p_bop"

fun p_reln r es =
    case r of
        Known =>
        (case es of
             [e] => box [string "known(",
                         p_exp e,
                         string ")"]
           | _ => raise Fail "Iflow.p_reln: Known")
      | Sql s => box [string (s ^ "("),
                      p_list p_exp es,
                      string ")"]
      | PCon0 s => box [string (s ^ "("),
                        p_list p_exp es,
                        string ")"]
      | PCon1 s => box [string (s ^ "("),
                        p_list p_exp es,
                        string ")"]
      | Eq => p_bop "=" es
      | Ne => p_bop "<>" es
      | Lt => p_bop "<" es
      | Le => p_bop "<=" es
      | Gt => p_bop ">" es
      | Ge => p_bop ">=" es

fun p_prop p =
    case p of
        True => string "True"
      | False => string "False"
      | Unknown => string "??"
      | And (p1, p2) => box [string "(",
                             p_prop p1,
                             string ")",
                             space,
                             string "&&",
                             space,
                             string "(",
                             p_prop p2,
                             string ")"]
      | Or (p1, p2) => box [string "(",
                            p_prop p1,
                            string ")",
                            space,
                            string "||",
                            space,
                            string "(",
                            p_prop p2,
                            string ")"]
      | Reln (r, es) => p_reln r es
      | Cond (e, p) => box [string "(",
                            p_exp e,
                            space,
                            string "==",
                            space,
                            p_prop p,
                            string ")"]

end

local
    val count = ref 1
in
fun newLvar () =
    let
        val n = !count
    in
        count := n + 1;
        n
    end
end

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
      | Lvar n =>
        (case IM.find (!unif, n) of
             NONE => e
           | SOME e => simplify e)
      | Func (f, es) => Func (f, map simplify es)
      | Recd xes => Recd (map (fn (x, e) => (x, simplify e)) xes)
      | Proj (e, s) => Proj (simplify e, s)
      | Finish => Finish

datatype atom =
         AReln of reln * exp list
       | ACond of exp * prop

fun p_atom a =
    p_prop (case a of
                AReln x => Reln x
              | ACond x => Cond x)

fun lvarIn lv =
    let
        fun lvi e =
            case e of
                Const _ => false
              | Var _ => false
              | Lvar lv' => lv' = lv
              | Func (_, es) => List.exists lvi es
              | Recd xes => List.exists (lvi o #2) xes
              | Proj (e, _) => lvi e
              | Finish => false
    in
        lvi
    end

fun lvarInP lv =
    let
        fun lvi p =
            case p of
                True => false
              | False => false
              | Unknown => true
              | And (p1, p2) => lvi p1 orelse lvi p2
              | Or (p1, p2) => lvi p1 orelse lvi p2
              | Reln (_, es) => List.exists (lvarIn lv) es
              | Cond (e, p) => lvarIn lv e orelse lvi p
    in
        lvi
    end

fun varIn lv =
    let
        fun lvi e =
            case e of
                Const _ => false
              | Lvar _ => false
              | Var lv' => lv' = lv
              | Func (_, es) => List.exists lvi es
              | Recd xes => List.exists (lvi o #2) xes
              | Proj (e, _) => lvi e
              | Finish => false
    in
        lvi
    end

fun varInP lv =
    let
        fun lvi p =
            case p of
                True => false
              | False => false
              | Unknown => false
              | And (p1, p2) => lvi p1 orelse lvi p2
              | Or (p1, p2) => lvi p1 orelse lvi p2
              | Reln (_, es) => List.exists (varIn lv) es
              | Cond (e, p) => varIn lv e orelse lvi p
    in
        lvi
    end

fun eq' (e1, e2) =
    case (e1, e2) of
        (Const p1, Const p2) => Prim.equal (p1, p2)
      | (Var n1, Var n2) => n1 = n2

      | (Lvar n1, _) =>
        (case IM.find (!unif, n1) of
             SOME e1 => eq' (e1, e2)
           | NONE =>
             case e2 of
                 Lvar n2 =>
                 (case IM.find (!unif, n2) of
                      SOME e2 => eq' (e1, e2)
                    | NONE => n1 = n2
                              orelse (unif := IM.insert (!unif, n2, e1);
                                      true))
               | _ =>
                 if lvarIn n1 e2 then
                     false
                 else
                     (unif := IM.insert (!unif, n1, e2);
                      true))

      | (_, Lvar n2) =>
        (case IM.find (!unif, n2) of
             SOME e2 => eq' (e1, e2)
           | NONE =>
             if lvarIn n2 e1 then
                 false
             else
                 ((*Print.prefaces "unif" [("n2", Print.PD.string (Int.toString n2)),
                                         ("e1", p_exp e1)];*)
                  unif := IM.insert (!unif, n2, e1);
                  true))
                                       
      | (Func (f1, es1), Func (f2, es2)) => f1 = f2 andalso ListPair.allEq eq' (es1, es2)
      | (Recd xes1, Recd xes2) => ListPair.allEq (fn ((x1, e1), (x2, e2)) => x1 = x2 andalso eq' (e1, e2)) (xes1, xes2)
      | (Proj (e1, s1), Proj (e2, s2)) => eq' (e1, e2) andalso s1 = s2
      | (Finish, Finish) => true
      | _ => false

fun eq (e1, e2) =
    let
        val saved = save ()
    in
        if eq' (simplify e1, simplify e2) then
            true
        else
            (restore saved;
             false)
    end

val debug = ref false

fun eeq (e1, e2) =
    case (e1, e2) of
        (Const p1, Const p2) => Prim.equal (p1, p2)
      | (Var n1, Var n2) => n1 = n2
      | (Lvar n1, Lvar n2) => n1 = n2
      | (Func (f1, es1), Func (f2, es2)) => f1 = f2 andalso ListPair.allEq eeq (es1, es2)
      | (Recd xes1, Recd xes2) => length xes1 = length xes2 andalso
                                  List.all (fn (x2, e2) =>
                                               List.exists (fn (x1, e1) => x1 = x2 andalso eeq (e1, e2)) xes2) xes1
      | (Proj (e1, x1), Proj (e2, x2)) => eeq (e1, e2) andalso x1 = x2
      | (Finish, Finish) => true
      | _ => false
             
(* Congruence closure *)
structure Cc :> sig
    type database
    type representative

    exception Contradiction
    exception Undetermined

    val database : unit -> database
    val representative : database * exp -> representative

    val assert : database * atom -> unit
    val check : database * atom -> bool

    val p_database : database Print.printer
end = struct

exception Contradiction
exception Undetermined

structure CM = BinaryMapFn(struct
                           type ord_key = Prim.t
                           val compare = Prim.compare
                           end)

datatype node = Node of {Rep : node ref option ref,
                         Cons : node ref SM.map ref,
                         Variety : variety,
                         Known : bool ref}

     and variety =
         Dt0 of string
       | Dt1 of string * node ref
       | Prim of Prim.t
       | Recrd of node ref SM.map ref
       | VFinish
       | Nothing

type representative = node ref

val finish = ref (Node {Rep = ref NONE,
                        Cons = ref SM.empty,
                        Variety = VFinish,
                        Known = ref false})

type database = {Vars : representative IM.map ref,
                 Consts : representative CM.map ref,
                 Con0s : representative SM.map ref,
                 Records : (representative SM.map * representative) list ref,
                 Funcs : ((string * representative list) * representative) list ref }

fun database () = {Vars = ref IM.empty,
                   Consts = ref CM.empty,
                   Con0s = ref SM.empty,
                   Records = ref [],
                   Funcs = ref []}

fun unNode n =
    case !n of
        Node r => r

open Print
val string = PD.string
val newline = PD.newline

fun p_rep n =
    case !(#Rep (unNode n)) of
        SOME n => p_rep n
      | NONE =>
        case #Variety (unNode n) of
            Nothing => string ("?" ^ Int.toString (Unsafe.cast n))
          | Dt0 s => string ("Dt0(" ^ s ^ ")")
          | Dt1 (s, n) => box[string ("Dt1(" ^ s ^ ","),
                              space,
                              p_rep n,
                              string ")"]
          | Prim p => Prim.p_t p
          | Recrd (ref m) => box [string "{",
                                  p_list (fn (x, n) => box [string x,
                                                            space,
                                                            string "=",
                                                            space,
                                                            p_rep n]) (SM.listItemsi m),
                                  string "}"]
          | VFinish => string "FINISH"

fun p_database (db : database) =
    box [string "Vars:",
         newline,
         p_list_sep newline (fn (i, n) => box [string ("x" ^ Int.toString i),
                                               space,
                                               string "=",
                                               space,
                                               p_rep n]) (IM.listItemsi (!(#Vars db)))]

fun repOf (n : representative) : representative =
    case !(#Rep (unNode n)) of
        NONE => n
      | SOME r =>
        let
            val r = repOf r
        in
            #Rep (unNode n) := SOME r;
            r
        end

fun markKnown r =
    (#Known (unNode r) := true;
     case #Variety (unNode r) of
         Dt1 (_, r) => markKnown r
       | Recrd xes => SM.app markKnown (!xes)
       | _ => ())

fun representative (db : database, e) =
    let
        fun rep e =
            case e of
                Const p => (case CM.find (!(#Consts db), p) of
                                SOME r => repOf r
                              | NONE =>
                                let
                                    val r = ref (Node {Rep = ref NONE,
                                                       Cons = ref SM.empty,
                                                       Variety = Prim p,
                                                       Known = ref false})
                                in
                                    #Consts db := CM.insert (!(#Consts db), p, r);
                                    r
                                end)
              | Var n => (case IM.find (!(#Vars db), n) of
                              SOME r => repOf r
                            | NONE =>
                              let
                                  val r = ref (Node {Rep = ref NONE,
                                                     Cons = ref SM.empty,
                                                     Variety = Nothing,
                                                     Known = ref false})
                              in
                                  #Vars db := IM.insert (!(#Vars db), n, r);
                                  r
                              end)
              | Lvar n =>
                (case IM.find (!unif, n) of
                     NONE => raise Undetermined
                   | SOME e => rep e)
              | Func (DtCon0 f, []) => (case SM.find (!(#Con0s db), f) of
                                            SOME r => repOf r
                                          | NONE =>
                                            let
                                                val r = ref (Node {Rep = ref NONE,
                                                                   Cons = ref SM.empty,
                                                                   Variety = Dt0 f,
                                                                   Known = ref false})
                                            in
                                                #Con0s db := SM.insert (!(#Con0s db), f, r);
                                                r
                                            end)
              | Func (DtCon0 _, _) => raise Fail "Iflow.rep: DtCon0"
              | Func (DtCon1 f, [e]) =>
                let
                    val r = rep e
                in
                    case SM.find (!(#Cons (unNode r)), f) of
                        SOME r => repOf r
                      | NONE =>
                        let
                            val r' = ref (Node {Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Dt1 (f, r),
                                                Known = ref false})
                        in
                            #Cons (unNode r) := SM.insert (!(#Cons (unNode r)), f, r');
                            r'
                        end
                end
              | Func (DtCon1 _, _) => raise Fail "Iflow.rep: DtCon1"
              | Func (UnCon f, [e]) =>
                let
                    val r = rep e
                in
                    case #Variety (unNode r) of
                        Dt1 (f', n) => if f' = f then
                                           repOf n
                                       else
                                           raise Contradiction
                      | Nothing =>
                        let
                            val cons = ref SM.empty
                            val r' = ref (Node {Rep = ref NONE,
                                                Cons = cons,
                                                Variety = Nothing,
                                                Known = ref false})

                            val r'' = ref (Node {Rep = ref NONE,
                                                 Cons = #Cons (unNode r),
                                                 Variety = Dt1 (f, r'),
                                                 Known = #Known (unNode r)})
                        in
                            cons := SM.insert (!cons, f, r'');
                            #Rep (unNode r) := SOME r'';
                            r'
                        end
                      | VFinish => r
                      | _ => raise Contradiction
                end
              | Func (UnCon _, _) => raise Fail "Iflow.rep: UnCon"
              | Func (Other f, es) =>
                let
                    val rs = map rep es
                in
                    case List.find (fn (x : string * representative list, _) => x = (f, rs)) (!(#Funcs db)) of
                        NONE =>
                        let
                            val r = ref (Node {Rep = ref NONE,
                                               Cons = ref SM.empty,
                                               Variety = Nothing,
                                               Known = ref false})
                        in
                            #Funcs db := ((f, rs), r) :: (!(#Funcs db));
                            r
                        end
                      | SOME (_, r) => repOf r
                end
              | Recd xes =>
                let
                    val xes = map (fn (x, e) => (x, rep e)) xes
                    val len = length xes
                in
                    case List.find (fn (xes', _) =>
                                       SM.numItems xes' = len
                                       andalso List.all (fn (x, n) =>
                                                            case SM.find (xes', x) of
                                                                NONE => false
                                                             | SOME n' => n = repOf n') xes)
                         (!(#Records db)) of
                        SOME (_, r) => repOf r
                      | NONE =>
                        let
                            val xes = foldl SM.insert' SM.empty xes

                            val r' = ref (Node {Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Recrd (ref xes),
                                                Known = ref false})
                        in
                            #Records db := (xes, r') :: (!(#Records db));
                            r'
                        end
                end
              | Proj (e, f) =>
                let
                    val r = rep e
                in
                    case #Variety (unNode r) of
                        Recrd xes =>
                        (case SM.find (!xes, f) of
                             SOME r => repOf r
                           | NONE =>let
                                  val r = ref (Node {Rep = ref NONE,
                                                     Cons = ref SM.empty,
                                                     Variety = Nothing,
                                                     Known = ref false})
                              in
                                 xes := SM.insert (!xes, f, r);
                                 r
                              end)
                      | Nothing =>
                        let
                            val r' = ref (Node {Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Nothing,
                                                Known = ref false})
                                     
                            val r'' = ref (Node {Rep = ref NONE,
                                                 Cons = #Cons (unNode r),
                                                 Variety = Recrd (ref (SM.insert (SM.empty, f, r'))),
                                                 Known = #Known (unNode r)})
                        in
                            #Rep (unNode r) := SOME r'';
                            r'
                        end
                      | VFinish => r 
                      | _ => raise Contradiction                             
                end
              | Finish => finish
    in
        rep e
    end

fun assert (db, a) =
    case a of
        ACond _ => ()
      | AReln x =>
        case x of
            (Known, [e]) => markKnown (representative (db, e))
          | (PCon0 f, [e]) =>
            let
                val r = representative (db, e)
            in
                case #Variety (unNode r) of
                    Dt0 f' => if f = f' then
                                  ()
                              else
                                  raise Contradiction
                  | Nothing =>
                    let
                        val r' = ref (Node {Rep = ref NONE,
                                            Cons = ref SM.empty,
                                            Variety = Dt0 f,
                                            Known = ref false})
                    in
                        #Rep (unNode r) := SOME r'
                    end
                  | _ => raise Contradiction
            end
          | (PCon1 f, [e]) =>
            let
                val r = representative (db, e)
            in
                case #Variety (unNode r) of
                    Dt1 (f', e') => if f = f' then
                                        ()
                                    else
                                        raise Contradiction
                  | Nothing =>
                    let
                        val r'' = ref (Node {Rep = ref NONE,
                                             Cons = ref SM.empty,
                                             Variety = Nothing,
                                             Known = ref false})

                        val r' = ref (Node {Rep = ref NONE,
                                            Cons = ref SM.empty,
                                            Variety = Dt1 (f, r''),
                                            Known = ref false})
                    in
                        #Rep (unNode r) := SOME r'
                    end
                  | _ => raise Contradiction
            end
          | (Eq, [e1, e2]) =>
            let
                fun markEq (r1, r2) =
                    if r1 = r2 then
                        ()
                    else case (#Variety (unNode r1), #Variety (unNode r2)) of
                             (Prim p1, Prim p2) => if Prim.equal (p1, p2) then
                                                       ()
                                                   else
                                                       raise Contradiction
                           | (Dt0 f1, Dt0 f2) => if f1 = f2 then
                                                     ()
                                                 else
                                                     raise Contradiction
                           | (Dt1 (f1, r1), Dt1 (f2, r2)) => if f1 = f2 then
                                                                 markEq (r1, r2)
                                                             else
                                                                 raise Contradiction
                           | (Recrd xes1, Recrd xes2) =>
                             let
                                 fun unif (xes1, xes2) =
                                     SM.appi (fn (x, r1) =>
                                                 case SM.find (xes2, x) of
                                                     NONE => ()
                                                   | SOME r2 => markEq (r1, r2)) xes1
                             in
                                 unif (!xes1, !xes2);
                                 unif (!xes2, !xes1)
                             end
                           | (VFinish, VFinish) => ()
                           | (Nothing, _) =>
                             (#Rep (unNode r1) := SOME r2;
                              if !(#Known (unNode r1)) andalso not (!(#Known (unNode r2))) then
                                  markKnown r2
                              else
                                  ();
                              #Cons (unNode r2) := SM.unionWith #1 (!(#Cons (unNode r2)), !(#Cons (unNode r1)));
                              compactFuncs ())
                           | (_, Nothing) =>
                             (#Rep (unNode r2) := SOME r1;
                              if !(#Known (unNode r2)) andalso not (!(#Known (unNode r1))) then
                                  markKnown r1
                              else
                                  ();
                              #Cons (unNode r1) := SM.unionWith #1 (!(#Cons (unNode r1)), !(#Cons (unNode r2)));
                              compactFuncs ())
                           | _ => raise Contradiction

                and compactFuncs () =
                    let
                        fun loop funcs =
                            case funcs of
                                [] => []
                              | (fr as ((f, rs), r)) :: rest =>
                                let
                                    val rest = List.filter (fn ((f' : string, rs'), r') =>
                                                               if f' = f
                                                                  andalso ListPair.allEq (fn (r1, r2) =>
                                                                                             repOf r1 = repOf r2)
                                                                                         (rs, rs') then
                                                                   (markEq (r, r');
                                                                    false)
                                                               else
                                                                   true) rest
                                in
                                    fr :: loop rest
                                end
                    in
                        #Funcs db := loop (!(#Funcs db))
                    end
            in
                markEq (representative (db, e1), representative (db, e2))
            end
          | _ => ()

fun check (db, a) =
    case a of
        ACond _ => false
      | AReln x =>
        case x of
            (Known, [e]) => !(#Known (unNode (representative (db, e))))
          | (PCon0 f, [e]) =>
            (case #Variety (unNode (representative (db, e))) of
                 Dt0 f' => f' = f
               | _ => false)
          | (PCon1 f, [e]) =>
            (case #Variety (unNode (representative (db, e))) of
                 Dt1 (f', _) => f' = f
               | _ => false)
          | (Eq, [e1, e2]) =>
            let
                val r1 = representative (db, e1)
                val r2 = representative (db, e2)
            in
                repOf r1 = repOf r2
            end
          | _ => false

end

fun decomp fals or =
    let
        fun decomp p k =
            case p of
                True => k []
              | False => fals
              | Unknown => k []
              | And (p1, p2) => 
                decomp p1 (fn ps1 =>
                              decomp p2 (fn ps2 =>
                                            k (ps1 @ ps2)))
              | Or (p1, p2) =>
                or (decomp p1 k, fn () => decomp p2 k)
              | Reln x => k [AReln x]
              | Cond x => k [ACond x]
    in
        decomp
    end

fun imply (p1, p2) =
    (reset ();
     decomp true (fn (e1, e2) => e1 andalso e2 ()) p1
            (fn hyps =>
                decomp false (fn (e1, e2) => e1 orelse e2 ()) p2
                       (fn goals =>
                           let
                               fun gls goals onFail acc =
                                   case goals of
                                       [] =>
                                       (let
                                            val cc = Cc.database ()
                                            val () = app (fn a => Cc.assert (cc, a)) hyps
                                        in
                                            (List.all (fn a =>
                                                          if Cc.check (cc, a) then
                                                              true
                                                          else
                                                              ((*Print.prefaces "Can't prove"
                                                                              [("a", p_atom a),
                                                                               ("hyps", Print.p_list p_atom hyps),
                                                                               ("db", Cc.p_database cc)];*)
                                                               false)) acc
                                             orelse onFail ())
                                            handle Cc.Contradiction => onFail ()
                                        end handle Cc.Undetermined => onFail ())
                                     | AReln (Sql gf, [ge]) :: goals =>
                                       let
                                           fun hps hyps =
                                               case hyps of
                                                   [] => onFail ()
                                                 | AReln (Sql hf, [he]) :: hyps =>
                                                   if gf = hf then
                                                       let
                                                           val saved = save ()
                                                       in
                                                           if eq (ge, he) then
                                                               let
                                                                   val changed = IM.numItems (!unif)
                                                                                 <> IM.numItems saved
                                                               in
                                                                   gls goals (fn () => (restore saved;
                                                                                        changed
                                                                                        andalso hps hyps))
                                                                       acc
                                                               end
                                                           else
                                                               hps hyps
                                                       end
                                                   else
                                                       hps hyps
                                                 | _ :: hyps => hps hyps 
                                       in
                                           hps hyps
                                       end
                                     | g :: goals => gls goals onFail (g :: acc)
                           in
                               gls goals (fn () => false) []
                           end handle Cc.Contradiction => true)))

fun patCon pc =
    case pc of
        PConVar n => "C" ^ Int.toString n
      | PConFfi {mod = m, datatyp = d, con = c, ...} => m ^ "." ^ d ^ "." ^ c

datatype chunk =
         String of string
       | Exp of Mono.exp

fun chunkify e =
    case #1 e of
        EPrim (Prim.String s) => [String s]
      | EStrcat (e1, e2) =>
        let
            val chs1 = chunkify e1
            val chs2 = chunkify e2
        in
            case chs2 of
                String s2 :: chs2' =>
                (case List.last chs1 of
                     String s1 => List.take (chs1, length chs1 - 1) @ String (s1 ^ s2) :: chs2'
                   | _ => chs1 @ chs2)
              | _ => chs1 @ chs2
        end
      | _ => [Exp e]

type 'a parser = chunk list -> ('a * chunk list) option

fun always v chs = SOME (v, chs)

fun parse p s =
    case p (chunkify s) of
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

fun wrapP p f chs =
    case p chs of
        NONE => NONE
      | SOME (v, chs) =>
        case f v of
            NONE => NONE
          | SOME r => SOME (r, chs)

fun alt p1 p2 chs =
    case p1 chs of
        NONE => p2 chs
      | v => v

fun altL ps =
    case rev ps of
        [] => (fn _ => NONE)
      | p :: ps =>
        foldl (fn (p1, p2) => alt p1 p2) p ps

fun opt p chs =
    case p chs of
        NONE => SOME (NONE, chs)
      | SOME (v, chs) => SOME (SOME v, chs)

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

fun ws p = wrap (follow (skip (fn ch => ch = #" "))
                        (follow p (skip (fn ch => ch = #" ")))) (#1 o #2)

fun log name p chs =
    (if !debug then
         case chs of
             String s :: _ => print (name ^ ": " ^ s ^ "\n")
           | _ => print (name ^ ": blocked!\n")
     else
         ();
     p chs)

fun list p chs =
    altL [wrap (follow p (follow (ws (const ",")) (list p)))
               (fn (v, ((), ls)) => v :: ls),
          wrap (ws p) (fn v => [v]),
          always []] chs

val ident = keep (fn ch => Char.isAlphaNum ch orelse ch = #"_")

val t_ident = wrapP ident (fn s => if String.isPrefix "T_" s then
                                       SOME (String.extract (s, 2, NONE))
                                   else
                                       NONE)
val uw_ident = wrapP ident (fn s => if String.isPrefix "uw_" s andalso size s >= 4 then
                                        SOME (str (Char.toUpper (String.sub (s, 3)))
                                              ^ String.extract (s, 4, NONE))
                                    else
                                        NONE)

val field = wrap (follow t_ident
                         (follow (const ".")
                                 uw_ident))
                 (fn (t, ((), f)) => (t, f))

datatype Rel =
         Exps of exp * exp -> prop
       | Props of prop * prop -> prop

datatype sqexp =
         SqConst of Prim.t
       | Field of string * string
       | Binop of Rel * sqexp * sqexp
       | SqKnown of sqexp
       | Inj of Mono.exp
       | SqFunc of string * sqexp
       | Count

fun cmp s r = wrap (const s) (fn () => Exps (fn (e1, e2) => Reln (r, [e1, e2])))

val sqbrel = altL [cmp "=" Eq,
                   cmp "<>" Ne,
                   cmp "<=" Le,
                   cmp "<" Lt,
                   cmp ">=" Ge,
                   cmp ">" Gt,
                   wrap (const "AND") (fn () => Props And),
                   wrap (const "OR") (fn () => Props Or)]

datatype ('a, 'b) sum = inl of 'a | inr of 'b

fun string chs =
    case chs of
        String s :: chs =>
        if size s >= 2 andalso String.sub (s, 0) = #"'" then
            let
                fun loop (cs, acc) =
                    case cs of
                        [] => NONE
                      | c :: cs =>
                        if c = #"'" then
                            SOME (String.implode (rev acc), cs)
                        else if c = #"\\" then
                            case cs of
                                c :: cs => loop (cs, c :: acc)
                              | _ => raise Fail "Iflow.string: Unmatched backslash escape"
                        else
                            loop (cs, c :: acc)
            in
                case loop (String.explode (String.extract (s, 1, NONE)), []) of
                    NONE => NONE
                  | SOME (s, []) => SOME (s, chs)
                  | SOME (s, cs) => SOME (s, String (String.implode cs) :: chs)
            end
        else
            NONE
      | _ => NONE                            

val prim =
    altL [wrap (follow (wrapP (follow (keep Char.isDigit) (follow (const ".") (keep Char.isDigit)))
                              (fn (x, ((), y)) => Option.map Prim.Float (Real64.fromString (x ^ "." ^ y))))
                       (opt (const "::float8"))) #1,
          wrap (follow (wrapP (keep Char.isDigit)
                              (Option.map Prim.Int o Int64.fromString))
                       (opt (const "::int8"))) #1,
          wrap (follow (opt (const "E")) (follow string (opt (const "::text"))))
               (Prim.String o #1 o #2)]

fun known' chs =
    case chs of
        Exp (EFfi ("Basis", "sql_known"), _) :: chs => SOME ((), chs)
      | _ => NONE

fun sqlify chs =
    case chs of
        Exp (EFfiApp ("Basis", f, [e]), _) :: chs =>
        if String.isPrefix "sqlify" f then
            SOME (e, chs)
        else
            NONE
      | _ => NONE

fun constK s = wrap (const s) (fn () => s)

val funcName = altL [constK "COUNT",
                     constK "MIN",
                     constK "MAX",
                     constK "SUM",
                     constK "AVG"]

fun sqexp chs =
    log "sqexp"
    (altL [wrap prim SqConst,
           wrap field Field,
           wrap known SqKnown,
           wrap func SqFunc,
           wrap (const "COUNT(*)") (fn () => Count),
           wrap sqlify Inj,
           wrap (follow (const "COALESCE(") (follow sqexp (follow (const ",")
                                                                  (follow (keep (fn ch => ch <> #")")) (const ")")))))
                (fn ((), (e, _)) => e),
           wrap (follow (ws (const "("))
                        (follow (wrap
                                     (follow sqexp
                                             (alt
                                                  (wrap
                                                       (follow (ws sqbrel)
                                                               (ws sqexp))
                                                       inl)
                                                  (always (inr ()))))
                                     (fn (e1, sm) =>
                                         case sm of
                                             inl (bo, e2) => Binop (bo, e1, e2)
                                           | inr () => e1))
                                (const ")")))
                (fn ((), (e, ())) => e)])
    chs

and known chs = wrap (follow known' (follow (const "(") (follow sqexp (const ")"))))
                     (fn ((), ((), (e, ()))) => e) chs
                
and func chs = wrap (follow funcName (follow (const "(") (follow sqexp (const ")"))))
                    (fn (f, ((), (e, ()))) => (f, e)) chs

datatype sitem =
         SqField of string * string
       | SqExp of sqexp * string

val sitem = alt (wrap field SqField)
            (wrap (follow sqexp (follow (const " AS ") uw_ident))
             (fn (e, ((), s)) => SqExp (e, s)))

val select = log "select"
             (wrap (follow (const "SELECT ") (list sitem))
                   (fn ((), ls) => ls))

val fitem = wrap (follow uw_ident
                         (follow (const " AS ")
                                 t_ident))
                 (fn (t, ((), f)) => (t, f))

val from = log "from"
           (wrap (follow (const "FROM ") (list fitem))
                 (fn ((), ls) => ls))

val wher = wrap (follow (ws (const "WHERE ")) sqexp)
           (fn ((), ls) => ls)

val query = log "query"
                (wrap (follow (follow select from) (opt wher))
                      (fn ((fs, ts), wher) => {Select = fs, From = ts, Where = wher}))

fun removeDups (ls : (string * string) list) =
    case ls of
        [] => []
      | x :: ls =>
        let
            val ls = removeDups ls
        in
            if List.exists (fn x' => x' = x) ls then
                ls  
            else
                x :: ls
        end

datatype queryMode =
         SomeCol of exp
       | AllCols of exp

fun queryProp env rvN rv oe e =
    case parse query e of
        NONE => (print ("Warning: Information flow checker can't parse SQL query at "
                        ^ ErrorMsg.spanToString (#2 e) ^ "\n");
                 (rvN, Var 0, Unknown, []))
      | SOME r =>
        let
            val (rvN, count) = rv rvN

            val (rvs, rvN) = ListUtil.foldlMap (fn ((_, v), rvN) =>
                                                   let
                                                       val (rvN, e) = rv rvN
                                                   in
                                                       ((v, e), rvN)
                                                   end) rvN (#From r)

            fun rvOf v =
                case List.find (fn (v', _) => v' = v) rvs of
                    NONE => raise Fail "Iflow.queryProp: Bad table variable"
                  | SOME (_, e) => e

            fun usedFields e =
                case e of
                    SqConst _ => []
                  | Field (v, f) => [(v, f)]
                  | Binop (_, e1, e2) => removeDups (usedFields e1 @ usedFields e2)
                  | SqKnown _ => []
                  | Inj _ => []
                  | SqFunc (_, e) => usedFields e
                  | Count => []

            val p =
                foldl (fn ((t, v), p) => And (p, Reln (Sql t, [rvOf v]))) True (#From r)

            fun expIn e =
                case e of
                    SqConst p => inl (Const p)
                  | Field (v, f) => inl (Proj (rvOf v, f))
                  | Binop (bo, e1, e2) =>
                    inr (case (bo, expIn e1, expIn e2) of
                             (Exps f, inl e1, inl e2) => f (e1, e2)
                           | (Props f, inr p1, inr p2) => f (p1, p2)
                           | _ => Unknown)
                  | SqKnown e =>
                    inr (case expIn e of
                             inl e => Reln (Known, [e])
                           | _ => Unknown)
                  | Inj e =>
                    let
                        fun deinj (e, _) =
                            case e of
                                ERel n => List.nth (env, n)
                              | EField (e, f) => Proj (deinj e, f)
                              | _ => raise Fail "Iflow: non-variable injected into query"
                    in
                        inl (deinj e)
                    end
                  | SqFunc (f, e) =>
                    inl (case expIn e of
                         inl e => Func (Other f, [e])
                       | _ => raise Fail ("Iflow: non-expresion passed to function " ^ f))
                  | Count => inl count

            val p = case #Where r of
                        NONE => p
                      | SOME e =>
                        case expIn e of
                            inr p' => And (p, p')
                          | _ => p
        in
            (rvN,
             count,
             And (p, case oe of
                         SomeCol oe =>
                         foldl (fn (si, p) =>
                                   let
                                       val p' = case si of
                                                    SqField (v, f) => Reln (Eq, [oe, Proj (rvOf v, f)])
                                                  | SqExp (e, f) =>
                                                    case expIn e of
                                                        inr _ => Unknown
                                                      | inl e => Reln (Eq, [oe, e])
                                   in
                                       Or (p, p')
                                   end)
                               False (#Select r)
                       | AllCols oe =>
                         foldl (fn (si, p) =>
                                   let
                                       val p' = case si of
                                                    SqField (v, f) => Reln (Eq, [Proj (Proj (oe, v), f),
                                                                                 Proj (rvOf v, f)])
                                                  | SqExp (e, f) =>
                                                    case expIn e of
                                                        inr p => Cond (Proj (oe, f), p)
                                                      | inl e => Reln (Eq, [Proj (oe, f), e])
                                   in
                                       And (p, p')
                                   end)
                               True (#Select r)),
             
             case #Where r of
                 NONE => []
               | SOME e => map (fn (v, f) => Proj (rvOf v, f)) (usedFields e))
        end

fun evalPat env e (pt, _) =
    case pt of
        PWild => (env, True)
      | PVar _ => (e :: env, True)
      | PPrim _ => (env, True)
      | PCon (_, pc, NONE) => (env, Reln (PCon0 (patCon pc), [e]))
      | PCon (_, pc, SOME pt) =>
        let
            val (env, p) = evalPat env (Func (UnCon (patCon pc), [e])) pt
        in
            (env, And (p, Reln (PCon1 (patCon pc), [e])))
        end
      | PRecord xpts =>
        foldl (fn ((x, pt, _), (env, p)) =>
                  let
                      val (env, p') = evalPat env (Proj (e, x)) pt
                  in
                      (env, And (p', p))
                  end) (env, True) xpts
      | PNone _ => (env, Reln (PCon0 "None", [e]))
      | PSome (_, pt) =>
        let
            val (env, p) = evalPat env (Func (UnCon "Some", [e])) pt
        in
            (env, And (p, Reln (PCon1 "Some", [e])))
        end

fun peq (p1, p2) =
    case (p1, p2) of
        (True, True) => true
      | (False, False) => true
      | (Unknown, Unknown) => true
      | (And (x1, y1), And (x2, y2)) => peq (x1, x2) andalso peq (y1, y2)
      | (Or (x1, y1), Or (x2, y2)) => peq (x1, x2) andalso peq (y1, y2)
      | (Reln (r1, es1), Reln (r2, es2)) => r1 = r2 andalso ListPair.allEq eeq (es1, es2)
      | (Cond (e1, p1), Cond (e2, p2)) => eeq (e1, e2) andalso peq (p1, p2)
      | _ => false

fun removeRedundant p1 =
    let
        fun rr p2 =
            if peq (p1, p2) then
                True
            else
                case p2 of
                    And (x, y) => And (rr x, rr y)
                  | Or (x, y) => Or (rr x, rr y)
                  | _ => p2
    in
        rr
    end

fun evalExp env (e as (_, loc), st as (nv, p, sent)) =
    let
        fun default () =
            ((*Print.preface ("Default" ^ Int.toString nv,
                            MonoPrint.p_exp MonoEnv.empty e);*)
            (Var nv, (nv+1, p, sent)))

        fun addSent (p, e, sent) =
            if isKnown e then
                sent
            else
                (loc, e, p) :: sent
    in
        case #1 e of
            EPrim p => (Const p, st)
          | ERel n => (List.nth (env, n), st)
          | ENamed _ => default ()
          | ECon (_, pc, NONE) => (Func (DtCon0 (patCon pc), []), st)
          | ECon (_, pc, SOME e) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Func (DtCon1 (patCon pc), [e]), st)
            end
          | ENone _ => (Func (DtCon0 "None", []), st)
          | ESome (_, e) =>
            let
                val (e, st) = evalExp env (e, st)
            in
                (Func (DtCon1 "Some", [e]), st)
            end
          | EFfi _ => default ()

          | EFfiApp (m, s, es) =>
            if m = "Basis" andalso SS.member (writers, s) then
                let
                    val (es, st) = ListUtil.foldlMap (evalExp env) st es
                in
                    (Recd [], (#1 st, p, foldl (fn (e, sent) => addSent (#2 st, e, sent)) sent es))
                end
            else if Settings.isEffectful (m, s) andalso not (Settings.isBenignEffectful (m, s)) then
                default ()
            else
                let
                    val (es, st) = ListUtil.foldlMap (evalExp env) st es
                in
                    (Func (Other (m ^ "." ^ s), es), st)
                end

          | EApp (e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
            in
                case e1 of
                    Finish => (Finish, st)
                  | _ => default ()
            end

          | EAbs _ => default ()
          | EUnop (s, e1) =>
            let
                val (e1, st) = evalExp env (e1, st)
            in
                (Func (Other s, [e1]), st)
            end
          | EBinop (s, e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
                val (e2, st) = evalExp env (e2, st)
            in
                (Func (Other s, [e1, e2]), st)
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
          | ECase (e, pes, _) =>
            let
                val (e, st) = evalExp env (e, st)
                val r = #1 st
                val st = (r + 1, #2 st, #3 st)
                val orig = #2 st

                val st = foldl (fn ((pt, pe), st) =>
                                   let
                                       val (env, pp) = evalPat env e pt
                                       val (pe, st') = evalExp env (pe, (#1 st, And (orig, pp), #3 st))
                                                       
                                       val this = And (removeRedundant orig (#2 st'), Reln (Eq, [Var r, pe]))
                                   in
                                       (#1 st', Or (#2 st, this), #3 st')
                                   end) (#1 st, False, #3 st) pes
            in
                (Var r, (#1 st, And (orig, #2 st), #3 st))
            end
          | EStrcat (e1, e2) =>
            let
                val (e1, st) = evalExp env (e1, st)
                val (e2, st) = evalExp env (e2, st)
            in
                (Func (Other "cat", [e1, e2]), st)
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
                (Recd [], (#1 st, p, addSent (#2 st, e, sent)))
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
                (Func (Other ("Cl" ^ Int.toString n), es), st)
            end

          | EQuery {query = q, body = b, initial = i, ...} =>
            let
                val (_, st) = evalExp env (q, st)
                val (i, st) = evalExp env (i, st)

                val r = #1 st
                val acc = #1 st + 1
                val st' = (#1 st + 2, #2 st, #3 st)

                val (b, st') = evalExp (Var acc :: Var r :: env) (b, st')

                val (rvN, count, qp, used) =
                    queryProp env
                              (#1 st') (fn rvN => (rvN + 1, Var rvN))
                              (AllCols (Var r)) q

                val p' = And (qp, #2 st')

                val (nvs, p, res) = if varInP acc (#2 st') then
                                        (#1 st + 1, #2 st, Var r)
                                    else
                                        let
                                            val out = rvN

                                            val p = Or (Reln (Eq, [Var out, i]),
                                                        And (Reln (Eq, [Var out, b]),
                                                             And (Reln (Gt, [count,
                                                                             Const (Prim.Int 0)]),
                                                                  p')))
                                        in
                                            (out + 1, p, Var out)
                                        end

                val sent = map (fn (loc, e, p) => (loc, e, And (qp, p))) (#3 st')
                val sent = map (fn e => (loc, e, p')) used @ sent
            in
                (res, (nvs, p, sent))
            end
          | EDml _ => default ()
          | ENextval _ => default ()
          | ESetval _ => default ()

          | EUnurlify ((EFfiApp ("Basis", "get_cookie", _), _), _, _) =>
            (Var nv, (nv + 1, And (p, Reln (Known, [Var nv])), sent))

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
        val file = MonoReduce.reduce file
        val file = MonoOpt.optimize file
        val file = Fuse.fuse file
        val file = MonoOpt.optimize file
        (*val () = Print.preface ("File", MonoPrint.p_file MonoEnv.empty file)*)

        val exptd = foldl (fn ((d, _), exptd) =>
                              case d of
                                  DExport (_, _, n, _, _, _) => IS.add (exptd, n)
                                | _ => exptd) IS.empty file

        fun decl ((d, _), (vals, pols)) =
            case d of
                DVal (_, n, _, e, _) =>
                let
                    val isExptd = IS.member (exptd, n)

                    fun deAbs (e, env, nv, p) =
                        case #1 e of
                            EAbs (_, _, _, e) => deAbs (e, Var nv :: env, nv + 1,
                                                        if isExptd then
                                                            And (p, Reln (Known, [Var nv]))
                                                        else
                                                            p)
                          | _ => (e, env, nv, p)

                    val (e, env, nv, p) = deAbs (e, [], 1, True)

                    val (e, (_, p, sent)) = evalExp env (e, (nv, p, []))
                in
                    (sent @ vals, pols)
                end

              | DPolicy (PolClient e) => (vals, #3 (queryProp [] 0 (fn rvN => (rvN + 1, Lvar rvN))
                                                              (SomeCol (Var 0)) e) :: pols)
                                        
              | _ => (vals, pols)

        val () = reset ()

        val (vals, pols) = foldl decl ([], []) file
    in
        app (fn (loc, e, p) =>
                let
                    fun doOne e =
                        let
                            val p = And (p, Reln (Eq, [Var 0, e]))
                        in
                            if List.exists (fn pol => if imply (p, pol) then
                                                          (if !debug then
                                                               Print.prefaces "Match"
                                                                              [("Hyp", p_prop p),
                                                                               ("Goal", p_prop pol)]
                                                           else
                                                               ();
                                                           true)
                                                      else
                                                          false) pols then
                                ()
                            else
                                (ErrorMsg.errorAt loc "The information flow policy may be violated here.";
                                 Print.preface ("The state satisifes this predicate:", p_prop p))
                        end

                    fun doAll e =
                        case e of
                            Const _ => ()
                          | Var _ => doOne e
                          | Lvar _ => raise Fail "Iflow.doAll: Lvar"
                          | Func (UnCon _, [e]) => doOne e
                          | Func (_, es) => app doAll es
                          | Recd xes => app (doAll o #2) xes
                          | Proj _ => doOne e
                          | Finish => ()
                in
                    doAll e
                end) vals
    end

val check = fn file =>
               let
                   val oldInline = Settings.getMonoInline ()
               in
                   (Settings.setMonoInline (case Int.maxInt of
                                                NONE => 1000000
                                              | SOME n => n);
                    check file;
                    Settings.setMonoInline oldInline)
                   handle ex => (Settings.setMonoInline oldInline;
                                 raise ex)
               end

end

