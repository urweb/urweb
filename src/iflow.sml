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
      | Lvar n => string ("X" ^ Int.toString n)
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

fun isKnown e =
    case e of
        Const _ => true
      | Func (_, es) => List.all isKnown es
      | Recd xes => List.all (isKnown o #2) xes
      | Proj (e, _) => isKnown e
      | _ => false

fun simplify unif =
    let
        fun simplify e =
            case e of
                Const _ => e
              | Var _ => e
              | Lvar n =>
                (case IM.find (unif, n) of
                     NONE => e
                   | SOME e => simplify e)
              | Func (f, es) => Func (f, map simplify es)
              | Recd xes => Recd (map (fn (x, e) => (x, simplify e)) xes)
              | Proj (e, s) => Proj (simplify e, s)
    in
        simplify
    end

datatype atom =
         AReln of reln * exp list
       | ACond of exp * prop

fun p_atom a =
    p_prop (case a of
                AReln x => Reln x
              | ACond x => Cond x)

val debug = ref false
             
(* Congruence closure *)
structure Cc :> sig
    type database

    exception Contradiction

    val database : unit -> database
    val clear : database -> unit

    val assert : database * atom -> unit
    val check : database * atom -> bool

    val p_database : database Print.printer

    val builtFrom : database * {Base : exp list, Derived : exp} -> bool

    val p_repOf : database -> exp Print.printer
end = struct

local
    val count = ref 0
in
fun nodeId () =
    let
        val n = !count
    in
        count := n + 1;
        n
    end
end

exception Contradiction
exception Undetermined

structure CM = BinaryMapFn(struct
                           type ord_key = Prim.t
                           val compare = Prim.compare
                           end)

datatype node = Node of {Id : int,
                         Rep : node ref option ref,
                         Cons : node ref SM.map ref,
                         Variety : variety,
                         Known : bool ref,
                         Ge : Int64.int option ref}

     and variety =
         Dt0 of string
       | Dt1 of string * node ref
       | Prim of Prim.t
       | Recrd of node ref SM.map ref * bool
       | Nothing

type representative = node ref

type database = {Vars : representative IM.map ref,
                 Consts : representative CM.map ref,
                 Con0s : representative SM.map ref,
                 Records : (representative SM.map * representative) list ref,
                 Funcs : ((string * representative list) * representative) list ref}

fun database () = {Vars = ref IM.empty,
                   Consts = ref CM.empty,
                   Con0s = ref SM.empty,
                   Records = ref [],
                   Funcs = ref []}

fun clear (t : database) = (#Vars t := IM.empty;
                            #Consts t := CM.empty;
                            #Con0s t := SM.empty;
                            #Records t := [];
                            #Funcs t := [])

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
        box [string (Int.toString (#Id (unNode n)) ^ ":"),
             space,
             case #Variety (unNode n) of
                 Nothing => string "?"
               | Dt0 s => string ("Dt0(" ^ s ^ ")")
               | Dt1 (s, n) => box[string ("Dt1(" ^ s ^ ","),
                                   space,
                                   p_rep n,
                                   string ")"]
               | Prim p => Prim.p_t p
               | Recrd (ref m, b) => box [string "{",
                                          p_list (fn (x, n) => box [string x,
                                                                    space,
                                                                    string "=",
                                                                    space,
                                                                    p_rep n]) (SM.listItemsi m),
                                          string "}",
                                          if b then
                                              box [space,
                                                   string "(complete)"]
                                          else
                                              box []],
             if !(#Known (unNode n)) then
                 string " (known)"
             else
                 box [],
             case !(#Ge (unNode n)) of
                 NONE => box []
               | SOME n => string (" (>= " ^ Int64.toString n ^ ")")]

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
    let
        val r = repOf r
    in
        (*Print.preface ("markKnown", p_rep r);*)
        if !(#Known (unNode r)) then
            ()(*TextIO.print "Already known\n"*)
        else
            (#Known (unNode r) := true;
             SM.app markKnown (!(#Cons (unNode r)));
             case #Variety (unNode r) of
                 Dt1 (_, r) => markKnown r
               | Recrd (xes, _) => SM.app markKnown (!xes)
               | _ => ())
    end

fun representative (db : database, e) =
    let
        fun rep e =
            case e of
                Const p => (case CM.find (!(#Consts db), p) of
                                SOME r => repOf r
                              | NONE =>
                                let
                                    val r = ref (Node {Id = nodeId (),
                                                       Rep = ref NONE,
                                                       Cons = ref SM.empty,
                                                       Variety = Prim p,
                                                       Known = ref true,
                                                       Ge = ref (case p of
                                                                     Prim.Int n => SOME n
                                                                   | _ => NONE)})
                                in
                                    #Consts db := CM.insert (!(#Consts db), p, r);
                                    r
                                end)
              | Var n => (case IM.find (!(#Vars db), n) of
                              SOME r => repOf r
                            | NONE =>
                              let
                                  val r = ref (Node {Id = nodeId (),
                                                     Rep = ref NONE,
                                                     Cons = ref SM.empty,
                                                     Variety = Nothing,
                                                     Known = ref false,
                                                     Ge = ref NONE})
                              in
                                  #Vars db := IM.insert (!(#Vars db), n, r);
                                  r
                              end)
              | Lvar _ => raise Undetermined
              | Func (DtCon0 f, []) => (case SM.find (!(#Con0s db), f) of
                                            SOME r => repOf r
                                          | NONE =>
                                            let
                                                val r = ref (Node {Id = nodeId (),
                                                                   Rep = ref NONE,
                                                                   Cons = ref SM.empty,
                                                                   Variety = Dt0 f,
                                                                   Known = ref true,
                                                                   Ge = ref NONE})
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
                            val r' = ref (Node {Id = nodeId (),
                                                Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Dt1 (f, r),
                                                Known = ref (!(#Known (unNode r))),
                                                Ge = ref NONE})
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
                            val r' = ref (Node {Id = nodeId (),
                                                Rep = ref NONE,
                                                Cons = cons,
                                                Variety = Nothing,
                                                Known = ref (!(#Known (unNode r))),
                                                Ge = ref NONE})

                            val r'' = ref (Node {Id = nodeId (),
                                                 Rep = ref NONE,
                                                 Cons = #Cons (unNode r),
                                                 Variety = Dt1 (f, r'),
                                                 Known = #Known (unNode r),
                                                 Ge = ref NONE})
                        in
                            cons := SM.insert (!cons, f, r'');
                            #Rep (unNode r) := SOME r'';
                            r'
                        end
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
                            val r = ref (Node {Id = nodeId (),
                                               Rep = ref NONE,
                                               Cons = ref SM.empty,
                                               Variety = Nothing,
                                               Known = ref (f = "allow"),
                                               Ge = ref NONE})
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

                            val r' = ref (Node {Id = nodeId (),
                                                Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Recrd (ref xes, true),
                                                Known = ref false,
                                                Ge = ref NONE})
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
                        Recrd (xes, _) =>
                        (case SM.find (!xes, f) of
                             SOME r => repOf r
                           | NONE => let
                                  val r = ref (Node {Id = nodeId (),
                                                     Rep = ref NONE,
                                                     Cons = ref SM.empty,
                                                     Variety = Nothing,
                                                     Known = ref (!(#Known (unNode r))),
                                                     Ge = ref NONE})
                              in
                                 xes := SM.insert (!xes, f, r);
                                 r
                              end)
                      | Nothing =>
                        let
                            val r' = ref (Node {Id = nodeId (),
                                                Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Nothing,
                                                Known = ref (!(#Known (unNode r))),
                                                Ge = ref NONE})
                                     
                            val r'' = ref (Node {Id = nodeId (),
                                                 Rep = ref NONE,
                                                 Cons = #Cons (unNode r),
                                                 Variety = Recrd (ref (SM.insert (SM.empty, f, r')), false),
                                                 Known = #Known (unNode r),
                                                 Ge = ref NONE})
                        in
                            #Rep (unNode r) := SOME r'';
                            r'
                        end
                      | _ => raise Contradiction                             
                end
    in
        rep e
    end

fun p_repOf db e = p_rep (representative (db, e))

fun assert (db, a) =
    let
        fun markEq (r1, r2) =
            let
                val r1 = repOf r1
                val r2 = repOf r2
            in
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
                       | (Recrd (xes1, _), Recrd (xes2, _)) =>
                         let
                             fun unif (xes1, xes2) =
                                 SM.appi (fn (x, r1) =>
                                             case SM.find (!xes2, x) of
                                                 NONE => xes2 := SM.insert (!xes2, x, r1)
                                               | SOME r2 => markEq (r1, r2)) (!xes1)
                         in
                             unif (xes1, xes2);
                             unif (xes2, xes1)
                         end
                       | (Nothing, _) => mergeNodes (r1, r2)
                       | (_, Nothing) => mergeNodes (r2, r1)
                       | _ => raise Contradiction
            end

        and mergeNodes (r1, r2) =
            (#Rep (unNode r1) := SOME r2;
             if !(#Known (unNode r1)) then
                 markKnown r2
             else
                 ();
             if !(#Known (unNode r2)) then
                 markKnown r1
             else
                 ();
             #Cons (unNode r2) := SM.unionWith #1 (!(#Cons (unNode r2)), !(#Cons (unNode r1)));

             case !(#Ge (unNode r1)) of
                 NONE => ()
               | SOME n1 =>
                 case !(#Ge (unNode r2)) of
                     NONE => #Ge (unNode r2) := SOME n1
                   | SOME n2 => #Ge (unNode r2) := SOME (Int64.max (n1, n2));

             compactFuncs ())

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
        case a of
            ACond _ => ()
          | AReln x =>
            case x of
                (Known, [e]) =>
                ((*Print.prefaces "Before" [("e", p_exp e),
                                            ("db", p_database db)];*)
                 markKnown (representative (db, e))(*;
                                                        Print.prefaces "After" [("e", p_exp e),
                                                                                ("db", p_database db)]*))
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
                        (case SM.find (!(#Con0s db), f) of
                             SOME r' => markEq (r, r')
                           | NONE =>
                             let
                                 val r' = ref (Node {Id = nodeId (),
                                                     Rep = ref NONE,
                                                     Cons = ref SM.empty,
                                                     Variety = Dt0 f,
                                                     Known = ref false,
                                                     Ge = ref NONE})
                             in
                                 #Rep (unNode r) := SOME r';
                                 #Con0s db := SM.insert (!(#Con0s db), f, r')
                             end)
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
                            val cons = ref SM.empty

                            val r'' = ref (Node {Id = nodeId (),
                                                 Rep = ref NONE,
                                                 Cons = cons,
                                                 Variety = Nothing,
                                                 Known = ref (!(#Known (unNode r))),
                                                 Ge = ref NONE})

                            val r' = ref (Node {Id = nodeId (),
                                                Rep = ref NONE,
                                                Cons = ref SM.empty,
                                                Variety = Dt1 (f, r''),
                                                Known = #Known (unNode r),
                                                Ge = ref NONE})
                        in
                            cons := SM.insert (!cons, f, r');
                            #Rep (unNode r) := SOME r'
                        end
                      | _ => raise Contradiction
                end
              | (Eq, [e1, e2]) =>
                markEq (representative (db, e1), representative (db, e2))
              | (Ge, [e1, e2]) =>
                let
                    val r1 = representative (db, e1)
                    val r2 = representative (db, e2)
                in
                    case !(#Ge (unNode (repOf r2))) of
                        NONE => ()
                      | SOME n2 =>
                        case !(#Ge (unNode (repOf r1))) of
                            NONE => #Ge (unNode (repOf r1)) := SOME n2
                          | SOME n1 => #Ge (unNode (repOf r1)) := SOME (Int64.max (n1, n2))
                end
              | _ => ()
    end handle Undetermined => ()

fun check (db, a) =
    (case a of
         ACond _ => false
       | AReln x =>
         case x of
             (Known, [e]) =>
             let
                 fun isKnown r =
                     let
                         val r = repOf r
                     in
                         !(#Known (unNode r))
                         orelse case #Variety (unNode r) of
                                    Dt1 (_, r) => isKnown r
                                  | Recrd (xes, true) => List.all isKnown (SM.listItems (!xes))
                                  | _ => false
                     end

                 val r = representative (db, e)
             in
                 isKnown r
             end
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
           | (Ge, [e1, e2]) =>
             let
                 val r1 = representative (db, e1)
                 val r2 = representative (db, e2)
             in
                 case (!(#Ge (unNode (repOf r1))), #Variety (unNode (repOf r2))) of
                     (SOME n1, Prim (Prim.Int n2)) => Int64.>= (n1, n2)
                   | _ => false
             end
           | _ => false)
    handle Undetermined => false

fun builtFrom (db, {Base = bs, Derived = d}) =
    let
        val bs = map (fn b => representative (db, b)) bs

        fun loop d =
            let
                val d = repOf d
            in
                !(#Known (unNode d))
                orelse List.exists (fn b => repOf b = d) bs
                orelse (case #Variety (unNode d) of
                            Dt0 _ => true
                          | Dt1 (_, d) => loop d
                          | Prim _ => true
                          | Recrd (xes, _) => List.all loop (SM.listItems (!xes))
                          | Nothing => false)
                orelse List.exists (fn r => List.exists (fn b => repOf b = repOf r) bs)
                                   (SM.listItems (!(#Cons (unNode d))))
            end

        fun decomp e =
            case e of
                Func (Other _, es) => List.all decomp es
              | _ => loop (representative (db, e))
    in
        decomp d
    end handle Undetermined => false

end

val tabs = ref (SM.empty : (string list * string list list) SM.map)

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
         (print (name ^ ": ");
          app (fn String s => print s
                | _ => print "???") chs;
          print "\n")
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
       | SqTrue
       | SqFalse
       | SqNot of sqexp
       | Field of string * string
       | Computed of string
       | Binop of Rel * sqexp * sqexp
       | SqKnown of sqexp
       | Inj of Mono.exp
       | SqFunc of string * sqexp
       | Unmodeled
       | Null

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
        Exp (EFfiApp ("Basis", f, [(e, _)]), _) :: chs =>
        if String.isPrefix "sqlify" f then
            SOME (e, chs)
        else
            NONE
      | Exp (ECase (e, [((PCon (_, PConFfi {mod = "Basis", con = "True", ...}, NONE), _),
                         (EPrim (Prim.String "TRUE"), _)),
                        ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, NONE), _),
                         (EPrim (Prim.String "FALSE"), _))], _), _) :: chs =>
        SOME (e, chs)
                          
      | _ => NONE

fun constK s = wrap (const s) (fn () => s)

val funcName = altL [constK "COUNT",
                     constK "MIN",
                     constK "MAX",
                     constK "SUM",
                     constK "AVG"]

val unmodeled = altL [const "COUNT(*)",
                      const "CURRENT_TIMESTAMP"]

fun sqexp chs =
    log "sqexp"
    (altL [wrap prim SqConst,
           wrap (const "TRUE") (fn () => SqTrue),
           wrap (const "FALSE") (fn () => SqFalse),
           wrap (const "NULL") (fn () => Null),
           wrap field Field,
           wrap uw_ident Computed,
           wrap known SqKnown,
           wrap func SqFunc,
           wrap unmodeled (fn () => Unmodeled),
           wrap sqlify Inj,
           wrap (follow (const "COALESCE(") (follow sqexp (follow (const ",")
                                                                  (follow (keep (fn ch => ch <> #")")) (const ")")))))
                (fn ((), (e, _)) => e),
           wrap (follow (const "(NOT ") (follow sqexp (const ")")))
                (fn ((), (e, _)) => SqNot e),
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

val sitem = alt (wrap (follow sqexp (follow (const " AS ") uw_ident))
                      (fn (e, ((), s)) => SqExp (e, s)))
                (wrap field SqField)

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

type query1 = {Select : sitem list,
              From : (string * string) list,
              Where : sqexp option}

val query1 = log "query1"
                (wrap (follow (follow select from) (opt wher))
                      (fn ((fs, ts), wher) => {Select = fs, From = ts, Where = wher}))

datatype query =
         Query1 of query1
       | Union of query * query

val orderby = log "orderby"
              (wrap (follow (ws (const "ORDER BY "))
                            (follow (list sqexp)
                                    (opt (ws (const "DESC")))))
                    ignore)

fun query chs = log "query"
                (wrap
                     (follow
                          (alt (wrap (follow (const "((")
                                             (follow query
                                                     (follow (const ") UNION (")
                                                             (follow query (const "))")))))
                                     (fn ((), (q1, ((), (q2, ())))) => Union (q1, q2)))
                               (wrap query1 Query1))
                          (opt orderby))
                     #1)
                chs

datatype dml =
         Insert of string * (string * sqexp) list
       | Delete of string * sqexp
       | Update of string * (string * sqexp) list * sqexp

val insert = log "insert"
             (wrapP (follow (const "INSERT INTO ")
                            (follow uw_ident
                                    (follow (const " (")
                                            (follow (list uw_ident)
                                                    (follow (const ") VALUES (")
                                                            (follow (list sqexp)
                                                                    (const ")")))))))
              (fn ((), (tab, ((), (fs, ((), (es, ())))))) =>
                  (SOME (tab, ListPair.zipEq (fs, es)))
                  handle ListPair.UnequalLengths => NONE))

val delete = log "delete"
                 (wrap (follow (const "DELETE FROM ")
                               (follow uw_ident
                                       (follow (const " AS T_T WHERE ")
                                               sqexp)))
                       (fn ((), (tab, ((), es))) => (tab, es)))

val setting = log "setting"
              (wrap (follow uw_ident (follow (const " = ") sqexp))
               (fn (f, ((), e)) => (f, e)))

val update = log "update"
                 (wrap (follow (const "UPDATE ")
                               (follow uw_ident
                                       (follow (const " AS T_T SET ")
                                               (follow (list setting)
                                                       (follow (ws (const "WHERE "))
                                                               sqexp)))))
                       (fn ((), (tab, ((), (fs, ((), e))))) =>
                           (tab, fs, e)))

val dml = log "dml"
              (altL [wrap insert Insert,
                     wrap delete Delete,
                     wrap update Update])

type check = exp * ErrorMsg.span

structure St :> sig
    val reset : unit -> unit

    type stashed
    val stash : unit -> stashed
    val reinstate : stashed -> unit

    type stashedPath
    val stashPath : unit -> stashedPath
    val reinstatePath : stashedPath -> unit

    val nextVar : unit -> int

    val assert : atom list -> unit

    val addPath : check -> unit

    val allowSend : atom list * exp list -> unit
    val send : check -> unit

    val allowInsert : atom list -> unit
    val insert : ErrorMsg.span -> unit

    val allowDelete : atom list -> unit
    val delete : ErrorMsg.span -> unit

    val allowUpdate : atom list -> unit
    val update : ErrorMsg.span -> unit

    val havocReln : reln -> unit
    val havocCookie : string -> unit

    val check : atom -> bool

    val debug : unit -> unit
end = struct

val hnames = ref 1

type hyps = int * atom list * bool ref

val db = Cc.database ()
val path = ref ([] : ((int * atom list) * check) option ref list)
val hyps = ref (0, [] : atom list, ref false)
val nvar = ref 0

fun setHyps (n', hs) =
    let
        val (n, _, _) = !hyps
    in
        if n' = n then
            ()
        else
            (hyps := (n', hs, ref false);
             Cc.clear db;
             app (fn a => Cc.assert (db, a)) hs)
    end    

fun useKeys () =
    let
        val changed = ref false

        fun findKeys (hyps, acc) =
            case hyps of
                [] => rev acc
              | (a as AReln (Sql tab, [r1])) :: hyps =>
                (case SM.find (!tabs, tab) of
                     NONE => findKeys (hyps, a :: acc)
                   | SOME (_, []) => findKeys (hyps, a :: acc)
                   | SOME (_, ks) =>
                     let
                         fun finder (hyps, acc) =
                             case hyps of
                                 [] => rev acc
                               | (a as AReln (Sql tab', [r2])) :: hyps =>
                                 if tab' = tab andalso
                                    List.exists (List.all (fn f =>
                                                              let
                                                                  val r =
                                                                      Cc.check (db,
                                                                                AReln (Eq, [Proj (r1, f),
                                                                                            Proj (r2, f)]))
                                                              in
                                                                  (*Print.prefaces "Fs"
                                                                                   [("tab",
                                                                                     Print.PD.string tab),
                                                                                    ("r1",
                                                                                     p_exp (Proj (r1, f))),
                                                                                    ("r2",
                                                                                     p_exp (Proj (r2, f))),
                                                                                    ("r",
                                                                                     Print.PD.string
                                                                                         (Bool.toString r))];*)
                                                                  r
                                                              end)) ks then
                                     (changed := true;
                                      Cc.assert (db, AReln (Eq, [r1, r2]));
                                      finder (hyps, acc))
                                 else
                                     finder (hyps, a :: acc)
                               | a :: hyps => finder (hyps, a :: acc)

                         val hyps = finder (hyps, [])
                     in
                         findKeys (hyps, a :: acc)
                     end)
              | a :: hyps => findKeys (hyps, a :: acc)

        fun loop hs =
            let
                val hs = findKeys (hs, [])
            in
                if !changed then
                    (changed := false;
                     loop hs)
                else
                    ()
            end

        val (_, hs, _) = !hyps
    in
        (*print "useKeys\n";*)
        loop hs
    end

fun complete () =
    let
        val (_, _, bf) = !hyps
    in
        if !bf then
            ()
        else
            (bf := true;
             useKeys ())
    end

type stashed = int * ((int * atom list) * check) option ref list * (int * atom list)
fun stash () = (!nvar, !path, (#1 (!hyps), #2 (!hyps)))
fun reinstate (nv, p, h) =
    (nvar := nv;
     path := p;
     setHyps h)

type stashedPath = ((int * atom list) * check) option ref list
fun stashPath () = !path
fun reinstatePath p = path := p

fun nextVar () =
    let
        val n = !nvar
    in
        nvar := n + 1;
        n
    end

fun assert ats =
    let
        val n = !hnames
        val (_, hs, _) = !hyps
    in
        hnames := n + 1;
        hyps := (n, ats @ hs, ref false);
        app (fn a => Cc.assert (db, a)) ats
    end

fun addPath c = path := ref (SOME ((#1 (!hyps), #2 (!hyps)), c)) :: !path

val sendable = ref ([] : (atom list * exp list) list)

fun checkGoals goals k =
    let
        fun checkGoals goals unifs =
            case goals of
                [] => k unifs
              | AReln (Sql tab, [Lvar lv]) :: goals =>
                let
                    val saved = stash ()
                    val (_, hyps, _) = !hyps

                    fun tryAll unifs hyps =
                        case hyps of
                            [] => false
                          | AReln (Sql tab', [e]) :: hyps =>
                            (tab' = tab andalso
                             checkGoals goals (IM.insert (unifs, lv, e)))
                            orelse tryAll unifs hyps
                          | _ :: hyps => tryAll unifs hyps
                in
                    tryAll unifs hyps
                end
              | (g as AReln (r, es)) :: goals =>
                (complete ();
                 (if Cc.check (db, AReln (r, map (simplify unifs) es)) then
                      true
                  else
                      ((*Print.preface ("Fail", p_atom (AReln (r, map (simplify unifs) es)));*)
                       false))
                 andalso checkGoals goals unifs)
              | ACond _ :: _ => false
    in
        checkGoals goals IM.empty
    end

fun buildable (e, loc) =
    let
        fun doPols pols acc =
            case pols of
                [] =>
                let
                    val b = Cc.builtFrom (db, {Base = acc, Derived = e})
                in
                    (*Print.prefaces "buildable" [("Base", Print.p_list p_exp acc),
                                                ("Derived", p_exp e),
                                                ("Hyps", Print.p_list p_atom (#2 (!hyps))),
                                                ("Good", Print.PD.string (Bool.toString b))];*)
                    b
                end
              | (goals, es) :: pols =>
                checkGoals goals (fn unifs => doPols pols (map (simplify unifs) es @ acc))
                orelse doPols pols acc
    in
        if doPols (!sendable) [] then
            ()
        else
            let
                val (_, hs, _) = !hyps
            in
                ErrorMsg.errorAt loc "The information flow policy may be violated here.";
                Print.prefaces "Situation" [("User learns", p_exp e),
                                            ("Hypotheses", Print.p_list p_atom hs),
                                            ("E-graph", Cc.p_database db)]
            end
    end

fun checkPaths () =
    let
        val (n, hs, _) = !hyps
        val hs = (n, hs)
    in
        app (fn r =>
                case !r of
                    NONE => ()
                  | SOME (hs, e) =>
                    (r := NONE;
                     setHyps hs;
                     buildable e)) (!path);
        setHyps hs
    end

fun allowSend v = ((*Print.prefaces "Allow" [("goals", Print.p_list p_atom (#1 v)),
                                           ("exps", Print.p_list p_exp (#2 v))];*)
                   sendable := v :: !sendable)

fun send (e, loc) = ((*Print.preface ("Send[" ^ Bool.toString uk ^ "]", p_exp e);*)
                     complete ();
                     checkPaths ();
                     if isKnown e then
                         ()
                     else
                         buildable (e, loc))

fun doable pols (loc : ErrorMsg.span) =
    let
        val pols = !pols
    in
        complete ();
        if List.exists (fn goals =>
                           if checkGoals goals (fn _ => true) then
                               ((*Print.prefaces "Match" [("goals", Print.p_list p_atom goals),
                                                        ("hyps", Print.p_list p_atom (#2 (!hyps)))];*)
                                true)
                           else
                               ((*Print.prefaces "No match" [("goals", Print.p_list p_atom goals)(*,
                                                           ("hyps", Print.p_list p_atom (#2 (!hyps)))*)];*)
                                false)) pols then
            ()
        else
            let
                val (_, hs, _) = !hyps
            in
                ErrorMsg.errorAt loc "The database update policy may be violated here.";
                Print.prefaces "Situation" [("Hypotheses", Print.p_list p_atom hs)(*,
                                            ("E-graph", Cc.p_database db)*)]
            end
    end

val insertable = ref ([] : atom list list)
fun allowInsert v = insertable := v :: !insertable
val insert = doable insertable

val updatable = ref ([] : atom list list)
fun allowUpdate v = updatable := v :: !updatable
val update = doable updatable

val deletable = ref ([] : atom list list)
fun allowDelete v = deletable := v :: !deletable
val delete = doable deletable

fun reset () = (Cc.clear db;
                path := [];
                hyps := (0, [], ref false);
                nvar := 0;
                sendable := [];
                insertable := [];
                updatable := [];
                deletable := [])

fun havocReln r =
    let
        val n = !hnames
        val (_, hs, _) = !hyps
    in
        hnames := n + 1;
        hyps := (n, List.filter (fn AReln (r', _) => r' <> r | _ => true) hs, ref false)
    end

fun havocCookie cname =
    let
        val cname = "cookie/" ^ cname
        val n = !hnames
        val (_, hs, _) = !hyps
    in
        hnames := n + 1;
        hyps := (n, List.filter (fn AReln (Eq, [_, Func (Other f, [])]) => f <> cname | _ => true) hs, ref false)
    end

fun check a = Cc.check (db, a)

fun debug () =
    let
        val (_, hs, _) = !hyps
    in
        Print.preface ("Hyps", Print.p_list p_atom hs)
    end

end


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

fun deinj env e =
    case #1 e of
        ERel n => SOME (List.nth (env, n))
      | EField (e, f) =>
        (case deinj env e of
             NONE => NONE
           | SOME e => SOME (Proj (e, f)))
      | EApp ((EFfi mf, _), e) =>
        if Settings.isEffectful mf orelse Settings.isBenignEffectful mf then
            NONE
        else (case deinj env e of
                  NONE => NONE
                | SOME e => SOME (Func (Other (#1 mf ^ "." ^ #2 mf), [e])))
      | _ => NONE

fun expIn rv env rvOf =
    let
        fun expIn e =
            let
                fun default () = inl (rv ())
            in
                case e of
                    SqConst p => inl (Const p)
                  | SqTrue => inl (Func (DtCon0 "Basis.bool.True", []))
                  | SqFalse => inl (Func (DtCon0 "Basis.bool.False", []))
                  | Null => inl (Func (DtCon0 "None", []))
                  | SqNot e =>
                    inr (case expIn e of
                             inl e => Reln (Eq, [e, Func (DtCon0 "Basis.bool.False", [])])
                           | inr _ => Unknown)
                  | Field (v, f) => inl (Proj (rvOf v, f))
                  | Computed _ => default ()
                  | Binop (bo, e1, e2) =>
                    let
                        val e1 = expIn e1
                        val e2 = expIn e2
                    in
                        inr (case (bo, e1, e2) of
                                 (Exps f, inl e1, inl e2) => f (e1, e2)
                               | (Props f, v1, v2) =>
                                 let
                                     fun pin v =
                                         case v of
                                             inl e => Reln (Eq, [e, Func (DtCon0 "Basis.bool.True", [])])
                                           | inr p => p
                                 in
                                     f (pin v1, pin v2)
                                 end
                               | _ => Unknown)
                    end
                  | SqKnown e =>
                    (case expIn e of
                         inl e => inr (Reln (Known, [e]))
                       | _ => inr Unknown)
                  | Inj e =>
                    inl (case deinj env e of
                             NONE => rv ()
                           | SOME e => e)
                  | SqFunc (f, e) =>
                    (case expIn e of
                         inl e => inl (Func (Other f, [e]))
                       | _ => default ())
                     
                  | Unmodeled => inl (Func (Other "allow", [rv ()]))
            end
    in
        expIn
    end

fun decomp {Save = save, Restore = restore, Add = add} =
    let
        fun go p k =
            case p of
                True => (k () handle Cc.Contradiction => ())
              | False => ()
              | Unknown => ()
              | And (p1, p2) => go p1 (fn () => go p2 k)
              | Or (p1, p2) =>
                let
                    val saved = save ()
                in
                    go p1 k;
                    restore saved;
                    go p2 k
                end
              | Reln x => (add (AReln x); k ())
              | Cond x => (add (ACond x); k ())
    in
        go
    end

datatype queryMode =
         SomeCol of {New : (string * exp) option, Old : (string * exp) option, Outs : exp list} -> unit
       | AllCols of exp -> unit

type 'a doQuery = {
     Env : exp list,
     NextVar : unit -> exp,
     Add : atom -> unit,
     Save : unit -> 'a,
     Restore : 'a -> unit,
     Cont : queryMode
}

fun doQuery (arg : 'a doQuery) (e as (_, loc)) =
    let
        fun default () = ErrorMsg.errorAt loc "Information flow checker can't parse SQL query"
    in
        case parse query e of
            NONE => default ()
          | SOME q =>
            let
                fun doQuery q =
                    case q of
                        Query1 r =>
                        let
                            val new = ref NONE
                            val old = ref NONE

                            val rvs = map (fn (tab, v) =>
                                              let
                                                  val nv = #NextVar arg ()
                                              in
                                                  case v of
                                                      "New" => new := SOME (tab, nv)
                                                    | "Old" => old := SOME (tab, nv)
                                                    | _ => ();
                                                  (v, nv)
                                              end) (#From r)

                            fun rvOf v =
                                case List.find (fn (v', _) => v' = v) rvs of
                                    NONE => raise Fail "Iflow.queryProp: Bad table variable"
                                  | SOME (_, e) => e

                            val expIn = expIn (#NextVar arg) (#Env arg) rvOf

                            val saved = #Save arg ()
                            fun addFrom () = app (fn (t, v) => #Add arg (AReln (Sql t, [rvOf v]))) (#From r)

                            fun usedFields e =
                                case e of
                                    SqConst _ => []
                                  | SqTrue => []
                                  | SqFalse => []
                                  | Null => []
                                  | SqNot e => usedFields e
                                  | Field (v, f) => [(false, Proj (rvOf v, f))]
                                  | Computed _ => []
                                  | Binop (_, e1, e2) => usedFields e1 @ usedFields e2
                                  | SqKnown _ => []
                                  | Inj e =>
                                    (case deinj (#Env arg) e of
                                         NONE => (ErrorMsg.errorAt loc "Expression injected into SQL is too complicated";
                                                  [])
                                       | SOME e => [(true, e)])
                                  | SqFunc (_, e) => usedFields e
                                  | Unmodeled => []

                            fun normal' () =
                                case #Cont arg of
                                    SomeCol k =>
                                    let
                                        val sis = map (fn si =>
                                                          case si of
                                                              SqField (v, f) => Proj (rvOf v, f)
                                                            | SqExp (e, f) =>
                                                              case expIn e of
                                                                  inr _ => #NextVar arg ()
                                                                | inl e => e) (#Select r)
                                    in
                                        k {New = !new, Old = !old, Outs = sis}
                                    end
                                  | AllCols k =>
                                    let
                                        val (ts, es) =
                                            foldl (fn (si, (ts, es)) =>
                                                      case si of
                                                          SqField (v, f) =>
                                                          let
                                                              val fs = getOpt (SM.find (ts, v), SM.empty)
                                                          in
                                                              (SM.insert (ts, v, SM.insert (fs, f, Proj (rvOf v, f))), es)
                                                          end
                                                        | SqExp (e, f) =>
                                                          let
                                                              val e =
                                                                  case expIn e of
                                                                      inr _ => #NextVar arg ()
                                                                    | inl e => e
                                                          in
                                                              (ts, SM.insert (es, f, e))
                                                          end)
                                                  (SM.empty, SM.empty) (#Select r)
                                    in
                                        k (Recd (map (fn (t, fs) => (t, Recd (SM.listItemsi fs)))
                                                     (SM.listItemsi ts)
                                                 @ SM.listItemsi es))
                                    end

                            fun doWhere final =
                                (addFrom ();
                                 case #Where r of
                                     NONE => final ()
                                   | SOME e =>
                                     let
                                         val p = case expIn e of
                                                     inl e => Reln (Eq, [e, Func (DtCon0 "Basis.bool.True", [])])
                                                   | inr p => p

                                         val saved = #Save arg ()
                                     in
                                         decomp {Save = #Save arg, Restore = #Restore arg, Add = #Add arg}
                                                p (fn () => final () handle Cc.Contradiction => ());
                                         #Restore arg saved
                                     end)
                                handle Cc.Contradiction => ()

                            fun normal () = doWhere normal'
                        in
                            (case #Select r of
                                 [SqExp (Binop (Exps bo, Count, SqConst (Prim.Int 0)), f)] =>
                                 (case bo (Const (Prim.Int 1), Const (Prim.Int 2)) of
                                      Reln (Gt, [Const (Prim.Int 1), Const (Prim.Int 2)]) =>
                                      (case #Cont arg of
                                           SomeCol _ => ()
                                         | AllCols k =>
                                           let
                                               fun answer e = k (Recd [(f, e)])

                                               val saved = #Save arg ()
                                               val () = (answer (Func (DtCon0 "Basis.bool.False", [])))
                                                        handle Cc.Contradiction => ()
                                           in
                                               #Restore arg saved;
                                               (*print "True time!\n";*)
                                               doWhere (fn () => answer (Func (DtCon0 "Basis.bool.True", [])));
                                               #Restore arg saved
                                           end)
                                    | _ => normal ())
                               | _ => normal ())
                            before #Restore arg saved
                        end
                      | Union (q1, q2) =>
                        let
                            val saved = #Save arg ()
                        in
                            doQuery q1;
                            #Restore arg saved;
                            doQuery q2;
                            #Restore arg saved
                        end
            in
                doQuery q
            end
    end

fun evalPat env e (pt, _) =
    case pt of
        PWild => env
      | PVar _ => e :: env
      | PPrim _ => env
      | PCon (_, pc, NONE) => (St.assert [AReln (PCon0 (patCon pc), [e])]; env)
      | PCon (_, pc, SOME pt) =>
        let
            val env = evalPat env (Func (UnCon (patCon pc), [e])) pt
        in
            St.assert [AReln (PCon1 (patCon pc), [e])];
            env
        end
      | PRecord xpts =>
        foldl (fn ((x, pt, _), env) => evalPat env (Proj (e, x)) pt) env xpts
      | PNone _ => (St.assert [AReln (PCon0 "None", [e])]; env)
      | PSome (_, pt) =>
        let
            val env = evalPat env (Func (UnCon "Some", [e])) pt
        in
            St.assert [AReln (PCon1 "Some", [e])];
            env
        end

datatype arg_mode = Fixed | Decreasing | Arbitrary
type rfun = {args : arg_mode list, tables : SS.set, cookies : SS.set, body : Mono.exp}
val rfuns = ref (IM.empty : rfun IM.map)

fun evalExp env (e as (_, loc)) k =
    let
        (*val () = St.debug ()*)
        (*val () = Print.preface ("evalExp", MonoPrint.p_exp MonoEnv.empty e)*)

        fun default () = k (Var (St.nextVar ()))

        fun doFfi (m, s, es) =
            if m = "Basis" andalso SS.member (writers, s) then
                let
                    fun doArgs es =
                        case es of
                            [] =>
                            (if s = "set_cookie" then
                                 case es of
                                     [_, (cname, _), _, _, _] =>
                                     (case #1 cname of
                                          EPrim (Prim.String cname) =>
                                          St.havocCookie cname
                                        | _ => ())
                                   | _ => ()
                             else
                                 ();
                             k (Recd []))
                          | (e, _) :: es =>
                            evalExp env e (fn e => (St.send (e, loc); doArgs es))
                in
                    doArgs es
                end
            else if Settings.isEffectful (m, s) andalso not (Settings.isBenignEffectful (m, s)) then
                default ()
            else
                let
                    fun doArgs (es, acc) =
                        case es of
                            [] => k (Func (Other (m ^ "." ^ s), rev acc))
                          | (e, _) :: es =>
                            evalExp env e (fn e => doArgs (es, e :: acc))
                in
                    doArgs (es, [])
                end            
    in
        case #1 e of
            EPrim p => k (Const p)
          | ERel n => k (List.nth (env, n))
          | ENamed _ => default ()
          | ECon (_, pc, NONE) => k (Func (DtCon0 (patCon pc), []))
          | ECon (_, pc, SOME e) => evalExp env e (fn e => k (Func (DtCon1 (patCon pc), [e])))
          | ENone _ => k (Func (DtCon0 "None", []))
          | ESome (_, e) => evalExp env e (fn e => k (Func (DtCon1 "Some", [e])))
          | EFfi _ => default ()

          | EFfiApp ("Basis", "rand", []) =>
            let
                val e = Var (St.nextVar ())
            in
                St.assert [AReln (Known, [e])];
                k e
            end
          | EFfiApp x => doFfi x
          | EApp ((EFfi (m, s), _), e) => doFfi (m, s, [(e, (TRecord [], loc))])

          | EApp (e1 as (EError _, _), _) => evalExp env e1 k

          | EApp (e1, e2) =>
            let
                fun adefault () = (ErrorMsg.errorAt loc "Excessively fancy function call";
                                   Print.preface ("Call", MonoPrint.p_exp MonoEnv.empty e);
                                   default ())

                fun doArgs (e, args) =
                    case #1 e of
                        EApp (e1, e2) => doArgs (e1, e2 :: args)
                      | ENamed n =>
                        (case IM.find (!rfuns, n) of
                             NONE => adefault ()
                           | SOME rf =>
                             if length (#args rf) <> length args then
                                 adefault ()
                             else
                                 let
                                     val () = (SS.app (St.havocReln o Sql) (#tables rf);
                                               SS.app St.havocCookie (#cookies rf))
                                     val saved = St.stash ()

                                     fun doArgs (args, modes, env') =
                                         case (args, modes) of
                                             ([], []) => (evalExp env' (#body rf) (fn _ => ());
                                                          St.reinstate saved;
                                                          default ())
                                                         
                                           | (arg :: args, mode :: modes) =>
                                             evalExp env arg (fn arg =>
                                                                 let
                                                                     val v = case mode of
                                                                                 Arbitrary => Var (St.nextVar ())
                                                                               | Fixed => arg
                                                                               | Decreasing =>
                                                                                 let
                                                                                     val v = Var (St.nextVar ())
                                                                                 in
                                                                                     if St.check (AReln (Known, [arg])) then
                                                                                         St.assert [(AReln (Known, [v]))]
                                                                                     else
                                                                                         ();
                                                                                     v
                                                                                 end
                                                                 in
                                                                     doArgs (args, modes, v :: env')
                                                                 end)
                                           | _ => raise Fail "Iflow.doArgs: Impossible"
                                 in
                                     doArgs (args, #args rf, [])
                                 end)
                          | _ => adefault ()
            in
                doArgs (e, [])
            end

          | EAbs _ => default ()
          | EUnop (s, e1) => evalExp env e1 (fn e1 => k (Func (Other s, [e1])))
          | EBinop (_, s, e1, e2) => evalExp env e1 (fn e1 => evalExp env e2 (fn e2 => k (Func (Other s, [e1, e2]))))
          | ERecord xets =>
            let
                fun doFields (xes, acc) =
                    case xes of
                        [] => k (Recd (rev acc))
                      | (x, e, _) :: xes =>
                        evalExp env e (fn e => doFields (xes, (x, e) :: acc))
            in
                doFields (xets, [])
            end
          | EField (e, s) => evalExp env e (fn e => k (Proj (e, s)))
          | ECase (e, pes, {result = res, ...}) =>
            evalExp env e (fn e =>
                              if List.all (fn (_, (EWrite (EPrim _, _), _)) => true
                                            | _ => false) pes then
                                  (St.send (e, loc);
                                   k (Recd []))
                              else
                                  (St.addPath (e, loc);
                                   app (fn (p, pe) =>
                                           let
                                               val saved = St.stash ()
                                           in
                                               let
                                                   val env = evalPat env e p
                                               in
                                                   evalExp env pe k;
                                                   St.reinstate saved
                                               end
                                               handle Cc.Contradiction => St.reinstate saved
                                           end) pes))
          | EStrcat (e1, e2) =>
            evalExp env e1 (fn e1 =>
                evalExp env e2 (fn e2 =>
                                   k (Func (Other "cat", [e1, e2]))))
          | EError (e, _) => evalExp env e (fn e => St.send (e, loc))
          | EReturnBlob {blob = b, mimeType = m, ...} =>
            evalExp env b (fn b =>
                              (St.send (b, loc);
                               evalExp env m
                               (fn m => St.send (m, loc))))
          | ERedirect (e, _) =>
            evalExp env e (fn e => St.send (e, loc))
          | EWrite e =>
            evalExp env e (fn e => (St.send (e, loc);
                                    k (Recd [])))
          | ESeq (e1, e2) =>
            let
                val path = St.stashPath ()
            in
                evalExp env e1 (fn _ => (St.reinstatePath path; evalExp env e2 k))
            end
          | ELet (_, _, e1, e2) =>
            evalExp env e1 (fn e1 => evalExp (e1 :: env) e2 k)
          | EClosure (n, es) =>
            let
                fun doArgs (es, acc) =
                    case es of
                        [] => k (Func (Other ("Cl" ^ Int.toString n), rev acc))
                      | e :: es =>
                        evalExp env e (fn e => doArgs (es, e :: acc))
            in
                doArgs (es, [])
            end

          | EQuery {query = q, body = b, initial = i, state = state, ...} =>
            evalExp env i (fn i =>
                              let
                                  val r = Var (St.nextVar ())
                                  val acc = Var (St.nextVar ())

                                  val (ts, cs) = MonoUtil.Exp.fold {typ = fn (_, st) => st,
                                                                   exp = fn (e, st as (cs, ts)) =>
                                                                            case e of
                                                                                EDml (e, _) =>
                                                                                (case parse dml e of
                                                                                     NONE => st
                                                                                   | SOME c =>
                                                                                     case c of
                                                                                         Insert _ => st
                                                                                       | Delete (tab, _) =>
                                                                                         (cs, SS.add (ts, tab))
                                                                                       | Update (tab, _, _) =>
                                                                                         (cs, SS.add (ts, tab)))
                                                                              | EFfiApp ("Basis", "set_cookie",
                                                                                         [_, ((EPrim (Prim.String cname), _), _),
                                                                                          _, _, _]) =>
                                                                                (SS.add (cs, cname), ts)
                                                                              | _ => st}
                                                                  (SS.empty, SS.empty) b
                              in
                                  case (#1 state, SS.isEmpty ts, SS.isEmpty cs) of
                                          (TRecord [], true, true) => ()
                                        | _ =>
                                          let
                                              val saved = St.stash ()
                                          in
                                              (k i)
                                              handle Cc.Contradiction => ();
                                              St.reinstate saved
                                          end;

                                  SS.app (St.havocReln o Sql) ts;
                                  SS.app St.havocCookie cs;

                                  doQuery {Env = env,
                                           NextVar = Var o St.nextVar,
                                           Add = fn a => St.assert [a],
                                           Save = St.stash,
                                           Restore = St.reinstate,
                                           Cont = AllCols (fn x =>
                                                              (St.assert [AReln (Eq, [r, x])];
                                                               evalExp (acc :: r :: env) b k))} q
                              end)
          | EDml (e, _) =>
            (case parse dml e of
                 NONE => (print ("Warning: Information flow checker can't parse DML command at "
                                 ^ ErrorMsg.spanToString loc ^ "\n");
                          default ())
               | SOME d =>
                 case d of
                     Insert (tab, es) =>
                     let
                         val new = St.nextVar ()

                         val expIn = expIn (Var o St.nextVar) env
                                           (fn _ => raise Fail "Iflow.evalExp: Bad field expression in INSERT [1]")

                         val es = map (fn (x, e) =>
                                          case expIn e of
                                              inl e => (x, e)
                                            | inr _ => raise Fail "Iflow.evalExp: Bad field expression in INSERT [2]")
                                  es

                         val saved = St.stash ()
                     in
                         St.assert [AReln (Sql (tab ^ "$New"), [Recd es])];
                         St.insert loc;
                         St.reinstate saved;
                         St.assert [AReln (Sql tab, [Recd es])];
                         k (Recd [])
                     end
                   | Delete (tab, e) =>
                     let
                         val old = St.nextVar ()
                                   
                         val expIn = expIn (Var o St.nextVar) env
                                           (fn "T" => Var old
                                             | _ => raise Fail "Iflow.evalExp: Bad field expression in DELETE")

                         val p = case expIn e of
                                     inl e => raise Fail "Iflow.evalExp: DELETE with non-boolean" 
                                   | inr p => p
                                                          
                         val saved = St.stash ()
                     in
                         St.assert [AReln (Sql (tab ^ "$Old"), [Var old]),
                                    AReln (Sql (tab), [Var old])];
                         decomp {Save = St.stash,
                                 Restore = St.reinstate,
                                 Add = fn a => St.assert [a]} p
                                (fn () => (St.delete loc;
                                           St.reinstate saved;
                                           St.havocReln (Sql tab);
                                           k (Recd []))
                                    handle Cc.Contradiction => ())
                     end
                   | Update (tab, fs, e) =>
                     let
                         val new = St.nextVar ()
                         val old = St.nextVar ()

                         val expIn = expIn (Var o St.nextVar) env
                                           (fn "T" => Var old
                                             | _ => raise Fail "Iflow.evalExp: Bad field expression in UPDATE")

                         val fs = map
                                      (fn (x, e) =>
                                          (x, case expIn e of
                                                  inl e => e
                                                | inr _ => raise Fail
                                                                     ("Iflow.evalExp: Selecting "
                                                                      ^ "boolean expression")))
                                      fs

                         val fs' = case SM.find (!tabs, tab) of
                                       NONE => raise Fail "Iflow.evalExp: Updating unknown table"
                                     | SOME (fs', _) => fs'

                         val fs = foldl (fn (f, fs) =>
                                            if List.exists (fn (f', _) => f' = f) fs then
                                                fs
                                            else
                                                (f, Proj (Var old, f)) :: fs) fs fs'

                         val p = case expIn e of
                                           inl e => raise Fail "Iflow.evalExp: UPDATE with non-boolean" 
                                         | inr p => p
                         val saved = St.stash ()
                     in
                         St.assert [AReln (Sql (tab ^ "$New"), [Recd fs]),
                                    AReln (Sql (tab ^ "$Old"), [Var old]),
                                    AReln (Sql tab, [Var old])];
                         decomp {Save = St.stash,
                                 Restore = St.reinstate,
                                 Add = fn a => St.assert [a]} p
                                (fn () => (St.update loc;
                                           St.reinstate saved;
                                           St.havocReln (Sql tab);
                                           k (Recd []))
                                    handle Cc.Contradiction => ())
                     end)
                     
          | ENextval (EPrim (Prim.String seq), _) =>
            let
                val nv = St.nextVar ()
            in
                St.assert [AReln (Sql (String.extract (seq, 3, NONE)), [Var nv])];
                k (Var nv)
            end
          | ENextval _ => default ()
          | ESetval _ => default ()

          | EUnurlify ((EFfiApp ("Basis", "get_cookie", [((EPrim (Prim.String cname), _), _)]), _), _, _) =>
            let
                val e = Var (St.nextVar ())
                val e' = Func (Other ("cookie/" ^ cname), [])
            in
                St.assert [AReln (Known, [e]), AReln (Eq, [e, e'])];
                k e
            end

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

datatype var_source = Input of int | SubInput of int | Unknown

fun check file =
    let
        val () = (St.reset ();
                  rfuns := IM.empty)

        val file = MonoReduce.reduce file
        val file = MonoOpt.optimize file
        val file = Fuse.fuse file
        val file = MonoOpt.optimize file
        val file = MonoShake.shake file
        (*val () = Print.preface ("File", MonoPrint.p_file MonoEnv.empty file)*)

        val exptd = foldl (fn ((d, _), exptd) =>
                              case d of
                                  DExport (_, _, n, _, _, _) => IS.add (exptd, n)
                                | _ => exptd) IS.empty file

        fun decl (d, loc) =
            case d of
                DTable (tab, fs, pk, _) =>
                let
                    val ks =
                        case #1 pk of
                            EPrim (Prim.String s) =>
                            (case String.tokens (fn ch => ch = #"," orelse ch = #" ") s of
                                 [] => []
                               | pk => [pk])
                          | _ => []
                in
                    if size tab >= 3 then
                        tabs := SM.insert (!tabs, String.extract (tab, 3, NONE),
                                           (map #1 fs,
                                            map (map (fn s => str (Char.toUpper (String.sub (s, 3)))
                                                              ^ String.extract (s, 4, NONE))) ks))
                    else
                        raise Fail "Table name does not begin with uw_"
                end
              | DVal (x, n, _, e, _) =>
                let
                    (*val () = print ("\n=== " ^ x ^ " ===\n\n");*)

                    val isExptd = IS.member (exptd, n)

                    val saved = St.stash ()

                    fun deAbs (e, env, ps) =
                        case #1 e of
                            EAbs (_, _, _, e) =>
                            let
                                val nv = Var (St.nextVar ())
                            in
                                deAbs (e, nv :: env,
                                       if isExptd then
                                           AReln (Known, [nv]) :: ps
                                       else
                                           ps)
                            end
                          | _ => (e, env, ps)

                    val (e, env, ps) = deAbs (e, [], [])
                in
                    St.assert ps;
                    (evalExp env e (fn _ => ()) handle Cc.Contradiction => ());
                    St.reinstate saved
                end

              | DValRec [(x, n, _, e, _)] =>
                let
                    val tables = ref SS.empty
                    val cookies = ref SS.empty

                    fun deAbs (e, env, modes) =
                        case #1 e of
                            EAbs (_, _, _, e) => deAbs (e, Input (length env) :: env, ref Fixed :: modes)
                          | _ => (e, env, rev modes)

                    val (e, env, modes) = deAbs (e, [], [])

                    fun doExp env (e as (_, loc)) =
                        case #1 e of
                            EPrim _ => e
                          | ERel _ => e
                          | ENamed _ => e
                          | ECon (_, _, NONE) => e
                          | ECon (dk, pc, SOME e) => (ECon (dk, pc, SOME (doExp env e)), loc)
                          | ENone _ => e
                          | ESome (t, e) => (ESome (t, doExp env e), loc)
                          | EFfi _ => e
                          | EFfiApp (m, f, es) =>
                            (case (m, f, es) of
                                 ("Basis", "set_cookie", [_, ((EPrim (Prim.String cname), _), _), _, _, _]) =>
                                 cookies := SS.add (!cookies, cname)
                               | _ => ();
                             (EFfiApp (m, f, map (fn (e, t) => (doExp env e, t)) es), loc))

                          | EApp (e1, e2) =>
                            let
                                fun default () = (EApp (doExp env e1, doExp env e2), loc)

                                fun explore (e, args) =
                                    case #1 e of
                                        EApp (e1, e2) => explore (e1, e2 :: args)
                                      | ENamed n' =>
                                        if n' = n then
                                            let
                                                fun doArgs (pos, args, modes) =
                                                    case (args, modes) of
                                                        ((e1, _) :: args, m1 :: modes) =>
                                                        (case e1 of
                                                             ERel n =>
                                                             (case List.nth (env, n) of
                                                                  Input pos' =>
                                                                  if pos' = pos then
                                                                      ()
                                                                  else
                                                                      m1 := Arbitrary
                                                                | SubInput pos' =>
                                                                  if pos' = pos then
                                                                      if !m1 = Arbitrary then
                                                                          ()
                                                                      else
                                                                          m1 := Decreasing
                                                                  else
                                                                      m1 := Arbitrary
                                                                | Unknown => m1 := Arbitrary)
                                                           | _ => m1 := Arbitrary;
                                                         doArgs (pos + 1, args, modes))
                                                      | (_ :: _, []) => ()
                                                      | ([], ms) => app (fn m => m := Arbitrary) ms
                                            in
                                                doArgs (0, args, modes);
                                                (EFfi ("Basis", "?"), loc)
                                            end
                                        else
                                            default ()
                                      | _ => default ()
                            in
                                explore (e, [])
                            end
                          | EAbs (x, t1, t2, e) => (EAbs (x, t1, t2, doExp (Unknown :: env) e), loc)
                          | EUnop (uo, e1) => (EUnop (uo, doExp env e1), loc)
                          | EBinop (bi, bo, e1, e2) => (EBinop (bi, bo, doExp env e1, doExp env e2), loc)
                          | ERecord xets => (ERecord (map (fn (x, e, t) => (x, doExp env e, t)) xets), loc)
                          | EField (e1, f) => (EField (doExp env e1, f), loc)
                          | ECase (e, pes, ts) =>
                            let
                                val source =
                                    case #1 e of
                                        ERel n =>
                                        (case List.nth (env, n) of
                                             Input n => SOME n
                                           | SubInput n => SOME n
                                           | Unknown => NONE)
                                      | _ => NONE

                                fun doV v =
                                    let
                                        fun doPat (p, env) =
                                            case #1 p of
                                                PWild => env
                                              | PVar _ => v :: env
                                              | PPrim _ => env
                                              | PCon (_, _, NONE) => env
                                              | PCon (_, _, SOME p) => doPat (p, env)
                                              | PRecord xpts => foldl (fn ((_, p, _), env) => doPat (p, env)) env xpts
                                              | PNone _ => env
                                              | PSome (_, p) => doPat (p, env)
                                    in
                                        (ECase (e, map (fn (p, e) => (p, doExp (doPat (p, env)) e)) pes, ts), loc)
                                    end
                            in
                                case source of
                                    NONE => doV Unknown
                                  | SOME inp => doV (SubInput inp)
                            end
                          | EStrcat (e1, e2) => (EStrcat (doExp env e1, doExp env e2), loc)
                          | EError (e1, t) => (EError (doExp env e1, t), loc)
                          | EReturnBlob {blob = b, mimeType = m, t} =>
                            (EReturnBlob {blob = doExp env b, mimeType = doExp env m, t = t}, loc)
                          | ERedirect (e1, t) => (ERedirect (doExp env e1, t), loc)
                          | EWrite e1 => (EWrite (doExp env e1), loc)
                          | ESeq (e1, e2) => (ESeq (doExp env e1, doExp env e2), loc)
                          | ELet (x, t, e1, e2) => (ELet (x, t, doExp env e1, doExp (Unknown :: env) e2), loc)
                          | EClosure (n, es) => (EClosure (n, map (doExp env) es), loc)
                          | EQuery {exps, tables, state, query, body, initial} =>
                            (EQuery {exps = exps, tables = tables, state = state,
                                     query = doExp env query,
                                     body = doExp (Unknown :: Unknown :: env) body,
                                     initial = doExp env initial}, loc)
                          | EDml (e1, mode) =>
                            (case parse dml e1 of
                                 NONE => ()
                               | SOME c =>
                                 case c of
                                     Insert _ => ()
                                   | Delete (tab, _) =>
                                     tables := SS.add (!tables, tab)
                                   | Update (tab, _, _) =>
                                     tables := SS.add (!tables, tab);
                             (EDml (doExp env e1, mode), loc))
                          | ENextval e1 => (ENextval (doExp env e1), loc)
                          | ESetval (e1, e2) => (ESetval (doExp env e1, doExp env e2), loc)
                          | EUnurlify (e1, t, b) => (EUnurlify (doExp env e1, t, b), loc)
                          | EJavaScript (m, e) => (EJavaScript (m, doExp env e), loc)
                          | ESignalReturn _ => e
                          | ESignalBind _ => e
                          | ESignalSource _ => e
                          | EServerCall _ => e
                          | ERecv _ => e
                          | ESleep _ => e
                          | ESpawn _ => e

                    val e = doExp env e
                in
                    rfuns := IM.insert (!rfuns, n, {tables = !tables, cookies = !cookies,
                                                    args = map (fn r => !r) modes, body = e})
                end

              | DValRec _ => ErrorMsg.errorAt loc "Iflow can't check mutually-recursive functions yet."

              | DPolicy pol =>
                let
                    val rvN = ref 0
                    fun rv () =
                        let
                            val n = !rvN
                        in
                            rvN := n + 1;
                            Lvar n
                        end

                    val atoms = ref ([] : atom list)
                    fun doQ k = doQuery {Env = [],
                                         NextVar = rv,
                                         Add = fn a => atoms := a :: !atoms,
                                         Save = fn () => !atoms,
                                         Restore = fn ls => atoms := ls,
                                         Cont = SomeCol (fn r => k (rev (!atoms), r))}

                    fun untab (tab, nams) = List.filter (fn AReln (Sql tab', [Lvar lv]) =>
                                                            tab' <> tab
                                                            orelse List.all (fn Lvar lv' => lv' <> lv
                                                                              | _ => false) nams
                                                          | _ => true)
                in
                    case pol of
                        PolClient e =>
                        doQ (fn (ats, {Outs = es, ...}) => St.allowSend (ats, es)) e
                      | PolInsert e =>
                        doQ (fn (ats, {New = SOME (tab, new), ...}) =>
                                St.allowInsert (AReln (Sql (tab ^ "$New"), [new]) :: untab (tab, [new]) ats)
                              | _ => raise Fail "Iflow: No New in mayInsert policy") e
                      | PolDelete e =>
                        doQ (fn (ats, {Old = SOME (tab, old), ...}) =>
                                St.allowDelete (AReln (Sql (tab ^ "$Old"), [old]) :: untab (tab, [old]) ats)
                              | _ => raise Fail "Iflow: No Old in mayDelete policy") e
                      | PolUpdate e =>
                        doQ (fn (ats, {New = SOME (tab, new), Old = SOME (_, old), ...}) =>
                                St.allowUpdate (AReln (Sql (tab ^ "$Old"), [old])
                                                :: AReln (Sql (tab ^ "$New"), [new])
                                                :: untab (tab, [new, old]) ats)
                              | _ => raise Fail "Iflow: No New or Old in mayUpdate policy") e
                      | PolSequence e =>
                        (case #1 e of
                             EPrim (Prim.String seq) =>
                             let
                                 val p = AReln (Sql (String.extract (seq, 3, NONE)), [Lvar 0])
                                 val outs = [Lvar 0]
                             in
                                 St.allowSend ([p], outs)
                             end
                           | _ => ())
                end
                                        
              | _ => ()
    in
        app decl file
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
