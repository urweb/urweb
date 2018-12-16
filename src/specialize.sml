(* Copyright (c) 2008-2010, Adam Chlipala
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

(* Simplify a Core program by repeating polymorphic definitions of datatypes *)

structure Specialize :> SPECIALIZE = struct

open Core

structure E = CoreEnv
structure U = CoreUtil

val liftConInCon = E.liftConInCon
val subConInCon = E.subConInCon

structure CK = struct
type ord_key = con list
val compare = Order.joinL U.Con.compare
end

structure CM = BinaryMapFn(CK)
structure IM = IntBinaryMap
structure IS = IntBinarySet

type datatyp' = {
     name : int,
     constructors : int IM.map
}

type datatyp = {
     name : string,
     params : int,
     constructors : (string * int * con option) list,
     specializations : datatyp' CM.map
}

type state = {
     count : int,
     datatypes : datatyp IM.map,
     constructors : int IM.map,
     decls : (string * int * string list * (string * int * con option) list) list
}

fun kind (k, st) = (k, st)

val isOpen = U.Con.exists {kind = fn _ => false,
                           con = fn c =>
                                    case c of
                                        CRel _ => true
                                      | _ => false}

fun findApp (c, args) =
    case c of
        CApp ((c', _), arg) => findApp (c', arg :: args)
      | CNamed n => SOME (n, args)
              | _ => NONE
                          
fun considerSpecialization (st : state, n, args, dt : datatyp) =
    let
        val args = map ReduceLocal.reduceCon args
    in
        case CM.find (#specializations dt, args) of
            SOME dt' => (#name dt', #constructors dt', st)
          | NONE =>
            let
                (*val () = Print.prefaces "Args" [("n", Print.PD.string (Int.toString n)),
                                                ("args", Print.p_list (CorePrint.p_con CoreEnv.empty) args)]*)

                val n' = #count st

                val nxs = length args - 1
                fun sub t = ListUtil.foldli (fn (i, arg, t) =>
                                                subConInCon (nxs - i, arg) t) t args

                val (cons, (count, cmap)) =
                    ListUtil.foldlMap (fn ((x, n, to), (count, cmap)) =>
                                          let
                                              val to = Option.map sub to
                                          in
                                              ((x, count, to),
                                               (count + 1,
                                                IM.insert (cmap, n, count)))
                                          end) (n' + 1, IM.empty) (#constructors dt)

                val st = {count = count,
                          datatypes = IM.insert (#datatypes st, n,
                                                 {name = #name dt,
                                                  params = #params dt,
                                                  constructors = #constructors dt,
                                                  specializations = CM.insert (#specializations dt,
                                                                               args,
                                                                               {name = n',
                                                                                constructors = cmap})}),
                          constructors = #constructors st,
                          decls = #decls st}

                val (cons, st) = ListUtil.foldlMap (fn ((x, n, NONE), st) => ((x, n, NONE), st)
                                                     | ((x, n, SOME t), st) =>
                                                       let
                                                           val (t, st) = specCon st t
                                                       in
                                                           ((x, n, SOME t), st)
                                                       end) st cons

                val dt = (#name dt ^ "_s",
                          n',
                          [],
                          cons)
            in
                (n', cmap, {count = #count st,
                            datatypes = #datatypes st,
                            constructors = #constructors st,
                            decls = dt :: #decls st})
            end
    end

and con (c, st : state) =
    case findApp (c, []) of
        SOME (n, args as ((_, loc) :: _)) =>
        (case IM.find (#datatypes st, n) of
             NONE => (c, st)
           | SOME dt =>
             if length args <> #params dt then
                 (c, st)
             else
                 let
                     val (n, _, st) = considerSpecialization (st, n, args, dt)
                 in
                     (CNamed n, st)
                 end)
      | _ => (c, st)

and specCon st = U.Con.foldMap {kind = kind, con = con} st

fun pat (p, st) =
    case #1 p of
        PVar _ => (p, st)
      | PPrim _ => (p, st)
      | PCon (dk, PConVar pn, args as (_ :: _), po) =>
        let
            val (po, st) =
                case po of
                    NONE => (NONE, st)
                  | SOME p =>
                    let
                        val (p, st) = pat (p, st)
                    in
                        (SOME p, st)
                    end
            val p = (PCon (dk, PConVar pn, args, po), #2 p)
        in
            if List.exists isOpen args then
                (p, st)
            else
                case IM.find (#constructors st, pn) of
                    NONE => (p, st)
                  | SOME n =>
                    case IM.find (#datatypes st, n) of
                        NONE => (p, st)
                      | SOME dt =>
                        let
                            val (n, cmap, st) = considerSpecialization (st, n, args, dt)
                        in
                            case IM.find (cmap, pn) of
                                NONE => raise Fail "Specialize: Missing datatype constructor (pat)"
                              | SOME pn' => ((PCon (dk, PConVar pn', [], po), #2 p), st)
                        end
        end
      | PCon (dk, pc, args, SOME p') =>
        let
            val (p', st) = pat (p', st)
        in
            ((PCon (dk, pc, args, SOME p'), #2 p), st)
        end
      | PCon _ => (p, st)
      | PRecord xps =>
        let
            val (xps, st) = ListUtil.foldlMap (fn ((x, p, t), st) =>
                                                  let
                                                      val (p, st) = pat (p, st)
                                                  in
                                                      ((x, p, t), st)
                                                  end)
                            st xps
        in
            ((PRecord xps, #2 p), st)
        end

fun exp (e, st) =
    case e of
        ECon (dk, PConVar pn, args as (_ :: _), eo) =>
        if List.exists isOpen args then
            (e, st)
        else
            (case IM.find (#constructors st, pn) of
                 NONE => (e, st)
               | SOME n =>
                 case IM.find (#datatypes st, n) of
                     NONE => (e, st)
                   | SOME dt =>
                     let
                         val (n, cmap, st) = considerSpecialization (st, n, args, dt)
                     in
                         case IM.find (cmap, pn) of
                             NONE => raise Fail "Specialize: Missing datatype constructor"
                           | SOME pn' => (ECon (dk, PConVar pn', [], eo), st)
                     end)
      | ECase (e, pes, r) =>
        let
            val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                  let
                                                      val (p, st) = pat (p, st)
                                                  in
                                                      ((p, e), st)
                                                  end) st pes
        in
            (ECase (e, pes, r), st)
        end
      | _ => (e, st)

fun decl (d, st) = (d, st)

val specDecl = U.Decl.foldMap {kind = kind, con = con, exp = exp, decl = decl}

fun specialize file =
    let
        (*val () = CorePrint.debug := true
        val () = print "SPECIALIZING\n"*)
                
        (* Let's run around a file, finding any polymorphic uses of a datatype.
         * However, don't count polymorphism within a datatype's own definition!
         * To that end, we run a silly transform on the file before traversing. *)
        val file' =
            map (fn d =>
                    case #1 d of
                        DDatatype dts =>
                        U.Decl.map {kind = fn x => x,
                                    exp = fn x => x,
                                    decl = fn x => x,
                                    con = fn CNamed n =>
                                             if List.exists (fn (_, n', _, _) => n' = n) dts then
                                                 CUnit
                                             else
                                                 CNamed n
                                           | c => c} d
                      | _ => d) file

        val fancyDatatypes = U.File.fold {kind = fn (_, fd) => fd,
                                          exp = fn (_, fd) => fd,
                                          decl = fn (_, fd) => fd,
                                          con = fn (c, fd) =>
                                                   case c of
                                                       CApp (c1, c2) =>
                                                       if isOpen c2 then
                                                           case findApp (c, []) of
                                                               SOME (n, _) =>
                                                               ((*Print.preface ("Disqualifier",
                                                                               CorePrint.p_con CoreEnv.empty (c, ErrorMsg.dummySpan));*)
                                                                IS.add (fd, n))
                                                             | NONE => fd
                                                       else
                                                           fd
                                                    |  _ => fd}
                                         IS.empty file'

        (* Why did we find the polymorphism?
         * It would be incoherent to specialize a datatype used polymorphically. *)

        fun doDecl (d, st) =
            let
                (*val () = Print.preface ("decl:", CorePrint.p_decl CoreEnv.empty all)*)
                val (d, st) = specDecl st d
            in
                case #1 d of
                    DDatatype dts =>
                    if List.exists (fn (_, n, _, _) => IS.member (fancyDatatypes, n)) dts then
                        ((*Print.preface ("Skipping", CorePrint.p_decl CoreEnv.empty d);*)
                         ([d], st))
                    else
                        ((case #decls st of
                              [] => [d]
                            | dts' => [(DDatatype (dts' @ dts), #2 d)]),
                         {count = #count st,
                          datatypes = foldl (fn ((x, n, xs, xnts), dts) =>
                                                IM.insert (dts, n,
                                                           {name = x,
                                                            params = length xs,
                                                            constructors = xnts,
                                                            specializations = CM.empty}))
                                            (#datatypes st) dts,
                          constructors = foldl (fn ((x, n, xs, xnts), cs) =>
                                                   foldl (fn ((_, n', _), constructors) =>
                                                             IM.insert (constructors, n', n))
                                                     cs xnts)
                                               (#constructors st) dts,
                          decls = []})
                  | _ =>
                    (case #decls st of
                          [] => [d]
                        | dts => [(DDatatype dts, #2 d), d],
                     {count = #count st,
                      datatypes = #datatypes st,
                      constructors = #constructors st,
                      decls = []})
            end

        val (ds, _) = ListUtil.foldlMapConcat doDecl
                                              {count = U.File.maxName file + 1,
                                               datatypes = IM.empty,
                                               constructors = IM.empty,
                                               decls = []} file
    in
        ds
    end

end
