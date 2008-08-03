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

structure Monoize :> MONOIZE = struct

structure E = ErrorMsg
structure Env = CoreEnv

structure L = Core
structure L' = Mono

val dummyTyp = (L'.TDatatype (0, []), E.dummySpan)

fun monoName env (all as (c, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported name constructor";
             Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
             "")
    in
        case c of
            L.CName s => s
          | _ => poly ()
    end

fun monoType env (all as (c, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported type constructor";
             Print.eprefaces' [("Constructor", CorePrint.p_con env all)];
             dummyTyp)
    in
        case c of
            L.TFun (c1, c2) => (L'.TFun (monoType env c1, monoType env c2), loc)
          | L.TCFun _ => poly ()
          | L.TRecord (L.CRecord ((L.KType, _), xcs), _) =>
            (L'.TRecord (map (fn (x, t) => (monoName env x, monoType env t)) xcs), loc)
          | L.TRecord _ => poly ()

          | L.CApp ((L.CApp ((L.CApp ((L.CFfi ("Basis", "xml"), _), _), _), _), _), _) =>
            (L'.TFfi ("Basis", "string"), loc)

          | L.CRel _ => poly ()
          | L.CNamed n =>
            let
                val (_, xncs) = Env.lookupDatatype env n

                val xncs = map (fn (x, n, to) => (x, n, Option.map (monoType env) to)) xncs
            in
                (L'.TDatatype (n, xncs), loc)
            end
          | L.CFfi mx => (L'.TFfi mx, loc)
          | L.CApp _ => poly ()
          | L.CAbs _ => poly ()

          | L.CName _ => poly ()

          | L.CRecord _ => poly ()
          | L.CConcat _ => poly ()
          | L.CFold _ => poly ()
          | L.CUnit => poly ()
    end

val dummyExp = (L'.EPrim (Prim.Int 0), E.dummySpan)

structure IM = IntBinaryMap

datatype foo_kind =
         Attr
       | Url

fun fk2s fk =
    case fk of
        Attr => "attr"
      | Url => "url"

structure Fm :> sig
    type t

    val empty : int -> t

    val lookup : t -> foo_kind -> int -> (int -> t -> L'.decl * t) -> t * int
    val enter : t -> t
    val decls : t -> L'.decl list
end = struct

structure M = BinaryMapFn(struct
                          type ord_key = foo_kind
                          fun compare x =
                              case x of
                                  (Attr, Attr) => EQUAL
                                | (Attr, _) => LESS
                                | (_, Attr) => GREATER

                                | (Url, Url) => EQUAL
                          end)

type t = {
     count : int,
     map : int IM.map M.map,
     decls : L'.decl list
}

fun empty count = {
    count = count,
    map = M.empty,
    decls = []
}

fun enter ({count, map, ...} : t) = {count = count, map = map, decls = []}
fun decls ({decls, ...} : t) = decls

fun lookup (t as {count, map, decls}) k n thunk =
    let
        val im = Option.getOpt (M.find (map, k), IM.empty)
    in
        case IM.find (im, n) of
            NONE =>
            let
                val n' = count
                val (d, {count, map, decls}) = thunk count {count = count + 1,
                                                            map = M.insert (map, k, IM.insert (im, n, n')),
                                                            decls = decls}
            in
                ({count = count,
                  map = map,
                  decls = d :: decls}, n')
            end
          | SOME n' => (t, n')
    end

end
                

fun fooifyExp fk env =
    let
        fun fooify fm (e, tAll as (t, loc)) =
            case #1 e of
                L'.EClosure (fnam, [(L'.ERecord [], _)]) =>
                let
                    val (_, _, _, s) = Env.lookupENamed env fnam
                in
                    ((L'.EPrim (Prim.String s), loc), fm)
                end
              | L'.EClosure (fnam, args) =>
                let
                    val (_, ft, _, s) = Env.lookupENamed env fnam
                    val ft = monoType env ft

                    fun attrify (args, ft, e, fm) =
                        case (args, ft) of
                            ([], _) => (e, fm)
                          | (arg :: args, (L'.TFun (t, ft), _)) =>
                            let
                                val (arg', fm) = fooify fm (arg, t)
                            in
                                attrify (args, ft,
                                         (L'.EStrcat (e,
                                                      (L'.EStrcat ((L'.EPrim (Prim.String "/"), loc),
                                                                   arg'), loc)), loc),
                                         fm)
                            end
                          | _ => (E.errorAt loc "Type mismatch encoding attribute";
                                  (e, fm))
                in
                    attrify (args, ft, (L'.EPrim (Prim.String s), loc), fm)
                end
              | _ =>
                case t of
                    L'.TFfi ("Basis", "string") => ((L'.EFfiApp ("Basis", fk2s fk ^ "ifyString", [e]), loc), fm)
                  | L'.TFfi ("Basis", "int") => ((L'.EFfiApp ("Basis", fk2s fk ^ "ifyInt", [e]), loc), fm)
                  | L'.TFfi ("Basis", "float") => ((L'.EFfiApp ("Basis", fk2s fk ^ "ifyFloat", [e]), loc), fm)
                  | L'.TRecord [] => ((L'.EPrim (Prim.String ""), loc), fm)

                  | L'.TDatatype (i, _) =>
                    let
                        fun makeDecl n fm =
                            let
                                val (x, xncs) = Env.lookupDatatype env i

                                val (branches, fm) =
                                    ListUtil.foldlMap
                                        (fn ((x, n, to), fm) =>
                                            case to of
                                                NONE =>
                                                (((L'.PCon (L'.PConVar n, NONE), loc),
                                                  (L'.EPrim (Prim.String x), loc)),
                                                 fm)
                                              | SOME t =>
                                                let
                                                    val (arg, fm) = fooify fm ((L'.ERel 0, loc),
                                                                               monoType env t)
                                                in
                                                    (((L'.PCon (L'.PConVar n, SOME (L'.PVar "a", loc)), loc),
                                                      (L'.EStrcat ((L'.EPrim (Prim.String (x ^ "/")), loc),
                                                                   arg), loc)),
                                                     fm)
                                                end)
                                        fm xncs

                                val dom = tAll
                                val ran = (L'.TFfi ("Basis", "string"), loc)
                            in
                                ((L'.DValRec [(fk2s fk ^ "ify_" ^ x,
                                               n,
                                               (L'.TFun (dom, ran), loc),
                                               (L'.EAbs ("x",
                                                         dom,
                                                         ran,
                                                         (L'.ECase ((L'.ERel 0, loc),
                                                                    branches,
                                                                    ran), loc)), loc),
                                               "")], loc),
                                 fm)
                            end       

                        val (fm, n) = Fm.lookup fm fk i makeDecl
                    in
                        ((L'.EApp ((L'.ENamed n, loc), e), loc), fm)
                    end

                  | _ => (E.errorAt loc "Don't know how to encode attribute type";
                          Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty tAll)];
                          (dummyExp, fm))
    in
        fooify
    end

val attrifyExp = fooifyExp Attr
val urlifyExp = fooifyExp Url

datatype 'a failable_search =
         Found of 'a
       | NotFound
       | Error

structure St :> sig
    type t

    val empty : t

    val radioGroup : t -> string option
    val setRadioGroup : t * string -> t
end = struct

type t = {
     radioGroup : string option
}

val empty = {radioGroup = NONE}

fun radioGroup (t : t) = #radioGroup t

fun setRadioGroup (t : t, x) = {radioGroup = SOME x}

end

fun monoPatCon pc =
    case pc of
        L.PConVar n => L'.PConVar n
      | L.PConFfi mx => L'.PConFfi mx

fun monoPat (p, loc) =
    case p of
        L.PWild => (L'.PWild, loc)
      | L.PVar x => (L'.PVar x, loc)
      | L.PPrim p => (L'.PPrim p, loc)
      | L.PCon (pc, po) => (L'.PCon (monoPatCon pc, Option.map monoPat po), loc)
      | L.PRecord xps => (L'.PRecord (map (fn (x, p) => (x, monoPat p)) xps), loc)

fun monoExp (env, st, fm) (all as (e, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported expression";
             Print.eprefaces' [("Expression", CorePrint.p_exp env all)];
             (dummyExp, fm))
    in
        case e of
            L.EPrim p => ((L'.EPrim p, loc), fm)
          | L.ERel n => ((L'.ERel n, loc), fm)
          | L.ENamed n => ((L'.ENamed n, loc), fm)
          | L.ECon (n, eo) =>
            let
                val (eo, fm) =
                    case eo of
                        NONE => (NONE, fm)
                      | SOME e =>
                        let
                            val (e, fm) = monoExp (env, st, fm) e
                        in
                            (SOME e, fm)
                        end
            in
                ((L'.ECon (n, eo), loc), fm)
            end
          | L.EFfi mx => ((L'.EFfi mx, loc), fm)
          | L.EFfiApp (m, x, es) =>
            let
                val (es, fm) = ListUtil.foldlMap (fn (e, fm) => monoExp (env, st, fm) e) fm es
            in
                ((L'.EFfiApp (m, x, es), loc), fm)
            end

          | L.EApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "cdata"), _), _), _),
             _), _),
            se) =>
            let
                val (se, fm) = monoExp (env, st, fm) se
            in
                ((L'.EFfiApp ("Basis", "htmlifyString", [se]), loc), fm)
            end

          | L.EApp (
            (L.EApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.EFfi ("Basis", "join"),
                     _), _), _),
                _), _),
               _), _),
              _), _),
             xml1), _),
            xml2) =>
            let
                val (xml1, fm) = monoExp (env, st, fm) xml1
                val (xml2, fm) = monoExp (env, st, fm) xml2
            in
                ((L'.EStrcat (xml1, xml2), loc), fm)
            end

          | L.EApp (
            (L.EApp (
             (L.EApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.ECApp (
                   (L.ECApp (
                    (L.ECApp (
                     (L.ECApp (
                      (L.EFfi ("Basis", "tag"),
                       _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _),
              attrs), _),
             tag), _),
            xml) =>
            let
                fun getTag' (e, _) =
                    case e of
                        L.EFfi ("Basis", tag) => (tag, [])
                      | L.ECApp (e, t) => let
                            val (tag, ts) = getTag' e
                        in
                            (tag, ts @ [t])
                        end
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              ("", []))

                fun getTag (e, _) =
                    case e of
                        L.EFfiApp ("Basis", tag, [(L.ERecord [], _)]) => (tag, [])
                      | L.EApp (e, (L.ERecord [], _)) => getTag' e
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              ("", []))

                val (tag, targs) = getTag tag

                val (attrs, fm) = monoExp (env, st, fm) attrs

                fun tagStart tag =
                    case #1 attrs of
                        L'.ERecord xes =>
                        let
                            fun lowercaseFirst "" = ""
                              | lowercaseFirst s = str (Char.toLower (String.sub (s, 0)))
                                                   ^ String.extract (s, 1, NONE)

                            val s = (L'.EPrim (Prim.String (String.concat ["<", tag])), loc)
                        in
                            foldl (fn ((x, e, t), (s, fm)) =>
                                      let
                                          val xp = " " ^ lowercaseFirst x ^ "=\""

                                          val fooify =
                                              case x of
                                                  "Link" => urlifyExp
                                                | "Action" => urlifyExp
                                                | _ => attrifyExp

                                          val (e, fm) = fooify env fm (e, t)
                                      in
                                          ((L'.EStrcat (s,
                                                        (L'.EStrcat ((L'.EPrim (Prim.String xp), loc),
                                                                     (L'.EStrcat (e,
                                                                                  (L'.EPrim (Prim.String "\""),
                                                                                   loc)),
                                                                      loc)),
                                                         loc)), loc),
                                           fm)
                                      end)
                                  (s, fm) xes
                        end
                      | _ => raise Fail "Non-record attributes!"

                fun input typ =
                    case targs of
                        [_, (L.CName name, _)] =>
                        let
                            val (ts, fm) = tagStart "input"
                        in
                            ((L'.EStrcat (ts,
                                          (L'.EPrim (Prim.String (" type=\"" ^ typ ^ "\" name=\"" ^ name ^ "\"/>")),
                                           loc)), loc), fm)
                        end
                      | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                              raise Fail "No name passed to input tag")

                fun normal (tag, extra) =
                    let
                        val (tagStart, fm) = tagStart tag
                        val tagStart = case extra of
                                           NONE => tagStart
                                         | SOME extra => (L'.EStrcat (tagStart, extra), loc)

                        fun normal () =
                            let
                                val (xml, fm) = monoExp (env, st, fm) xml
                            in
                                ((L'.EStrcat ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String ">"), loc)), loc),
                                              (L'.EStrcat (xml,
                                                           (L'.EPrim (Prim.String (String.concat ["</", tag, ">"])),
                                                            loc)), loc)),
                                  loc),
                                 fm)
                            end
                    in
                        case xml of
                            (L.EApp ((L.ECApp (
                                      (L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                                                _), _),
                                      _), _),
                                     (L.EPrim (Prim.String s), _)), _) =>
                            if CharVector.all Char.isSpace s then
                                ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String "/>"), loc)), loc), fm)
                            else
                                normal ()
                          | _ => normal ()
                    end
            in
                case tag of
                    "submit" => ((L'.EPrim (Prim.String "<input type=\"submit\"/>"), loc), fm)

                  | "textbox" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "input"
                         in
                             ((L'.EStrcat (ts,
                                           (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\"/>")),
                                            loc)), loc), fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to textarea tag"))
                  | "password" => input "password"
                  | "ltextarea" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "textarea"
                             val (xml, fm) = monoExp (env, st, fm) xml
                         in
                             ((L'.EStrcat ((L'.EStrcat (ts,
                                                        (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                           (L'.EStrcat (xml,
                                                        (L'.EPrim (Prim.String "</textarea>"),
                                                         loc)), loc)),
                               loc), fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to ltextarea tag"))

                  | "radio" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         monoExp (env, St.setRadioGroup (st, name), fm) xml
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to radio tag"))
                  | "radioOption" =>
                    (case St.radioGroup st of
                         NONE => raise Fail "No name for radioGroup"
                       | SOME name =>
                         normal ("input",
                                 SOME (L'.EPrim (Prim.String (" type=\"radio\" name=\"" ^ name ^ "\"")), loc)))

                  | "lselect" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         let
                             val (ts, fm) = tagStart "select"
                             val (xml, fm) = monoExp (env, st, fm) xml
                         in
                             ((L'.EStrcat ((L'.EStrcat (ts,
                                                        (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                           (L'.EStrcat (xml,
                                                        (L'.EPrim (Prim.String "</select>"),
                                                         loc)), loc)),
                               loc),
                              fm)
                         end
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to lselect tag"))

                  | "loption" => normal ("option", NONE)

                  | _ => normal (tag, NONE)
            end

          | L.EApp ((L.ECApp (
                     (L.ECApp ((L.EFfi ("Basis", "lform"), _), _), _),
                     _), _),
                    xml) =>
            let
                fun findSubmit (e, _) =
                    case e of
                        L.EApp (
                        (L.EApp (
                         (L.ECApp (
                          (L.ECApp (
                           (L.ECApp (
                            (L.ECApp (
                             (L.EFfi ("Basis", "join"),
                              _), _), _),
                            _), _),
                           _), _),
                          _), _),
                         xml1), _),
                        xml2) => (case findSubmit xml1 of
                                      Error => Error
                                    | NotFound => findSubmit xml2
                                    | Found e =>
                                      case findSubmit xml2 of
                                          NotFound => Found e
                                        | _ => Error)
                      | L.EApp (
                        (L.EApp (
                         (L.EApp (
                          (L.ECApp (
                           (L.ECApp (
                            (L.ECApp (
                             (L.ECApp (
                              (L.ECApp (
                               (L.ECApp (
                                (L.ECApp (
                                 (L.ECApp (
                                  (L.EFfi ("Basis", "tag"),
                                   _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _), _),
                          attrs), _),
                         _), _),
                        xml) =>
                        (case #1 attrs of
                             L.ERecord xes =>
                             (case ListUtil.search (fn ((L.CName "Action", _), e, t) => SOME (e, t)
                                                     | _ => NONE) xes of
                                  NONE => findSubmit xml
                                | SOME et =>
                                  case findSubmit xml of
                                      NotFound => Found et
                                    | _ => Error)
                           | _ => findSubmit xml)
                      | _ => NotFound

                val (action, actionT) = case findSubmit xml of
                    NotFound => raise Fail "No submit found"
                  | Error => raise Fail "Not ready for multi-submit lforms yet"
                  | Found et => et

                val actionT = monoType env actionT
                val (action, fm) = monoExp (env, st, fm) action
                val (action, fm) = urlifyExp env fm (action, actionT)
                val (xml, fm) = monoExp (env, st, fm) xml
            in
                ((L'.EStrcat ((L'.EStrcat ((L'.EPrim (Prim.String "<form action=\""), loc),
                                           (L'.EStrcat (action,
                                                        (L'.EPrim (Prim.String "\">"), loc)), loc)), loc),
                              (L'.EStrcat (xml,
                                           (L'.EPrim (Prim.String "</form>"), loc)), loc)), loc),
                 fm)
            end

          | L.EApp ((L.ECApp (
                     (L.ECApp (
                      (L.ECApp (
                       (L.ECApp (
                        (L.EFfi ("Basis", "useMore"), _), _), _),
                       _), _),
                      _), _),
                     _), _),
                    xml) => monoExp (env, st, fm) xml

          | L.EApp (e1, e2) =>
            let
                val (e1, fm) = monoExp (env, st, fm) e1
                val (e2, fm) = monoExp (env, st, fm) e2
            in
                ((L'.EApp (e1, e2), loc), fm)
            end
          | L.EAbs (x, dom, ran, e) =>
            let
                val (e, fm) = monoExp (Env.pushERel env x dom, st, fm) e
            in
                ((L'.EAbs (x, monoType env dom, monoType env ran, e), loc), fm)
            end
          | L.ECApp _ => poly ()
          | L.ECAbs _ => poly ()

          | L.ERecord xes =>
            let
                val (xes, fm) = ListUtil.foldlMap
                                    (fn ((x, e, t), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, st, fm) e
                                        in
                                            ((monoName env x,
                                              e,
                                              monoType env t), fm)
                                        end) fm xes
            in
                ((L'.ERecord xes, loc), fm)
            end
          | L.EField (e, x, _) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EField (e, monoName env x), loc), fm)
            end
          | L.ECut _ => poly ()
          | L.EFold _ => poly ()

          | L.ECase (e, pes, t) =>
            let
                val (e, fm) = monoExp (env, st, fm) e
                val (pes, fm) = ListUtil.foldlMap
                                    (fn ((p, e), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, st, fm) e
                                        in
                                            ((monoPat p, e), fm)
                                        end) fm pes
            in
                ((L'.ECase (e, pes, monoType env t), loc), fm)
            end

          | L.EWrite e =>
            let
                val (e, fm) = monoExp (env, st, fm) e
            in
                ((L'.EWrite e, loc), fm)
            end

          | L.EClosure (n, es) =>
            let
                val (es, fm) = ListUtil.foldlMap (fn (e, fm) =>
                                                     monoExp (env, st, fm) e)
                               fm es
            in
                ((L'.EClosure (n, es), loc), fm)
            end
    end

fun monoDecl (env, fm) (all as (d, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported declaration";
             Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
             NONE)
    in
        case d of
            L.DCon _ => NONE
          | L.DDatatype (x, n, xncs) =>
            let
                val d = (L'.DDatatype (x, n, map (fn (x, n, to) => (x, n, Option.map (monoType env) to)) xncs), loc)
            in
                SOME (Env.declBinds env all, fm, d)
            end
          | L.DVal (x, n, t, e, s) =>
            let
                val (e, fm) = monoExp (env, St.empty, fm) e
            in
                SOME (Env.pushENamed env x n t NONE s,
                      fm,
                      (L'.DVal (x, n, monoType env t, e, s), loc))
            end
          | L.DValRec vis =>
            let
                val env = foldl (fn ((x, n, t, e, s), env) => Env.pushENamed env x n t NONE s) env vis

                val (vis, fm) = ListUtil.foldlMap
                                    (fn ((x, n, t, e, s), fm) =>
                                        let
                                            val (e, fm) = monoExp (env, St.empty, fm) e
                                        in
                                            ((x, n, monoType env t, e, s), fm)
                                        end)
                                    fm vis
            in
                SOME (env,
                      fm,
                      (L'.DValRec vis, loc))
            end
          | L.DExport (ek, n) =>
            let
                val (_, t, _, s) = Env.lookupENamed env n

                fun unwind (t, _) =
                    case t of
                        L.TFun (dom, ran) => dom :: unwind ran
                      | _ => []

                val ts = map (monoType env) (unwind t)
            in
                SOME (env, fm, (L'.DExport (ek, s, n, ts), loc))
            end
    end

fun monoize env ds =
    let
        val (_, _, ds) = List.foldl (fn (d, (env, fm, ds)) =>
                                     case monoDecl (env, fm) d of
                                         NONE => (env, fm, ds)
                                       | SOME (env, fm, d) =>
                                         (env,
                                          Fm.enter fm,
                                          d :: Fm.decls fm @ ds))
                                    (env, Fm.empty (CoreUtil.File.maxName ds + 1), []) ds
    in
        rev ds
    end

end
