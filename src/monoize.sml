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

val dummyTyp = (L'.TNamed 0, E.dummySpan)

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
          | L.CNamed n => (L'.TNamed n, loc)
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

fun fooifyExp name env =
    let
        fun fooify (e, tAll as (t, loc)) =
            case #1 e of
                L'.EClosure (fnam, [(L'.ERecord [], _)]) =>
                let
                    val (_, _, _, s) = Env.lookupENamed env fnam
                in
                    (L'.EPrim (Prim.String s), loc)
                end
              | L'.EClosure (fnam, args) =>
                let
                    val (_, ft, _, s) = Env.lookupENamed env fnam
                    val ft = monoType env ft

                    fun attrify (args, ft, e) =
                        case (args, ft) of
                            ([], _) => e
                          | (arg :: args, (L'.TFun (t, ft), _)) =>
                            attrify (args, ft,
                                     (L'.EStrcat (e,
                                                  (L'.EStrcat ((L'.EPrim (Prim.String "/"), loc),
                                                               fooify (arg, t)), loc)), loc))
                          | _ => (E.errorAt loc "Type mismatch encoding attribute";
                                  e)
                in
                    attrify (args, ft, (L'.EPrim (Prim.String s), loc))
                end
              | _ =>
                case t of
                    L'.TFfi ("Basis", "string") => (L'.EFfiApp ("Basis", name ^ "ifyString", [e]), loc)
                  | L'.TFfi ("Basis", "int") => (L'.EFfiApp ("Basis", name ^ "ifyInt", [e]), loc)
                  | L'.TFfi ("Basis", "float") => (L'.EFfiApp ("Basis", name ^ "ifyFloat", [e]), loc)
                  | L'.TRecord [] => (L'.EPrim (Prim.String ""), loc)

                  | _ => (E.errorAt loc "Don't know how to encode attribute type";
                          Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty tAll)];
                          dummyExp)
    in
        fooify
    end

val attrifyExp = fooifyExp "attr"
val urlifyExp = fooifyExp "url"

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

fun monoExp (env, st) (all as (e, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported expression";
             Print.eprefaces' [("Expression", CorePrint.p_exp env all)];
             dummyExp)
    in
        case e of
            L.EPrim p => (L'.EPrim p, loc)
          | L.ERel n => (L'.ERel n, loc)
          | L.ENamed n => (L'.ENamed n, loc)
          | L.EFfi mx => (L'.EFfi mx, loc)
          | L.EFfiApp (m, x, es) => (L'.EFfiApp (m, x, map (monoExp (env, st)) es), loc)

          | L.EApp (
            (L.ECApp (
             (L.ECApp ((L.EFfi ("Basis", "cdata"), _), _), _),
             _), _),
            se) => (L'.EFfiApp ("Basis", "htmlifyString", [monoExp (env, st) se]), loc)
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
            xml2) => (L'.EStrcat (monoExp (env, st) xml1, monoExp (env, st) xml2), loc)

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

                val attrs = monoExp (env, st) attrs

                fun tagStart tag =
                    case #1 attrs of
                        L'.ERecord xes =>
                        let
                            fun lowercaseFirst "" = ""
                              | lowercaseFirst s = str (Char.toLower (String.sub (s, 0)))
                                                   ^ String.extract (s, 1, NONE)

                            val s = (L'.EPrim (Prim.String (String.concat ["<", tag])), loc)
                        in
                            foldl (fn ((x, e, t), s) =>
                                      let
                                          val xp = " " ^ lowercaseFirst x ^ "=\""

                                          val fooify =
                                              case x of
                                                  "Link" => urlifyExp
                                                | "Action" => urlifyExp
                                                | _ => attrifyExp
                                      in
                                          (L'.EStrcat (s,
                                                       (L'.EStrcat ((L'.EPrim (Prim.String xp), loc),
                                                                    (L'.EStrcat (fooify env (e, t),
                                                                                 (L'.EPrim (Prim.String "\""),
                                                                                  loc)),
                                                                     loc)),
                                                        loc)), loc)
                                      end)
                                  s xes
                        end
                      | _ => raise Fail "Non-record attributes!"

                fun input typ =
                    case targs of
                        [_, (L.CName name, _)] =>
                        (L'.EStrcat (tagStart "input",
                                     (L'.EPrim (Prim.String (" type=\"" ^ typ ^ "\" name=\"" ^ name ^ "\"/>")),
                                      loc)), loc)
                      | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                              raise Fail "No name passed to input tag")

                fun normal (tag, extra) =
                    let
                        val tagStart = tagStart tag
                        val tagStart = case extra of
                                           NONE => tagStart
                                         | SOME extra => (L'.EStrcat (tagStart, extra), loc)

                        fun normal () =
                            (L'.EStrcat ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String ">"), loc)), loc),
                                         (L'.EStrcat (monoExp (env, st) xml,
                                                      (L'.EPrim (Prim.String (String.concat ["</", tag, ">"])),
                                                       loc)), loc)),
                             loc)
                    in
                        case xml of
                            (L.EApp ((L.ECApp (
                                      (L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                                                _), _),
                                      _), _),
                                     (L.EPrim (Prim.String s), _)), _) =>
                            if CharVector.all Char.isSpace s then
                                (L'.EStrcat (tagStart, (L'.EPrim (Prim.String "/>"), loc)), loc)
                            else
                                normal ()
                          | _ => normal ()
                    end
            in
                case tag of
                    "submit" => (L'.EPrim (Prim.String "<input type=\"submit\"/>"), loc)

                  | "textbox" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         (L'.EStrcat (tagStart "input",
                                      (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\"/>")),
                                       loc)), loc)
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to textarea tag"))
                  | "password" => input "password"
                  | "ltextarea" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         (L'.EStrcat ((L'.EStrcat (tagStart "textarea",
                                                   (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                      (L'.EStrcat (monoExp (env, st) xml,
                                                   (L'.EPrim (Prim.String "</textarea>"),
                                                    loc)), loc)),
                          loc)
                       | _ => (Print.prefaces "Targs" (map (fn t => ("T", CorePrint.p_con env t)) targs);
                               raise Fail "No name passed to ltextarea tag"))

                  | "radio" =>
                    (case targs of
                         [_, (L.CName name, _)] =>
                         monoExp (env, St.setRadioGroup (st, name)) xml
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
                         (L'.EStrcat ((L'.EStrcat (tagStart "select",
                                                   (L'.EPrim (Prim.String (" name=\"" ^ name ^ "\">")), loc)), loc),
                                      (L'.EStrcat (monoExp (env, st) xml,
                                                   (L'.EPrim (Prim.String "</select>"),
                                                    loc)), loc)),
                          loc)
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
                val action = monoExp (env, st) action
            in
                (L'.EStrcat ((L'.EStrcat ((L'.EPrim (Prim.String "<form action=\""), loc),
                                          (L'.EStrcat (urlifyExp env (action, actionT),
                                                       (L'.EPrim (Prim.String "\">"), loc)), loc)), loc),
                             (L'.EStrcat (monoExp (env, st) xml,
                                          (L'.EPrim (Prim.String "</form>"), loc)), loc)), loc)
            end

          | L.EApp ((L.ECApp (
                     (L.ECApp (
                      (L.ECApp (
                       (L.ECApp (
                        (L.EFfi ("Basis", "useMore"), _), _), _),
                       _), _),
                      _), _),
                     _), _),
                    xml) => monoExp (env, st) xml
                     

          | L.EApp (e1, e2) => (L'.EApp (monoExp (env, st) e1, monoExp (env, st) e2), loc)
          | L.EAbs (x, dom, ran, e) =>
            (L'.EAbs (x, monoType env dom, monoType env ran, monoExp (Env.pushERel env x dom, st) e), loc)
          | L.ECApp _ => poly ()
          | L.ECAbs _ => poly ()

          | L.ERecord xes => (L'.ERecord (map (fn (x, e, t) => (monoName env x,
                                                                monoExp (env, st) e,
                                                                monoType env t)) xes), loc)
          | L.EField (e, x, _) => (L'.EField (monoExp (env, st) e, monoName env x), loc)
          | L.ECut _ => poly ()
          | L.EFold _ => poly ()
          | L.EWrite e => (L'.EWrite (monoExp (env, st) e), loc)

          | L.EClosure (n, es) => (L'.EClosure (n, map (monoExp (env, st)) es), loc)
    end

fun monoDecl env (all as (d, loc)) =
    let
        fun poly () =
            (E.errorAt loc "Unsupported declaration";
             Print.eprefaces' [("Declaration", CorePrint.p_decl env all)];
             NONE)
    in
        case d of
            L.DCon _ => NONE
          | L.DVal (x, n, t, e, s) => SOME (Env.pushENamed env x n t (SOME e) s,
                                            (L'.DVal (x, n, monoType env t, monoExp (env, St.empty) e, s), loc))
          | L.DValRec vis =>
            let
                val env = foldl (fn ((x, n, t, e, s), env) => Env.pushENamed env x n t NONE s) env vis
            in
                SOME (env,
                      (L'.DValRec (map (fn (x, n, t, e, s) => (x, n, monoType env t,
                                                               monoExp (env, St.empty) e, s)) vis), loc))
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
                SOME (env, (L'.DExport (ek, s, n, ts), loc))
            end
    end

fun monoize env ds =
    let
        val (_, ds) = List.foldl (fn (d, (env, ds)) =>
                                     case monoDecl env d of
                                         NONE => (env, ds)
                                       | SOME (env, d) => (env, d :: ds)) (env, []) ds
    in
        rev ds
    end

end
