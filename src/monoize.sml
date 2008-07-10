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

          | L.CApp ((L.CFfi ("Basis", "xml"), _), _) => (L'.TFfi ("Basis", "string"), loc)

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

fun attrifyExp (e, tAll as (t, loc)) =
    case t of
        L'.TFfi ("Basis", "string") => e
      | L'.TFfi ("Basis", "int") => (L'.EFfiApp ("Basis", "attrifyInt", [e]), loc)
      | L'.TFfi ("Basis", "float") => (L'.EFfiApp ("Basis", "attrifyFloat", [e]), loc)
      | _ => (E.errorAt loc "Don't know how to encode attribute type";
              Print.eprefaces' [("Type", MonoPrint.p_typ MonoEnv.empty tAll)];
              dummyExp)

fun monoExp env (all as (e, loc)) =
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
          | L.EFfiApp (m, x, es) => (L'.EFfiApp (m, x, map (monoExp env) es), loc)

          | L.EApp ((L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                              _), _), se) => monoExp env se
          | L.EApp (
            (L.EApp (
             (L.ECApp (
              (L.ECApp (
               (L.ECApp (
                (L.EFfi ("Basis", "join"),
                 _), _), _),
               _), _),
              _), _),
             xml1), _),
            xml2) => (L'.EStrcat (monoExp env xml1, monoExp env xml2), loc)

          | L.EApp (
            (L.EApp (
             (L.EApp (
              (L.ECApp (
               (L.ECApp (
                (L.ECApp (
                 (L.ECApp (
                  (L.EFfi ("Basis", "tag"),
                   _), _), _), _), _), _), _), _), _),
              attrs), _),
             tag), _),
            xml) =>
            let
                fun getTag (e, _) =
                    case e of
                        L.EFfi ("Basis", tag) => tag
                      | _ => (E.errorAt loc "Non-constant XML tag";
                              Print.eprefaces' [("Expression", CorePrint.p_exp env tag)];
                              "")

                val tag = getTag tag

                val attrs = monoExp env attrs

                val tagStart =
                    case #1 attrs of
                        L'.ERecord xes =>
                        let
                            fun lowercaseFirst "" = ""
                              | lowercaseFirst s = str (Char.toLower (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

                            val s = (L'.EPrim (Prim.String (String.concat ["<", tag])), loc)
                        in
                            foldl (fn ((x, e, t), s) =>
                                      let
                                          val xp = " " ^ lowercaseFirst x ^ "=\""
                                      in
                                          (L'.EStrcat (s,
                                                       (L'.EStrcat ((L'.EPrim (Prim.String xp), loc),
                                                                    (L'.EStrcat (attrifyExp (e, t),
                                                                                 (L'.EPrim (Prim.String "\""), loc)),
                                                                     loc)),
                                                        loc)), loc)
                                      end)
                            s xes
                        end
                      | _ => raise Fail "Attributes!"

                fun normal () =
                    (L'.EStrcat ((L'.EStrcat (tagStart, (L'.EPrim (Prim.String ">"), loc)), loc),
                                 (L'.EStrcat (monoExp env xml,
                                              (L'.EPrim (Prim.String (String.concat ["</", tag, ">"])), loc)), loc)),
                     loc)


            in
                case xml of
                    (L.EApp ((L.ECApp ((L.EFfi ("Basis", "cdata"), _),
                                       _), _), (L.EPrim (Prim.String s), _)), _) =>
                    if CharVector.all Char.isSpace s then
                        (L'.EStrcat (tagStart, (L'.EPrim (Prim.String "/>"), loc)), loc)
                    else
                        normal ()
                  | _ => normal ()
            end

          | L.EApp (e1, e2) => (L'.EApp (monoExp env e1, monoExp env e2), loc)
          | L.EAbs (x, dom, ran, e) =>
            (L'.EAbs (x, monoType env dom, monoType env ran, monoExp (Env.pushERel env x dom) e), loc)
          | L.ECApp _ => poly ()
          | L.ECAbs _ => poly ()

          | L.ERecord xes => (L'.ERecord (map (fn (x, e, t) => (monoName env x, monoExp env e, monoType env t)) xes), loc)
          | L.EField (e, x, _) => (L'.EField (monoExp env e, monoName env x), loc)
          | L.EFold _ => poly ()
          | L.EWrite e => (L'.EWrite (monoExp env e), loc)
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
          | L.DVal (x, n, t, e) => SOME (Env.pushENamed env x n t (SOME e),
                                         (L'.DVal (x, n, monoType env t, monoExp env e), loc))
          | L.DPage ((c, _), e) =>
            (case c of
                 L.CRecord (_, vs) => SOME (env,
                                            (L'.DPage (map (fn (nm, t) => (monoName env nm,
                                                                           monoType env t)) vs,
                                                       monoExp env e), loc))
               | _ => poly ())
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
