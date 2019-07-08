(* Copyright (c) 2013, Adam Chlipala
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

structure FileCache :> FILE_CACHE = struct

open Mono

structure SS = BinarySetFn(struct
                            type ord_key = string
                            val compare = String.compare
                            end)

val hasBlob =
    MonoUtil.Typ.exists (fn TFfi ("Basis", "blob") => true
                        | _ => false)

val unBlob =
    MonoUtil.Typ.map (fn TFfi ("Basis", "blob") => TFfi ("Basis", "string")
                       | t => t)

fun nodups (exps : (string * typ) list, tables : (string * (string * typ) list) list) =
    let
        val cols = map #1 exps @ ListUtil.mapConcat (map #1 o #2) tables

        val (_, good) =
            foldl (fn (name, (names, good)) =>
                      if SS.member(names, name) then
                          (names, false)
                      else
                          (SS.add (names, name), good)) (SS.empty, true) cols
    in
        good
    end

fun instrument file =
    let
        fun exp e =
            case e of
                EQuery {exps, tables, state, query, body, initial} =>
                if (List.exists (hasBlob o #2) exps
                    orelse List.exists (List.exists (hasBlob o #2) o #2) tables)
                   andalso nodups (exps, tables) then
                    let
                        val exps = ListMergeSort.sort
                                       (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER)
                                       exps
                        val tables = ListMergeSort.sort
                                         (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER)
                                         tables
                        val tables = map (fn (x, xts) =>
                                             (x, ListMergeSort.sort
                                                     (fn ((x, _), (y, _)) => String.compare (x, y) = GREATER)
                                                     xts)) tables

                        val loc = #2 query

                        fun wrapCol (name, t) =
                            case #1 t of
                                TFfi ("Basis", "blob") =>
                                (case #supportsSHA512 (Settings.currentDbms ()) of
                                     NONE => (ErrorMsg.error "DBMS doesn't support SHA512.";
                                              "ERROR")
                                   | SOME r => #GenerateHash r name)
                              | TOption t' => wrapCol (name, t')
                              | _ => name

                        val mangle = Settings.mangleSql

                        val cols = map (fn (name, t) => (mangle name, t)) exps
                                   @ ListUtil.mapConcat (fn (_, cols) =>
                                                            map (fn (name, t) =>
                                                                    (mangle name,
                                                                     t)) cols) tables

                        val prequery =
                            "SELECT "
                            ^ String.concatWith ", " (map wrapCol cols)
                            ^ " FROM ("

                        val postquery =
                            ") AS Wrap"

                        val wrapped_query =
                            (EStrcat ((EPrim (Prim.String (Prim.Normal, prequery)), loc),
                                      (EStrcat (query,
                                                (EPrim (Prim.String (Prim.Normal, postquery)), loc)), loc)), loc)
                        val wrapped_query = MonoOpt.optExp wrapped_query

                        val exps' = map (fn (name, t) => (name, unBlob t)) exps
                        val tables' = map (fn (name, cols) =>
                                              (name,
                                               map (fn (cname, t) => (cname, unBlob t)) cols)) tables

                        val blob = (TFfi ("Basis", "blob"), loc)
                        val string = (TFfi ("Basis", "string"), loc)

                        fun trycache (name, e, t : typ) =
                            (name,
                             case #1 t of
                                 TFfi ("Basis", "blob") =>
                                 (EFfiApp ("Basis",
                                           "check_filecache",
                                           [(e, string)]), loc)
                               | TOption (TFfi ("Basis", "blob"), _) =>
                                 (ECase (e,
                                         [((PNone string, loc),
                                           (ENone blob, loc)),
                                          ((PSome (string, (PVar ("hash", string), loc)), loc),
                                           (ESome (blob,
                                                   (EFfiApp ("Basis",
                                                             "check_filecache",
                                                             [((ERel 0, loc), string)]), loc)), loc))],
                                         {disc = (TOption string, loc),
                                          result = (TOption blob, loc)}), loc)
                               | _ => e,
                             t)

                        val wrapped_body_trycache =
                            (ELet ("uncached",
                                   (TRecord (exps @ map (fn (name, cols) =>
                                                            (name, (TRecord cols, loc))) tables),
                                    loc),
                                   (ERecord (map (fn (name, t) =>
                                                     trycache (name,
                                                               (EField ((ERel 1, loc),
                                                                        name), loc),
                                                               t)) exps
                                             @ map (fn (tname, cols) =>
                                                       (tname,
                                                        (ERecord (map (fn (name, t) =>
                                                                          trycache (name,
                                                                                    (EField ((EField ((ERel 1, loc), tname), loc), name), loc),
                                                                                    t)) cols), loc),
                                                        (TRecord cols, loc))) tables), loc),
                                   MonoEnv.subExpInExp (2, (ERel 0, loc))


                                                       (MonoEnv.liftExpInExp 0 body)), loc)

                        fun maybeadd (e, t, acc) =
                            case #1 t of
                                TFfi ("Basis", "blob") =>
                                (ESeq ((EFfiApp ("Basis",
                                                  "cache_file",
                                                  [(e, blob)]), loc),
                                       acc), loc)
                              | TOption (TFfi ("Basis", "blob"), _) =>
                                (ESeq ((ECase (e,
                                               [((PNone blob, loc),
                                                 (ERecord [], loc)),
                                                ((PSome (blob, (PVar ("blob", blob), loc)), loc),
                                                 (EFfiApp ("Basis",
                                                            "cache_file",
                                                            [((ERel 0, loc), blob)]), loc))],
                                               {disc = t,
                                                result = (TRecord [], loc)}), loc),
                                       acc), loc)
                              | _ => acc

                        val wrapped_body_addtocache =
                            foldl (fn ((name, t), e) =>
                                      maybeadd ((EField ((ERel 1, loc), name), loc),
                                                t, e))
                                  (foldl (fn ((tname, cols), e) =>
                                             foldl (fn ((name, t), e) =>
                                                       maybeadd ((EField ((EField ((ERel 1, loc), tname), loc), name), loc),
                                                                 t, e)) e cols) body tables)
                                  exps
                    in
                        ECase ((EFfiApp ("Basis", "filecache_missed", []), loc),
                               [((PCon (Enum,
                                        PConFfi {mod = "Basis",
                                                 datatyp = "bool",
                                                 con = "False",
                                                 arg = NONE},
                                        NONE), loc),
                                 (EQuery {exps = exps',
                                          tables = tables',
                                          state = state,
                                          query = wrapped_query,
                                          body = wrapped_body_trycache,
                                          initial = initial}, loc)),
                                ((PCon (Enum,
                                        PConFfi {mod = "Basis",
                                                 datatyp = "bool",
                                                 con = "True",
                                                 arg = NONE},
                                        NONE), loc),
                                 (EQuery {exps = exps,
                                          tables = tables,
                                          state = state,
                                          query = query,
                                          body = wrapped_body_addtocache,
                                          initial = initial}, loc))],
                               {disc = (TFfi ("Basis", "bool"), loc),
                                result = state})
                    end
                else
                    e
              | _ => e
    in
        case Settings.getFileCache () of
            NONE => file
          | SOME _ => MonoUtil.File.map {typ = fn t => t,
                                         exp = exp,
                                         decl = fn d => d} file
    end

end
