structure ToyCache : sig
    val cache : Cache.cache
end = struct


(* Mono *)

open Mono

val dummyLoc = ErrorMsg.dummySpan
val stringTyp = (TFfi ("Basis", "string"), dummyLoc)
val optionStringTyp = (TOption stringTyp, dummyLoc)
fun withTyp typ = map (fn exp => (exp, typ))

fun ffiAppCache' (func, index, argTyps) =
    EFfiApp ("Sqlcache", func ^ Int.toString index, argTyps)

fun check (index, keys) =
    ffiAppCache' ("check", index, withTyp stringTyp keys)

fun store (index, keys, value) =
    ffiAppCache' ("store", index, (value, stringTyp) :: withTyp stringTyp keys)

fun flush (index, keys) =
    ffiAppCache' ("flush", index, withTyp optionStringTyp keys)

fun lock (index, keys) =
    raise Fail "ToyCache doesn't yet implement lock"


(* Cjr *)

open Print
open Print.PD

fun setupQuery {index, params} =
    let

        val i = Int.toString index

        fun paramRepeat itemi sep =
            let
                fun f n =
                    if n < 0 then ""
                    else if n = 0 then itemi (Int.toString 0)
                    else f (n-1) ^ sep ^ itemi (Int.toString n)
            in
                f (params - 1)
            end

        fun paramRepeatInit itemi sep =
            if params = 0 then "" else sep ^ paramRepeat itemi sep

        val args = paramRepeatInit (fn p => "uw_Basis_string p" ^ p) ", "

        val decls = paramRepeat (fn p => "uw_Basis_string param" ^ i ^ "_"
                                         ^ p ^ " = NULL;")
                                "\n"

        val sets = paramRepeat (fn p => "param" ^ i ^ "_" ^ p
                                        ^ " = strdup(p" ^ p ^ ");")
                               "\n"

        val frees = paramRepeat (fn p => "free(param" ^ i ^ "_" ^ p ^ ");")
                                "\n"

        val eqs = paramRepeatInit (fn p => "strcmp(param" ^ i ^ "_" ^ p
                                           ^ ", p" ^ p ^ ")")
                                  " || "

        (* Using [!=] instead of [==] to mimic [strcmp]. *)
        val eqsNull = paramRepeatInit (fn p => "(p" ^ p ^ " == NULL || "
                                               ^ "!strcmp(param" ^ i ^ "_"
                                               ^ p ^ ", p" ^ p ^ "))")
                                      " && "

    in
        Print.box
            [string "static char *cacheQuery",
             string i,
             string " = NULL;",
             newline,
             string "static char *cacheWrite",
             string i,
             string " = NULL;",
             newline,
             string decls,
             newline,
             string "static uw_Basis_string uw_Sqlcache_check",
             string i,
             string "(uw_context ctx",
             string args,
             string ") {",
             newline,
             string "if (cacheWrite",
             string i,
             (* ASK: is returning the pointer okay? Should we duplicate? *)
             string " == NULL",
             string eqs,
             string ") {",
             newline,
             string "puts(\"SQLCACHE: miss ",
             string i,
             string ".\");",
             newline,
             string "uw_recordingStart(ctx);",
             newline,
             string "return NULL;",
             newline,
             string "} else {",
             newline,
             string "puts(\"SQLCACHE: hit ",
             string i,
             string ".\");",
             newline,
             string " if (cacheWrite",
             string i,
             string " != NULL) { uw_write(ctx, cacheWrite",
             string i,
             string "); }",
             newline,
             string "return cacheQuery",
             string i,
             string ";",
             newline,
             string "} };",
             newline,
             string "static uw_unit uw_Sqlcache_store",
             string i,
             string "(uw_context ctx, uw_Basis_string s",
             string args,
             string ") {",
             newline,
             string "free(cacheQuery",
             string i,
             string "); free(cacheWrite",
             string i,
             string ");",
             newline,
             string frees,
             newline,
             string "cacheQuery",
             string i,
             string " = strdup(s); cacheWrite",
             string i,
             string " = uw_recordingRead(ctx);",
             newline,
             string sets,
             newline,
             string "puts(\"SQLCACHE: store ",
             string i,
             string ".\");",
             newline,
             string "return uw_unit_v;",
             newline,
             string "};",
             newline,
             string "static uw_unit uw_Sqlcache_flush",
             string i,
             string "(uw_context ctx",
             string args,
             string ") {",
             newline,
             string "if (cacheQuery",
             string i,
             string " != NULL",
             string eqsNull,
             string ") {",
             newline,
             string "free(cacheQuery",
             string i,
             string ");",
             newline,
             string "cacheQuery",
             string i,
             string " = NULL;",
             newline,
             string "free(cacheWrite",
             string i,
             string ");",
             newline,
             string "cacheWrite",
             string i,
             string " = NULL;",
             newline,
             string "puts(\"SQLCACHE: flush ",
             string i,
             string ".\");}",
             newline,
             string "else { puts(\"SQLCACHE: keep ",
             string i,
             string ".\"); } return uw_unit_v;",
             newline,
             string "};",
             newline,
             newline]
    end

val setupGlobal = string "/* No global setup for toy cache. */"


(* Bundled up. *)

val cache = {check = check, store = store, flush = flush, lock = lock,
             setupQuery = setupQuery, setupGlobal = setupGlobal}

end
