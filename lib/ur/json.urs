(** The JSON text-based serialization format (with YAML as a bonus) *)

class json

val toJson : a ::: Type -> json a -> a -> string
val fromJson : a ::: Type -> json a -> string -> a
val fromJson' : a ::: Type -> json a -> string -> a * string

val toYaml : a ::: Type -> json a -> a -> string
val fromYaml : a ::: Type -> json a -> string -> a

val mkJson : a ::: Type -> {ToJson : a -> string,
                            FromJson : string -> a * string} -> json a

val json_string : json string
val json_int : json int
val json_float : json float
val json_bool : json bool
val json_time : json time
val json_option : a ::: Type -> json a -> json (option a)
val json_list : a ::: Type -> json a -> json (list a)

(* By the way, time formatting follows RFC 3339, and we expose the more
 * primitive formatting functions here. *)
val rfc3339_out : time -> string
val rfc3339_in : string -> time

val json_record : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json $ts
val json_record_withOptional : ts ::: {Type} -> ots ::: {Type} -> [ts ~ ots]
                               => folder ts -> $(map json ts) -> $(map (fn _ => string) ts)
                               -> folder ots -> $(map json ots) -> $(map (fn _ => string) ots)
                               -> json $(ts ++ map option ots)
val json_variant : ts ::: {Type} -> folder ts -> $(map json ts) -> $(map (fn _ => string) ts) -> json (variant ts)

val json_unit : json unit

val json_dict : a ::: Type -> json a -> json (list (string * a))
(* Simple key-value list, encoded in YAML with keys as labels on lines *)

val json_derived : base ::: Type -> derived ::: Type
                   -> (base -> derived)
                   -> (derived -> base)
                   -> json base -> json derived

functor Recursive (M : sig
                       con t :: Type -> Type
                       val json_t : a ::: Type -> json a -> json (t a)
                   end) : sig
    datatype r = Rec of M.t r

    val json_r : json r
end

datatype prim = String of string | Int of int | Float of float | Bool of bool
val json_prim : json prim
val show_prim : show prim

