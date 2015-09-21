signature MONO_FM = sig
    type t

    type vr = string * int * Mono.typ * Mono.exp * string

    datatype foo_kind =
             Attr
             | Url

    val empty : int -> t

    val lookup : t -> foo_kind -> int -> (int -> t -> vr * t) -> t * int
    val lookupList : t -> foo_kind -> Mono.typ -> (int -> t -> vr * t) -> t * int
    val enter : t -> t
    val decls : t -> Mono.decl list

    val freshName : t -> int * t

    (* TODO: don't expose raw references if possible. *)
    val nextPvar : int ref
    val postMonoize : t ref
end
