signature MONO_FOOIFY = sig

(* TODO: don't expose raw references if possible. *)
val nextPvar : int ref
val pvarDefs : ((string * int * (string * int * Mono.typ option) list) list) ref

datatype foo_kind = Attr | Url

structure Fm : sig
    type t

    type vr = string * int * Mono.typ * Mono.exp * string

    val empty : int -> t

    val lookup : t -> foo_kind -> int -> (int -> t -> vr * t) -> t * int
    val lookupList : t -> foo_kind -> Mono.typ -> (int -> t -> vr * t) -> t * int
    val enter : t -> t
    (* This list should be reversed before adding to list of file declarations. *)
    val decls : t -> Mono.decl list

    val freshName : t -> int * t
end

(* General form used in [Monoize]. *)
val fooifyExp : foo_kind
                -> (int -> Mono.typ * string)
                -> (int -> string * (string * int * Mono.typ option) list)
                -> Fm.t
                -> Mono.exp * Mono.typ
                -> Mono.exp * Fm.t

(* Easy-to-use interface in [Sqlcache]. Uses [Fm.canonical]. *)
val canonicalFm : Fm.t ref (* Set at the end of [Monoize]. *)
val urlify : MonoEnv.env -> Mono.exp * Mono.typ -> Mono.exp option
(* This list should be reversed before adding to list of file declarations. *)
val getNewFmDecls : unit -> Mono.decl list

end
