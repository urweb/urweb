structure M : sig datatype t = A | B end = struct
        datatype t = A | B
end

val ac = M.A

datatype u = datatype M.t

val ac : M.t = A
val a2 : u = ac

structure M2 = M
structure M3 : sig datatype t = datatype M.t end = M2
structure M4 : sig datatype t = datatype M.t end = M

val bc : M3.t = M4.B

structure Ma : sig type t end = M

structure Magain : sig datatype t = A | B end = M

val page : M.t -> page = fn x => <html><body>
        Hi.
</body></html>

val main : unit -> page = fn () => <html><body>
        <a link={page a2}>Link</a>
</body></html>
