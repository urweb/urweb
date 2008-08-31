val x = 0
val x = x

type t = int
type t = { A : t }

signature S = sig end
signature S = sig type t structure M : S end

structure S = struct end
structure S : S = struct type t = int structure M = S end
