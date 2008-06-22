structure S = struct
        type t = int
        val x = 0

        structure S' : sig type u val y : t end = struct
                type u = t
                val y = x
        end

        signature Sig = sig
                type t
                val x : t
        end
end

open S.S'
open S
open S'

structure S' : Sig = S
