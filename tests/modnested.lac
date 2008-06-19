signature S = sig
        type t
        val x : t

        structure Q : sig
                type q
                val y : q

                structure V : sig
                        type v
                end
        end
end

structure S = struct
        type t = int
        val x = 0

        structure Q = struct
                type q = float
                val y = 0.0

                structure V = struct
                        type v = string
                        val hi = "Hi"
                end
        end
end

structure S1 = S
structure S2 : S = S
structure S3 = S2

val main = S3.Q.y
