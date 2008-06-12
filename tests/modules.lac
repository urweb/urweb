signature A = sig end
structure A = struct end
structure Ao : A = A


structure B = struct
        type t = int
end
structure Bo0 : sig end = B
structure BoA : A = B

signature B1 = sig
        type t
end
structure Bo1 : B1 = B
(*structure AoB1 : B1 = A*)

signature B2 = sig
        type t = int
end
structure Bo2 : B2 = B


structure C = struct
        type t = float
end
structure CoB1 : B1 = C
(*structure CoB2 : B2 = C*)


signature NAT = sig
        type t
        val zero : t
end
structure Nat : NAT = struct
        type t = int
        val zero = 0
end
(*structure NotNat : NAT = struct
        type t = int
        val zero = 0.0
end*)
(*structure NotNat : NAT = struct
        val zero = 0
end*)


signature WOBBLE = sig
        type t
        type s
end
structure Wobble1 = struct
        type t = int
        type s = float
end
structure Wobble2 = struct
        type s = int
        type t = float
end


structure N = struct
        type t = string
        structure N2 = struct
                type t = int
                val zero = 0
        end
        val x = "Hi"
end
signature N = sig
        structure N2 : NAT
        type t
        val x : t
end
structure No : N = N
