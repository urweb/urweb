signature S = sig
        type t
        val x : t
end

structure M : S = struct
        val x = 0
end

signature S = sig
        con r :: {Type}
        val x : $r
end

structure M : S = struct
        val x = {A = 0, B = "Hi"}
end

signature S = sig
        type t
        con r :: {Type}
        val x : t -> $r
end

structure M : S = struct
        val x = fn v : int => {A = 0, B = "Hi"}
end

signature S = sig
        con nm :: Name
        con t :: Type
        con r :: {Type}
        val x : $([nm = t] ++ r)
end

structure M : S = struct
        val x = {A = 0, B = "Hi"}
end

signature S = sig
        con nm :: Name
        con r :: {Type}
        val x : $([nm = int] ++ r)
end

structure M : S = struct
        val x = {A = 0, B = "Hi"}
end

signature S = sig
        con nm :: Name
        con r :: {Type}
        val x : $([nm = string] ++ r)
end

structure M : S = struct
        val x = {A = 0, B = "Hi"}
end
