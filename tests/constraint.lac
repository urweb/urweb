signature S = sig
        con nm :: Name
        con r :: {Type}

        constraint [nm] ~ r
end

structure M : S = struct
        con nm = #A
        con r = [B = float, C = string]

        constraint [A] ~ [B]
        constraint [nm] ~ r
        constraint [C] ~ [D]
end

structure M' = struct
        open M

        con combo = [nm = int] ++ r
end

structure M' = struct
        open constraints M

        con nm' = M.nm
        con r' = M.r
        con combo = [nm' = int] ++ r'
end


signature S' = sig
        con r1 :: {Type}
        con r2 :: {Type}

        constraint r1 ~ r2
end

functor F (M : S) : S' = struct
        con r1 = [M.nm = int]
        con r2 = M.r

        open constraints M
        constraint r1 ~ r2
end
