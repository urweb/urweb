con func a b = a -> b

signature S = sig
    con funcy a b = a -> b
end

structure M : S = struct
    con funcy = func
end
