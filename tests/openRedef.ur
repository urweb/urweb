structure M = struct
    con num = int
    val zero = 0
end

structure N = struct
    open M
    con num = num * num
    val zero = zero + 1
end

structure O = struct
    open N

    val one : num = (zero + 1, zero)
end
