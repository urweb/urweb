structure A = struct
    signature S = sig
        val x : int
    end 
end
structure B : A.S = struct
    val x = 42
end
