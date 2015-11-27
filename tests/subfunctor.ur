functor F(M : sig con fs :: {Type} end) = struct
    open M

    functor G(M : sig val x : $(map sql_injectable_prim fs) end) = struct
    end
end
