functor F(M : sig con fs :: {Type} end) : sig
    functor G(M : sig val x : $(map sql_injectable_prim M.fs) end) : sig end
end
