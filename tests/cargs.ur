con id = fn t :: Type => t
con id2 = fn (t :: Type) => id t
con id3 = fn t => id2 t

con pair = fn (t :: Type) (u :: Type) => (t, u)
con pair2 = fn t u => pair t u
con pair3 = fn t (u :: Type) => pair2 t u

con id4 (t :: Type) = t
con id5 (t :: Type) :: Type = id4 t
con id6 t :: Type = id5 t

con pair4 t (u :: Type) = pair3 t u
con pair5 t (u :: Type) :: (Type * Type) = pair4 t u
