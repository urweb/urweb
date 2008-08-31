val id1 = fn n : int => n
val id2 = fn n => id1 n

val pair1 = fn (t1 ::: Type) (t2 ::: Type) (x1 : t1) (x2 : t2) => (x1, x2)
val pair2 = fn (t1 ::: Type) (t2 ::: Type) (x1 : t1) (x2 : t2) () => pair1 x1 x2

val id3 n = id2 n
val id4 n : int = id3 n
val id5 (n : int) = id4 n
val id6 (n : int) : int = id5 n

val id1 (t ::: Type) (x : t) = x
val id2 (t ::: Type) (x : t) : t = id1 x
