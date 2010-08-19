val v1 : variant [A = int, B = float] = make [#A] 1
val v2 : variant [A = int, B = float] = make [#B] 2.3

fun main () = return (match v1 {A = fn n => <xml>A: {[n]}</xml>,
                                B = fn n => <xml>B: {[n]}</xml>})
