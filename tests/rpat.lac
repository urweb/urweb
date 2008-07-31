val f = fn x : {A : int} => case x of {A = _} => 0
val f = fn x : {A : int} => case x of {A = _, ...} => 0
val f = fn x : {A : int, B : int} => case x of {A = _, ...} => 0
val f = fn x : {A : int, B : int} => case x of {A = 1, B = 2} => 0 | {A = _, ...} => 1

datatype t = A | B

val f = fn x => case x of {A = A, B = 2} => 0 | {A = A, ...} => 0 | {A = B, ...} => 0

val f = fn x => case x of {A = {A = A, ...}, B = B} => 0
        | {B = A, ...} => 1
        | {A = {A = B, B = A}, B = B} => 2
        | {A = {A = B, B = B}, B = B} => 3
