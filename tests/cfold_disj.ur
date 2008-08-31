con id = fold (fn nm => fn t :: Type => fn acc => [nm] ~ acc => [nm = t] ++ acc) []

con idT = id [D = int, E = float]

val idV = fn x : $idT => x.E
