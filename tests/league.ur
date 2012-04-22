type team = string
type league = string

table team : { Id : team,
               League : league }

val foo = queryL(SELECT * FROM team)
val bar : int = "hi"
