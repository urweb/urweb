type team = string
type league = string

table team : { Id : team,
               League : league }

val foo:int = queryL(SELECT * FROM team)
