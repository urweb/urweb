con currier = fold (fn nm => fn t => fn acc => t -> acc) {}

con greenCurryIngredients :: {Type} = []
con greenCurry = currier greenCurryIngredients
val greenCurry : greenCurry = {}

con redCurryIngredients = [A = int, B = string]
con redCurry = currier redCurryIngredients
val redCurry : redCurry = fn x : int => fn y : string => {}

con yellowCurryIngredients = [A = string, B = int, C = float]
con yellowCurry = currier yellowCurryIngredients
val yellowCurry : yellowCurry = fn x => fn y => fn z => {}

val main = yellowCurry
