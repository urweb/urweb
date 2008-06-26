val currier : rs :: {Type} -> Cfold.currier rs =
        fold [Cfold.currier] (fn nm :: Name => fn t :: Type => fn rest :: {Type} => fn acc => fn x : t => acc) {}

val greenCurry : Cfold.greenCurry = currier [Cfold.greenCurryIngredients]
val redCurry : Cfold.redCurry = currier [Cfold.redCurryIngredients]
val yellowCurry : Cfold.yellowCurry = currier [Cfold.yellowCurryIngredients]

val main = yellowCurry
