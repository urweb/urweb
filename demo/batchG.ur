table t : {Id : int, A : string, B : float}

open BatchFun.Make(struct
                       val tab = t
                       val title = "BatchG"
                       val cols = {A = BatchFun.string "A",
                                   B = BatchFun.float "B"}
                   end)
