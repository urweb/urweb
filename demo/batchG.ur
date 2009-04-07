table t : {Id : int, A : string, B : float}
  PRIMARY KEY Id

open BatchFun.Make(struct
                       val tab = t
                       val title = "BatchG"
                       val cols = {A = BatchFun.string "A",
                                   B = BatchFun.float "B"}
                   end)
