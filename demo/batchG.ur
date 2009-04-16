table t : {Id : int, A : string, B : float}
  PRIMARY KEY Id

structure B = BatchFun.Make(struct
                                val tab = t
                                val title = "BatchG"
                                val cols = {A = BatchFun.string "A",
                                            B = BatchFun.float "B"}
                            end)

fun main () = return <xml><body>
  <form><submit value="Begin demo" action={B.main}/></form>
</body></xml>
