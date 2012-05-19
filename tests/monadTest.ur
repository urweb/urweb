val x : transaction int = Monad.liftM2 plus (return 1) (return 2)

val x : transaction xbody = List.mapXiM (fn i x => return <xml><li>{[i]} = {[x]}</li></xml>) (1 :: 2 :: [])
