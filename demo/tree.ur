table t : { Id : int, Parent : option int, Nam : string }

open TreeFun.Make(struct
                      val tab = t
                  end)

fun row r = <xml>
  #{[r.Id]}: {[r.Nam]}
</xml>

fun main () =
    xml <- tree row None;
    return <xml><body>
      {xml}
    </body></xml>
