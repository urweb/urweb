datatype message =  NewQuestion of { Id : string}
                  | Something of {Blah : int}

fun showMessage (message : message) =
    case message of
        NewQuestion {Id = a} => a
      | Something {Blah = x} => "blah"

val show_mes = mkShow showMessage

fun showTwoTuple [a] [b] (_ : show a) (_: show b) = mkShow (fn (two_tuple : {1:a, 2:b}) =>
                              "(" ^ show two_tuple.1 ^ ", " ^ show two_tuple.2 ^ ")"

                          )
val om = show  (Something {Blah = 1}, Something {Blah = 2}) 
