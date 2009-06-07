datatype t = datatype Basis.option

fun isSome [a] x =
    case x of
        None => False
      | Some _ => True
