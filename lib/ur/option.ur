datatype t = datatype Basis.option

fun isSome [a] x =
    case x of
        None => False
      | Some _ => True

fun mp [a] [b] f x =
    case x of
        None => None
      | Some y => Some (f y)
