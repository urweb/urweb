fun isNone [a] (x : option a) =
    case x of
        None => True
      | Some _ => False
