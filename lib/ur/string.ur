type t = Basis.string

val length = Basis.strlen
val append = Basis.strcat

val sub = Basis.strsub
val suffix = Basis.strsuffix

val index = Basis.strindex
val atFirst = Basis.strchr

fun substring s {Start = start, Len = len} = Basis.substring s start len

fun split s ch =
    case index s ch of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        substring s {Start = i + 1, Len = length s - i - 1})
