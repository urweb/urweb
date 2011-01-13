type t = Basis.string

val str = Basis.str1

val length = Basis.strlen
val lengthGe = Basis.strlenGe
val append = Basis.strcat

val sub = Basis.strsub
val suffix = Basis.strsuffix

val index = Basis.strindex
fun sindex r = Basis.strsindex r.Haystack r.Needle
val atFirst = Basis.strchr

fun mindex {Haystack = s, Needle = chs} =
    let
        val n = Basis.strcspn s chs
    in
        if n >= length s then
            None
        else
            Some n
    end

fun substring s {Start = start, Len = len} = Basis.substring s start len

fun seek s ch =
    case index s ch of
        None => None
      | Some i => Some (suffix s (i + 1))
fun mseek {Haystack = s, Needle = chs} =
    case mindex {Haystack = s, Needle = chs} of
        None => None
      | Some i => Some (sub s i, suffix s (i + 1))

fun split s ch =
    case index s ch of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        suffix s (i + 1))
fun split' s ch =
    case index s ch of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        suffix s i)
fun msplit {Haystack = s, Needle = chs} =
    case mindex {Haystack = s, Needle = chs} of
        None => None
      | Some i => Some (substring s {Start = 0, Len = i},
                        sub s i,
                        suffix s (i + 1))

fun ssplit r =
    case sindex r of
        None => None
      | Some i => Some (substring r.Haystack {Start = 0, Len = i},
                        suffix r.Haystack (i + length r.Needle))

fun all f s =
    let
        val len = length s

        fun al i =
            i >= len
            || (f (sub s i) && al (i + 1))
    in
        al 0
    end

fun mp f s =
    let
        fun mp' i acc =
            if i < 0 then
                acc
            else
                mp' (i - 1) (str (f (sub s i)) ^ acc)
    in
        mp' (length s - 1) ""
    end

fun newlines [ctx] [[Body] ~ ctx] (s : string) : xml ([Body] ++ ctx) [] [] =
    case split s #"\n" of
        None => cdata s
      | Some (s1, s2) => <xml>{[s1]}<br/>{newlines s2}</xml>

fun isPrefix {Full = f, Prefix = p} =
    length f >= length p && substring f {Start = 0, Len = length p} = p
