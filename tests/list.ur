fun isNil (t ::: Type) (ls : list t) =
    case ls of
        [] => True
      | _ => False

fun delist (ls : list string) : xbody =
        case ls of
            [] => <xml>Nil</xml>
          | h :: t => <xml>{[h]} :: {delist t}</xml>

fun callback ls = return <xml><body>
  {delist ls}
</body></xml>

fun main () = return <xml><body>
  {[isNil ([] : list bool)]},
  {[isNil (1 :: [])]},
  {[isNil ("A" :: "B" :: [])]}

  <p>{delist ("X" :: "Y" :: "Z" :: [])}</p>
  <a link={callback ("A" :: "B" :: [])}>Go!</a>
</body></xml>
