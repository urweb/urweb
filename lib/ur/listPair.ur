fun foldlAbort [a] [b] [c] f =
    let
        fun foldlAbort' acc ls1 ls2 =
            case (ls1, ls2) of
                ([], []) => Some acc
              | (x1 :: ls1, x2 :: ls2) =>
                (case f x1 x2 acc of
                     None => None
                   | Some acc' => foldlAbort' acc' ls1 ls2)
              | _ => None
    in
        foldlAbort'
    end

fun mapX [a] [b] [ctx ::: {Unit}] f =
    let
        fun mapX' ls1 ls2 =
            case (ls1, ls2) of
                ([], []) => <xml/>
              | (x1 :: ls1, x2 :: ls2) => <xml>{f x1 x2}{mapX' ls1 ls2}</xml>
              | _ => error <xml>ListPair.mapX: Unequal list lengths</xml>
    in
        mapX'
    end

fun all [a] [b] f =
    let
        fun all' ls1 ls2 =
            case (ls1, ls2) of
                ([], []) => True
              | (x1 :: ls1, x2 :: ls2) => f x1 x2 && all' ls1 ls2
              | _ => False
    in
        all'
    end
