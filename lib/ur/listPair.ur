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
