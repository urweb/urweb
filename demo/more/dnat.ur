datatype t' = O | S of source t'
type t = source t'

val zero = source O

fun inc n =
    v <- get n;
    case v of
        O =>
        n' <- source O;
        set n (S n')
      | S n => inc n

fun dec n =
    let
        fun dec' last n =
            v <- get n;
            case v of
                O => (case last of
                          None => return ()
                        | Some n' => set n' O)
              | S n' => dec' (Some n) n'
    in
        dec' None n
    end

fun render [ctx] [inp] [[Body] ~ ctx] (xml : xml ([Body] ++ ctx) inp []) n =
    let
        fun render n =
            n <- signal n;
            return (render' n)

        and render' n =
            case n of
                O => <xml/>
              | S n => <xml>
                {xml}
                <dyn signal={render n}/>
              </xml>
    in
        <xml><dyn signal={render n}/></xml>
    end
