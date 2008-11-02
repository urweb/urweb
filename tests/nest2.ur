fun wooho (wrap : xbody -> transaction page) =
    let
        fun subPage n =
            let
                fun subberPage () = wrap <xml>{[n]}</xml>
            in
                wrap <xml><a link={subberPage ()}>Go</a></xml>
            end
    in
        subPage 0
    end

fun wrap x = return <xml><body>{x}</body></xml>

fun main () = wooho wrap
