fun main wrap =
    let
        fun edit id =
            let
                val r = 0
                fun save () = <xml/>
            in
                wrap (save ())
            end
    in
        edit 0
    end

fun wrap (inside : xbody) = return <xml/>

val main () = main wrap
