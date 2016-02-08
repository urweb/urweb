datatype foo = Bar

val x =
    let
        val _ = mkShow (fn Bar => "Bar")
    in
        show Bar
    end
