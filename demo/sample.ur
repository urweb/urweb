structure AB = Mymaths.Make(struct
                               type data = string
                               val callback = "Harshita!"
                           end)


fun main()=
let
    fun callMe () =
        x <- AB.simple "test";
        debug x

in
    return <xml>
        <body>
            <h1> Sample </h1>
            <button value="Click Me" onclick={fn _ => callMe()}></button>
        </body>
        </xml>
end

