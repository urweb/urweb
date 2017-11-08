fun debugMe a =
    debug (show a)

fun callMe () =
    Mymaths.simple 30 80 debugMe

fun main()=
return <xml>
    <body>
        <h1> Sample </h1>
        <button value="Click Me" onclick={fn _ => callMe()}></button>
    </body>
    </xml>