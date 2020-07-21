datatype sum a b = Left of a | Right of b

val l : sum int string = Left 5
val r : sum int string = Right "Hi"

val show = fn x : sum int string => case x of Left _ => "Left _" | Right s => s

val page = fn x => <xml><html><body>
        {cdata (show x)}
</body></html></xml>

val main : unit -> page = fn () => <xml><html><body>
        <li><a link={page l}>Left</a></li>
        <li><a link={page r}>Right</a></li>
</body></html></xml>
