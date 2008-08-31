val pA : int -> int -> page = fn size1 => fn size2 => <html><body>
        <font size={size1}>Hello</font> <font size={size2}>World!</font>
</body></html>

val main : unit -> page = fn () => <html><body>
        <li> <a link={pA 5 10}>Size 5</a></li>
        <li> <a link={pA 10 5}>Size 10</a></li>
</body></html>
