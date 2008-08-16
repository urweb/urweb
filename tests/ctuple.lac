val page = fn p :: (Type * Type) => fn f : p.1 -> string => fn x : p.1 => <html><body>
        {cdata (f x)}
</body></html>

val page_string = page [(string, int)] (fn x => x)

val main : unit -> page = fn () => <html><body>
        <a link={page_string "Hi"}>Hi</a>
</body></html>
