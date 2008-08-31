type t = {A : string, B : {C : string, D : string}}

val page = fn x : t => <html><body>
        {cdata x.A},{cdata x.B.C},{cdata x.B.D}
</body></html>

val main : unit -> page = fn () => <html><body>
        <li><a link={page {A = "A", B = {C = "B", D = "C"}}}>First</a></li>
        <li><a link={page {A = "D", B = {C = "E", D = "F"}}}>Second</a></li>
</body></html>