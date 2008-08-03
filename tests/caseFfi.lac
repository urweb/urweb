extern structure M : sig
        datatype t = A | B
        datatype u = C of t | D
end

val f = fn x => case x of M.A => M.B | M.B => M.A

val t2s = fn x => case x of M.A => "A" | M.B => "B"

val g = fn x => case x of M.C a => M.C (f a) | M.D => M.C M.A

val u2s = fn x => case x of M.C a => t2s a | M.D => "D"

val page = fn x => <html><body>
        {cdata (t2s x)}
</body></html>

val page2 = fn x => <html><body>
        {cdata (u2s x)}
</body></html>

val main : unit -> page = fn () => <html><body>
        <li><a link={page M.A}>A</a></li>
        <li><a link={page M.B}>B</a></li>
        <li><a link={page2 (M.C M.A)}>C A</a></li>
        <li><a link={page2 (M.C M.B)}>C B</a></li>
        <li><a link={page2 M.D}>D</a></li>
</body></html>
