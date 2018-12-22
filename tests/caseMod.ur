structure M = struct
        datatype t = A | B
end

val f = fn x : M.t => case x of M.A => M.B | M.B => M.A

datatype t = datatype M.t

val g = fn x : t => case x of M.A => B | B => M.A

structure N = struct
        datatype u = C of t | D
end

val h = fn x : N.u => case x of N.C x => x | N.D => M.A

datatype u = datatype N.u

val i = fn x : u => case x of N.C x => x | D => M.A

val toString = fn x =>
        case x of
            C A => "C A"
          | C B => "C B"
          | D => "D"

val rec page = fn x => return <xml><body>
        {cdata (toString x)}<br/>
        <br/>

        <a link={page x}>Again!</a>
</body></xml>

val main : unit -> transaction page = fn () => return <xml><body>
        <li> <a link={page (C A)}>C A</a></li>
        <li> <a link={page (C B)}>C B</a></li>
        <li> <a link={page D}>D</a></li>
</body></xml>
