structure I = struct
    type t = int
    val toString = show
    val fromString = read
end

structure S = struct
    type t = string
    val toString = show
    val fromString = read
end

structure IL = ListFun.Make(I)
structure SL = ListFun.Make(S)

fun main () = return <xml><body>
  Pick your poison:<br/>
  <li> <a link={IL.main ()}>Integers</a></li>
  <li> <a link={SL.main ()}>Strings</a></li>
</body></xml>
