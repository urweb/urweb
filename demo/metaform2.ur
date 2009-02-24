structure MM = Metaform.Make(struct
                                 val names = {X = "x", Y = "y"}
                             end)

fun diversion () = return <xml><body>
  Welcome to the diversion.
</body></xml>

fun main () = return <xml><body>
  <li> <a link={diversion ()}>See something shiny!</a></li>
  <li> <a link={MM.main ()}>Fill out a form!</a></li>
</body></xml>
