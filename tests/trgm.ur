table turtles : { Nam : string }

fun add name =
    dml (INSERT INTO turtles(Nam)
         VALUES ({[name]}))

fun closest name =
    List.mapQuery (SELECT *
                   FROM turtles
                   ORDER BY similarity(turtles.Nam, {[name]}) DESC
                   LIMIT 5)
                  (fn r => r.Turtles.Nam)

val main =
    name <- source "";
    results <- source [];
    return <xml><body>
      Name: <ctextbox source={name}/><br/>
      <button value="Add" onclick={fn _ => n <- get name; rpc (add n)}/><br/>
      <button value="Search" onclick={fn _ => n <- get name; ls <- rpc (closest n); set results ls}/><br/>
      <dyn signal={rs <- signal results;
                   return <xml><ol>
                     {List.mapX (fn n => <xml><li>{[n]}</li></xml>) rs}
                   </ol></xml>}/>
    </body></xml>
