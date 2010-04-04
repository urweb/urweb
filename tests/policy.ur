table fruit : { Id : int, Nam : string, Weight : float, Secret : string }

policy query_policy (SELECT fruit.Id, fruit.Nam, fruit.Weight FROM fruit)

fun main () =
    xml <- queryX (SELECT fruit.Nam
                   FROM fruit
                   ORDER BY fruit.Nam)
           (fn x => <xml><li>{[x.Fruit.Nam]}</li></xml>);

    return <xml><body>
      {xml}
    </body></xml>
