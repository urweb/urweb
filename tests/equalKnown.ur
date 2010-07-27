type fruit = int
table fruit : { Id : fruit, Nam : string, Weight : float, Secret : string }
  PRIMARY KEY Id,
  CONSTRAINT Nam UNIQUE Nam

policy sendClient (SELECT fruit.Id, fruit.Nam
                   FROM fruit)

policy sendClient (SELECT fruit.Weight
                   FROM fruit
                   WHERE known(fruit.Secret))

policy equalKnown[#Secret] fruit

fun main () =
    x1 <- queryX (SELECT fruit.Id, fruit.Nam, fruit.Weight
                  FROM fruit
                  WHERE fruit.Nam = "apple"
                    AND fruit.Secret = "tasty")
                 (fn x => <xml><li>{[x.Fruit.Id]}: {[x.Fruit.Nam]}, {[x.Fruit.Weight]}</li></xml>);

    return <xml><body>
      <ul>{x1}</ul>
    </body></xml>
