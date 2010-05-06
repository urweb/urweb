type fruit = int
table fruit : { Id : fruit, Nam : string, Weight : float, Secret : string }
  PRIMARY KEY Id,
  CONSTRAINT Nam UNIQUE Nam

(* Everyone may knows IDs and names. *)
policy sendClient (SELECT fruit.Id, fruit.Nam
                   FROM fruit)

(* The weight is sensitive information; you must know the secret. *)
policy sendClient (SELECT fruit.Weight, fruit.Secret
                   FROM fruit
                   WHERE known(fruit.Secret))

fun main () =
    x1 <- queryX (SELECT fruit.Id, fruit.Nam
                  FROM fruit
                  WHERE fruit.Nam = "apple")
                 (fn x => <xml><li>{[x.Fruit.Id]}: {[x.Fruit.Nam]}</li></xml>);
    return <xml><body>
      <ul>{x1}</ul>
    </body></xml>
