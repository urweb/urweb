type fruit = int
table fruit : { Id : fruit, Nam : string, Weight : float, Secret : string }
  PRIMARY KEY Id,
  CONSTRAINT Nam UNIQUE Nam

type order = int
table order : { Id : order, Fruit : fruit, Qty : int, Code : int }
  PRIMARY KEY Id,
  CONSTRAINT Fruit FOREIGN KEY Fruit REFERENCES fruit(Id)

policy query_policy (SELECT fruit.Id, fruit.Nam, fruit.Weight
                     FROM fruit)
policy query_policy (SELECT order.Id, order.Fruit, order.Qty
                     FROM order, fruit
                     WHERE order.Fruit = fruit.Id
                       AND order.Qty = 13)

fun main () =
    x1 <- queryX (SELECT fruit.Id, fruit.Nam
                  FROM fruit)
                 (fn x => <xml><li>{[x.Fruit.Id]}: {[x.Fruit.Nam]}</li></xml>);

    x2 <- queryX (SELECT fruit.Nam, order.Qty
                  FROM fruit, order
                  WHERE fruit.Id = order.Fruit
                    AND order.Qty = 13)
                 (fn x => <xml><li>{[x.Fruit.Nam]}: {[x.Order.Qty]}</li></xml>);

    return <xml><body>
      <ul>{x1}</ul>
      <ul>{x2}</ul>
    </body></xml>
