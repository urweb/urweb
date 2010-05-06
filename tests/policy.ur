type fruit = int
table fruit : { Id : fruit, Nam : string, Weight : float, Secret : string }
  PRIMARY KEY Id,
  CONSTRAINT Nam UNIQUE Nam

type order = int
table order : { Id : order, Fruit : fruit, Qty : int, Code : int }
  PRIMARY KEY Id,
  CONSTRAINT Fruit FOREIGN KEY Fruit REFERENCES fruit(Id)

(* Everyone may knows IDs and names. *)
policy sendClient (SELECT fruit.Id, fruit.Nam
                   FROM fruit)

(* The weight is sensitive information; you must know the secret. *)
policy sendClient (SELECT fruit.Weight, fruit.Secret
                   FROM fruit
                   WHERE known(fruit.Secret))

policy sendClient (SELECT order.Id, order.Fruit, order.Qty
                   FROM order, fruit
                   WHERE order.Fruit = fruit.Id
                     AND order.Qty = 13)

fun fname r =
    x <- queryX (SELECT fruit.Weight
                 FROM fruit
                 WHERE fruit.Nam = {[r.Nam]}
                   AND fruit.Secret = {[r.Secret]}
                   AND fruit.Weight <> 3.14
                   AND fruit.Weight < 100.0
                   AND fruit.Weight <= 200.1
                   AND fruit.Weight > 1.23
                   AND fruit.Weight >= 1.24)
         (fn r => <xml>Weight is {[r.Fruit.Weight]}</xml>);

    return <xml><body>
      {x}
    </body></xml>

fun main () =
    x1 <- queryX (SELECT fruit.Id, fruit.Nam
                  FROM fruit
                  WHERE fruit.Nam = "apple")
                 (fn x => <xml><li>{[x.Fruit.Id]}: {[x.Fruit.Nam]}</li></xml>);

    x2 <- queryX (SELECT fruit.Nam, order.Qty
                  FROM fruit, order
                  WHERE fruit.Id = order.Fruit
                    AND order.Qty = 13)
                 (fn x => <xml><li>{[x.Fruit.Nam]}: {[x.Order.Qty]}</li></xml>);

    ro <- oneOrNoRows (SELECT fruit.Id, fruit.Nam
                       FROM fruit);

    return <xml><body>
      <ul>{x1}</ul>

      <ul>{x2}</ul>

      {case ro of
           None => <xml>None</xml>
         | Some _ => <xml>Some</xml>}

      <form>
        Fruit name: <textbox{#Nam}/><br/>
        Secret: <textbox{#Secret}/><br/>
        <submit action={fname}/>
      </form>
    </body></xml>
