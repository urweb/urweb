fun draggableList title items =
    itemSources <- List.mapM source items;
    draggingItem <- source None;
    return <xml>
      <h2>Great {[title]}</h2>
      <ul>
        {List.mapX (fn itemSource => <xml>
          <li onmousedown={set draggingItem (Some itemSource)}
              onmouseup={set draggingItem None}
              onmouseover={di <- get draggingItem;
                           case di of
                               None => return ()
                             | Some di => item1 <- get di;
                               item2 <- get itemSource;
                               set di item2;
                               set itemSource item1}>
              <dyn signal={s <- signal itemSource; return <xml>{[s]}</xml>}/>
         </li></xml>) itemSources}
      </ul>
    </xml>

fun main () =
    bears <- draggableList "Bears" ("Pooh" :: "Paddington" :: "Rupert" :: "Edward" :: []);
    beers <- draggableList "Beers" ("Budvar" :: "Delirium Tremens" :: "Deuchars" :: []);
    boars <- draggableList "Boars" ("Sus scrofa scrofa"
                                        :: "Sus scrofa ussuricus"
                                        :: "Sus scrofa cristatus"
                                        :: "Sus scrofa taiwanus" :: []);
    return <xml><body>
      {bears}
      {beers}
      {boars}
    </body></xml>
