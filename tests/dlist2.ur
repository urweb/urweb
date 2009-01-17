datatype dlist = Nil | Cons of string * source dlist

fun delist dl =
    case dl of
        Nil => <xml>[]</xml>
      | Cons (x, s) => <xml>{[x]} <ctextbox/> :: {delistSource s}</xml>

and delistSource s = <xml><dyn signal={dl <- signal s; return (delist dl)}/></xml>

fun main () : transaction page =
    tail0 <- source Nil;
    tail <- source tail0;
    tb <- source "";
    return <xml><body>
      {delist (Cons ("ROOT", tail0))}
      <br/>
      <ctextbox source={tb}/>
      <button value="Add" onclick={hd <- get tb;
                                   tl <- source Nil;
                                   old <- get tail;

                                   set old (Cons (hd, tl));
                                   set tail tl}/>
      <button value="Reset" onclick={set tail0 Nil; set tail tail0}/>
    </body></xml>
