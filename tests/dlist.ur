datatype dlist = Nil | Cons of string * source dlist

fun delist dl =
    case dl of
        Nil => <xml>[]</xml>
      | Cons (x, s) => <xml>{[x]} :: ({delistSource s})</xml>

and delistSource s = <xml><dyn signal={dl <- signal s; return (delist dl)}/></xml>

fun main () : transaction page =
    ns <- source Nil;
    s <- source ns;
    tb <- source "";
    return <xml><body>
      <dyn signal={s <- signal s; return (delistSource s)}/><br/>
      <br/>
      <ctextbox source={tb}/>
      <button value="Add" onclick={hd <- get tb;
                                   tl <- get s;
                                   s' <- source (Cons (hd, tl));
                                   set s s'}/>
      <button value="Reset" onclick={set s ns}/>
    </body></xml>
