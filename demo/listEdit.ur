datatype rlist = Nil | Cons of {Data : source string,
                                NewData : source string,
                                Tail : source rlist}

fun showString ss =
    s <- signal ss;
    return <xml>{[s]}</xml>

fun show rls =
    v <- signal rls;
    show' v

and show' rl =
    case rl of
        Nil => return <xml/>
      | Cons {Data = ss, NewData = ss', Tail = rls} => return <xml>
        <dyn signal={showString ss}/>
        <button value="Change to:" onclick={fn _ => s <- get ss'; set ss s}/>
        <ctextbox source={ss'}/><br/>
        <dyn signal={show rls}/>
      </xml>

fun main () =
    head <- source Nil;
    tailP <- source head;
    data <- source "";

    let
        fun add () =
            data <- get data;
            data <- source data;
            ndata <- source "";
            tail <- get tailP;
            tail' <- source Nil;

            let
                val cons = Cons {Data = data, NewData = ndata, Tail = tail'}
            in
                set tail cons;
                set tailP tail'
            end
    in
        return <xml><body>
          <ctextbox source={data}/> <button value="Add" onclick={fn _ => add ()}/><br/>
          <br/>

          <dyn signal={show head}/>
        </body></xml>
    end
