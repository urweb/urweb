datatype log = End | Line of string * source log

fun render log =
    case log of
        End => <xml/>
      | Line (line, logS) => <xml>{[line]}<br/><dyn signal={renderS logS}/></xml>

and renderS logS =
    log <- signal logS;
    return (render log)

sequence s
table t : { Id : int, Title : string, Chan : option (channel string) }

fun chat id =
    r <- oneRow (SELECT t.Title, t.Chan FROM t WHERE t.Id = {[id]});
    ch <- (case r.T.Chan of
               None => (ch <- channel;
                        dml (UPDATE t SET Chan = {[Some ch]} WHERE Id = {[id]});
                        return ch)
             | Some ch => return ch);

    newLine <- source "";
    logHead <- source End;
    logTail <- source logHead;

    let
        fun join () = subscribe ch

        fun onload () =
            let
                fun listener () =
                    s <- recv ch;
                    oldTail <- get logTail;
                    newTail <- source End;
                    set oldTail (Line (s, newTail));
                    set logTail newTail;
                    listener ()
            in
                join ();
                listener ()
            end

        fun speak line =
            send ch line

        fun doSpeak () =
            line <- get newLine;
            speak line
    in
        return <xml><body onload={onload ()}>
          <h1>{[r.T.Title]}</h1>

          <button value="Send:" onclick={doSpeak ()}/> <ctextbox source={newLine}/>

          <h2>Messages</h2>

          <dyn signal={renderS logHead}/>
          
        </body></xml>            
    end

fun list () =
    queryX (SELECT * FROM t)
    (fn r => <xml><tr>
      <td>{[r.T.Id]}</td> <td><a link={chat r.T.Id}>{[r.T.Title]}</a></td>
      <td><a link={delete r.T.Id}>[delete]</a></td>
    </tr></xml>)

and delete id =
    dml (DELETE FROM t WHERE Id = {[id]});
    main ()

and main () : transaction page =
    let
        fun create r =
            id <- nextval s;
            dml (INSERT INTO t (Id, Title, Chan) VALUES ({[id]}, {[r.Title]}, NULL));
            main ()
    in
        ls <- list ();
        return <xml><body>
          <table>
            <tr> <th>ID</th> <th>Title</th> </tr>
            {ls}
          </table>
          
          <h1>New Channel</h1>
          
          <form>
            Title: <textbox{#Title}/><br/>
            <submit action={create}/>
          </form>
        </body></xml>
    end
