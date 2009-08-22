structure Room = Broadcast.Make(struct
                                    type t = string
                                end)

sequence s
table t : { Id : int, Title : string, Room : Room.topic }
  PRIMARY KEY Id

fun chat id () =
    r <- oneRow (SELECT t.Title, t.Room FROM t WHERE t.Id = {[id]});
    ch <- Room.subscribe r.T.Room;

    newLine <- source "";
    buf <- Buffer.create;

    let
        fun onload () =
            let
                fun listener () =
                    s <- recv ch;
                    Buffer.write buf s;
                    listener ()
            in
                listener ()
            end

        fun getRoom () =
            r <- oneRow (SELECT t.Room FROM t WHERE t.Id = {[id]});
            return r.T.Room

        fun speak line =
            room <- getRoom ();
            Room.send room line

        fun doSpeak () =
            line <- get newLine;
            set newLine "";
            rpc (speak line)
    in
        return <xml><body onload={onload ()}>
          <h1>{[r.T.Title]}</h1>

          <button value="Send:" onclick={doSpeak ()}/> <ctextbox source={newLine}/>

          <h2>Messages</h2>

          <dyn signal={Buffer.render buf}/>
          
        </body></xml>            
    end

fun list () =
    queryX' (SELECT * FROM t)
    (fn r =>
        count <- Room.subscribers r.T.Room;
        return <xml><tr>
          <td>{[r.T.Id]}</td>
          <td>{[r.T.Title]}</td>
          <td>{[count]}</td>
          <td><form><submit action={chat r.T.Id} value="Enter"/></form></td>
          <td><form><submit action={delete r.T.Id} value="Delete"/></form></td>
        </tr></xml>)

and delete id () =
    dml (DELETE FROM t WHERE Id = {[id]});
    main ()

and main () =
    let
        fun create r =
            id <- nextval s;
            room <- Room.create;
            dml (INSERT INTO t (Id, Title, Room) VALUES ({[id]}, {[r.Title]}, {[room]}));
            main ()
    in
        ls <- list ();
        return <xml><body>
          <h1>Current Channels</h1>

          <table>
            <tr> <th>ID</th> <th>Title</th> <th>#Subscribers</th> </tr>
            {ls}
          </table>
          
          <h1>New Channel</h1>
          
          <form>
            Title: <textbox{#Title}/><br/>
            <submit action={create}/>
          </form>
        </body></xml>
    end
