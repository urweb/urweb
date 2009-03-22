fun main () : transaction page =
    ch <- channel;
    let
        fun make () =
            subscribe ch;
            send ch "Hello world!"

        fun echo () =
            msg <- recv ch;
            alert(msg);
            echo ()

        fun onload () =
            make ();
            echo ()

        fun haveAnother () =
            send ch "Here's another."
    in
        return <xml><body onload={onload ()}>
          <button value="Another?" onclick={haveAnother ()}/>
        </body></xml>
    end
