fun main () : transaction page =
    ch <- channel;
    let
        fun onload () =
            subscribe ch;
            send ch "Hello world!"

        fun haveAnother () =
            send ch "Here's another."
    in
        return <xml><body onload={onload ()}>
          <button value="Another?" onclick={haveAnother ()}/>
        </body></xml>
    end
