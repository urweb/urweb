fun main () : transaction page =
    ch <- channel;
    let
        fun onload () =
            subscribe ch;
            send ch "Hello world!"
    in
        return <xml><body onload={onload ()}>

        </body></xml>
    end
