fun main () =
    buf <- Buffer.create;
    let
        fun loop1 () =
            Buffer.write buf "A";
            sleep 9;
            loop1 ()

        fun loop2 () =
            Buffer.write buf "B";
            sleep 9;
            error <xml>Darn</xml>
            loop2 ()
    in
        return <xml><body onload={spawn (loop1 ()); loop2 ()}>
          <dyn signal={Buffer.render buf}/>
        </body></xml>
    end
