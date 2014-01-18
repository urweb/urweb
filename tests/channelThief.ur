table t : { Ch : channel string }

fun go () =
    let
        fun overwrite () =
            dml (DELETE FROM t WHERE TRUE);
            ch <- channel;
            dml (INSERT INTO t (Ch) VALUES ({[ch]}));
            return ch

        fun retrieve () =
            oneRowE1 (SELECT (t.Ch) FROM t)

        fun transmit () =
            ch <- retrieve ();
            send ch "Test"

        fun listenOn ch =
            s <- recv ch;
            alert s
    in
        ch <- overwrite ();
        return <xml><body onload={listenOn ch}>
          <button value="overwrite" onclick={fn _ => ch <- rpc (overwrite ()); listenOn ch}/>
          <button value="retrieve" onclick={fn _ => ch <- rpc (retrieve ()); listenOn ch}/>
          <button value="transmit" onclick={fn _ => rpc (transmit ())}/>
        </body></xml>
    end

fun main () = return <xml><body>
  <form><submit action={go}/></form>
</body></xml>
