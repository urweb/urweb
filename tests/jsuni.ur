fun main () =
    s1 <- source "";
    s2 <- source "";

    let
        fun echo s = return s

        fun echoer () =
            v1 <- get s1;
            v1' <- rpc (echo v1);
            set s2 v1'
    in
        return <xml><body>
          <dyn signal={v <- signal s2; return (cdata v)}/><hr/>
          <ctextbox source={s1}/> <button onclick={echoer ()}/>
        </body></xml>
    end
