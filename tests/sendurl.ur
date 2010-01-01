fun main () =
    let
        fun getIt () = return (bless "http://www.yahoo.com/")
    in
        s <- source <xml/>;
        return <xml><body>
          <dyn signal={signal s}/>

          <button onclick={v <- rpc (getIt ()); set s <xml><a href={v}>Go!</a></xml>}/>
        </body></xml>
    end
