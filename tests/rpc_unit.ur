val callme = return ((), (), "A", (), ())

val main : transaction page = return <xml><body>
  <button value="CLICK ME"
          onclick={fn _ =>
                      (_, _, s, _, _) <- rpc callme;
                      alert s}/>
</body></xml>
