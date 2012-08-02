fun main (n : int) (s : string) : transaction page = return <xml><body>
  <button onclick={fn _ => alert ("n = " ^ show n ^ "; s = " ^ s)}/>
</body></xml>
