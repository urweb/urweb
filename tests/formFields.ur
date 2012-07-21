fun main (pb : postBody) : transaction page = return <xml><body>
  {List.mapX (fn (n, v) => <xml><li>{[n]} = {[v]}</li></xml>) (postFields pb)}
</body></xml>
