fun main () =
  a <- source "some text";
  return <xml><body>
    <label>A: <ctextbox source={a}/></label>
  </body></xml>
