fun main () = return <xml><body>
  <a link={other ()}>Go to the other one!</a>
</body></xml>

and other () = return <xml><body>
  <a link={main ()}>Return to <tt>main</tt>!</a>
</body></xml>
