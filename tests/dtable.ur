fun main () =
  s <- source <xml><tr><td>A</td><td>A'</td></tr></xml>;
  return <xml><body>
    <button value="Click me!" onclick={set s <xml><tr><td>B</td><td>B'</td></tr><tr><td>C</td><td>C'</td></tr></xml>}/><br/>
    <table><tr><td>Pre</td><td>Pre'</td></tr><dyn signal={signal s}/><tr><td>Post</td><td>Post</td><td>Post'</td></tr></table>
  </body></xml>
