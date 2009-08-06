fun main () =
  s <- source <xml><td>A</td><td>A'</td></xml>;
  return <xml><body>
    <button value="Click me!" onclick={set s <xml><td>B</td><td>B'</td></xml>}/><br/>
    <table><tr><td>Pre</td><td>Pre'</td></tr>
      <tr><td>Hehe</td><dyn signal={signal s}/><td>Hoho</td></tr>
      <tr><td>Post</td><td>Post</td><td>Post'</td></tr></table>
  </body></xml>
