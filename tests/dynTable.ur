fun main () : transaction page =
    s <- source <xml/>;
    s1 <- source <xml/>;
    n <- source 0;
    return <xml><body>
      <table>
        <dyn signal={signal s}/>
        <tr> <td>Hi</td> </tr>
      </table>

      <button onclick={fn _ => v <- get n;
                          set n (v + 1);
                          set s <xml><tr> <td>Whoa!({[v]})</td> </tr></xml>}/>

      <table>
        <tr> <dyn signal={signal s1}/> </tr>
        <tr> <td>Hi!</td> </tr>
      </table>

      <button onclick={fn _ => set s1 <xml><td>Whoa!</td></xml>}/>
    </body></xml>
