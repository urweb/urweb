fun main () : transaction page = return <xml><body>
  <p>Before</p>
  <p><dyn signal={s <- return "Bye"; return <xml>{[s]}</xml>}/></p>
  <p>After</p>
</body></xml>
