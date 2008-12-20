fun main () : transaction page = return <xml><body>
  <p>Before</p>
  <p><dyn signal={return <xml>Hi!</xml>}/></p>
  <p>After</p>
</body></xml>
