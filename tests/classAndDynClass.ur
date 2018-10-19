style style1
style style2

fun main () : transaction page = return <xml><body>
  <div class="style1" dynClass={return (CLASS "style2")}>Text</div>
  <div dynClass={return (CLASS "style2")}>Text</div>
  <div style="font-weight: bold" dynStyle={return (STYLE "font-variant: small-caps")}>Text</div>
  <div dynStyle={return (STYLE "font-variant: small-caps")}>Text</div>
</body></xml>
