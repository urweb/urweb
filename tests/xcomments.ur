fun foo () = <xml>Hi!</xml>

(* fun bar () = return (* No *)<xml>Yes!</xml> *)

fun main () = return <xml><body>
  A (* B *) C (* D (* E *) F *) D<br/>
  <b>A</b> <i>(* B *) C <b>D (* E <!-- 7 --> *) F {foo ()}</b></i>
  A <!-- B --> C <!-- D (* E *) F --> D<br/>
  <b>A</b> <i><!-- B --> C <b>D <!-- E --> F {foo ()}</b></i>
</body></xml>
