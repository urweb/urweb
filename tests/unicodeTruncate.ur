val s = "𝕬" (* U+1D56C *)

val main x : transaction page =
    return
	<xml>
	  <body>
	    <p>𝕬</p>
	    <p>{[s]}</p>
	    <p>{[if x then s else s]}</p>
	  </body>
	</xml>
