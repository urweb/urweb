fun decodeURIComponent1 s =
    case decodeURIComponent s of
    | None => "None"
    | Some s => "Some: " ^ s

fun encode s : transaction string = return (encodeURIComponent s)
fun decode s : transaction string = return (decodeURIComponent1 s)

val main : transaction page =
    src <- source "";
    dst <- source "";
    let
	fun c f = fn _ => v <- get src; v <- rpc (f v); set dst ("C: " ^ v)
	fun js f = fn _ => v <- get src; set dst ("JS: " ^ f v)
    in
	return
	    <xml>
	      <body>
		<p><ctextbox source={src}/></p>
		<p><dyn signal={v <- signal dst; return <xml>{[v]}</xml>}/></p>
		<p>
		  <button value="Encode (C)" onclick={c encode}/>
		  <button value="Encode (JS)" onclick={js encodeURIComponent}/>
		  <button value="Decode (C)" onclick={c decode}/>
		  <button value="Decode (JS)" onclick={js decodeURIComponent1}/>
		</p>
              </body>
            </xml>
    end
