table peers : { A : client, B : channel ( string ) , C : string}
  PRIMARY KEY A

fun connectWithClient () = "Hi"

fun main () =
	me <- self;
	ch <- channel;
    dml (INSERT INTO peers (A, B, C)
         VALUES ({[me]}, {[ch]}, {["Hi"]}));
    rows <- queryX (SELECT * FROM peers WHERE peers.A > {[me]} OR peers.A < {[me]})
            (fn row => <xml><tr>
            	<td>{[row.Peers.C]}</td>
             	<td>
             		<button value="Connect" onclick={fn _ => rpc (connectWithClient ()); } />
             	</td>
            </tr></xml>);

    return <xml>
      <head>
	    <title>Hello WebRTC!</title>
	  </head>
	  <body>
	    <h1>Hello WebRTC!</h1>
	      <table border=1>
	        <tr> <th>Client Name</th> <th>Connect</th> <th>Message</th> </tr>  
	        {rows}      
	      </table>
      </body>
    </xml>

fun otherFun () =
	return <xml>
	  <head>
	    <title>Hello WebRTC!</title>
	  </head>

	  <body>
	    <h1>Hello WebRTC!</h1>
	    <form>
	    	<submit value="Begin demo" action={otherFun}/>
	    </form>
	  </body>
	</xml>
