fun main () =
visible <- source True;
return
    <xml>
      <body>
	<section>
	  <h1>Static table</h1>
	  <table border=1>
	    <thead>
	      <tr>
		<th>Column 0</th>
		<th>Column 1</th>
	      </tr>
	    </thead>
	    <tbody>
	      <tr>
		<td>A</td>
		<td>B</td>
	      </tr>
	    </tbody>
	  </table>
	</section>

	<section>
	  <h1>Dynamic table</h1>
	  <table border=1>
	    <thead>
	      <tr>
		<th>Column 0</th>
		<th>Column 1</th>
	      </tr>
	    </thead>
	    <tbody>
	      <dyn signal={
		 visible <- signal visible;
		 return (if visible then
			     <xml>
			       <tr>
			       <td>A</td>
			       <td>B</td>
			       </tr>			       
			     </xml>
			 else
			     <xml></xml>)
	      }/>
	    </tbody>
	  </table>
	</section>
      </body>
    </xml>
