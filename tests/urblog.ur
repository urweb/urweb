table entry : { Id : int, Title : string, Created : time, Author : string,
Body : string }
  PRIMARY KEY Id

fun list () =
    rows <- queryX (SELECT * FROM entry)
            (fn row => 
				<xml>
					<div>
						<h1>{[row.Entry.Title]}</h1><br />
							<h2>By {[row.Entry.Author]} at {[row.Entry.Created]}</h2>
						<p>{[row.Entry.Body]}</p>
					</div>
				</xml>
            );
    return 
	 	<xml>
		  <head>
				<title>All Entries</title>
		  </head>
		  <body>
			<h1>All Entries</h1>
        {rows}
		  </body>
    	</xml>

fun main () = return <xml>
  <head>
    <title>UrBlog</title>
  </head>

  <body>
    <h1>UrBlog</h1>
  </body>
</xml>
