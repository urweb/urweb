style date
style topic

fun main () : transaction page =
    toggle <- source False;
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="http://adam.chlipala.net/style.css"/>
      </head>
      <body>
	<button dynClass={b <- signal toggle;
                          return (if b then date else topic)}
                dynStyle={b <- signal toggle;
                          return (if b then
                                      STYLE "width: 500px"
                                  else
                                      STYLE "width: 200px")}
                onclick={b <- get toggle; set toggle (not b)}/>

        <button dynStyle={b <- signal toggle;
                          return (if b then
                                      STYLE "width: 200px"
                                  else
                                      STYLE "width: 100px")}/>
        <button dynClass={b <- signal toggle;
                          return (if b then
                                      topic
                                  else
                                      date)}/>
      </body>
    </xml>
