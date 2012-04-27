fun main () : transaction page =
    x <- source <xml/>;
    return <xml>
      <head>
        <title>Test</title>
      </head>
      <body onload={set x <xml>boo</xml>}>
        <dyn signal={signal x}/>
      </body>
    </xml>
