style linktitle
style topic

fun main () : transaction page =
    text <- source "Lorem ipsum dolor sit amet";
    cls <- source linktitle;
    return <xml>
      <head>
        <title>Dynamic CSS class test</title>
        <link href="http://adam.chlipala.net/style.css" rel="stylesheet" type="text/css" media="all"/>
      </head>
      <body>
        <dyn signal={t <- signal text;
                     return <xml><div dynClass={signal cls}>{[t]}</div></xml>}/>
        <div dynClass={signal cls}>
          <dyn signal={t <- signal text; return (txt t)}/>
        </div>
        <button value="Style1" onclick={set cls linktitle}/>
        <button value="Style2" onclick={set cls topic}/>
        <ctextbox source={text}/>
      </body>
    </xml>
