fun main () : transaction page =
    id1 <- fresh;
    id2 <- fresh;
    id3 <- fresh;
    idS <- source id3;
    return <xml><body>
      <span id={id1}>Hi</span> <span id={id2}>there!</span><br/><br/>
      <dyn signal={idS <- signal idS; return <xml><span id={idS}>Whoa-hoa!</span></xml>}/>
      <button onclick={id <- fresh; set idS id}/>
      Source: <dyn signal={idS <- signal idS; return (txt (<xml><span id={idS}>Whoa-hoa!</span></xml> : xbody))}/>
    </body></xml>
