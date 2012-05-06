fun handler r =
    return <xml><body>
      <span style={oneProperty
                       (oneProperty noStyle (value (property r.Prop) (atom r.Valu)))
                       (value (property "background") (css_url (bless r.Url)))}>
        Teeeest
      </span>
    </body></xml>

fun main () =
    prop <- source "";
    valu <- source "";
    url <- source "";
    xm <- source <xml/>;
    return <xml><body>
      Property: <ctextbox source={prop}/><br/>
      Value: <ctextbox source={valu}/><br/>
      URL: <ctextbox source={url}/><br/>
      <button value="Go!" onclick={prop <- get prop;
                                   valu <- get valu;
                                   url <- get url;
                                   set xm <xml><span style={oneProperty
                                                                (oneProperty noStyle (value (property prop) (atom valu)))
                                                                (value (property "background") (css_url (bless url)))}>
                                     Teeeest
                                   </span></xml>}/>
      <hr/>
      <dyn signal={signal xm}/>
      <hr/>
      <h2>Or the old fashioned way...</h2>

      <form>
        Property: <textbox{#Prop}/><br/>
        Value: <textbox{#Valu}/><br/>
        URL: <textbox{#Url}/><br/>
        <submit action={handler}/>
      </form>
    </body></xml>
