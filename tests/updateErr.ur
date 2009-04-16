fun main () : transaction page =
    s <- source "";
    b <- Buffer.create;
    txt <- source "";

    return <xml><body onload={onError (fn xml => Buffer.write b (show xml));
                              onFail (fn s => alert ("FAIL! " ^ s))}>
      <dyn signal={s <- signal s; return <xml>{[s]}</xml>}/><br/>
      <dyn signal={s <- signal s; if s = "" then return <xml>Init</xml> else error <xml>Crapky</xml>}/><br/>
      <dyn signal={s <- signal s; return <xml>"{[s]}"</xml>}/><br/>

      <ctextbox source={txt}/> <button onclick={s' <- get txt; set s s'; set txt ""}/>

      <hr/>

      <dyn signal={Buffer.render b}/>
    </body></xml>
