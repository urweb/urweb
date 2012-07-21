fun main () : transaction page =
    id1 <- fresh;
    id2 <- fresh;
    s1 <- source "";
    s2 <- source "";
    which <- source False;

    return <xml><body>
      <ctextbox id={id1} source={s1}/>
      <ctextbox id={id2} source={s2}/>
      <button onclick={fn _ => w <- get which;
                          set which (not w);
                          giveFocus (if w then id1 else id2)}/>
    </body></xml>
