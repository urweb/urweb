con t ctx = source bool * xml ctx [] []

fun create [ctx] (x : xml ctx [] []) =
    s <- source False;
    return (s, x)

fun expand [ctx] (t : t ctx) =
    set t.1 True

fun collapse [ctx] (t : t ctx) =
    set t.1 False

fun render [ctx] [[Body] ~ ctx] (t : t ([Body] ++ ctx)) =
    <xml><dyn signal={b <- signal t.1;
                      return (if b then
                                  <xml>
                                    <button value="-" onclick={collapse t}/><br/>
                                    {t.2}
                                  </xml>
                              else
                                  <xml>
                                    <button value="+" onclick={expand t}/><br/>
                                  </xml>)}/></xml>
