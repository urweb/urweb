fun main () =
    s1 <- source False;
    s2 <- source False;

    return <xml><body>
      <dyn signal={s1 <- signal s1;
                   return (if s1 then
                               <xml><dyn signal={s2 <- signal s2;
                                                 return <xml>{[s2]}</xml>}/></xml>
                           else
                               <xml>Not yet</xml>)}/>
      <hr/>
      <button value="s1" onclick={set s1 True}/>
      <button value="s2" onclick={set s2 True}/>
   </body></xml>
