table channels : {Id : int, Channel:channel xbody}

fun dosend (s:string) : transaction unit =
  c <- oneRow1 (SELECT * FROM channels);
  debug ("Sending " ^ s ^ " through the channel...");
  send c.Channel <xml>{[s]}</xml>

fun mkchannel {} : transaction xbody =
  c <- channel;
  s <- source <xml/>;
  dml( DELETE FROM channels WHERE Id >= 0);
  dml( INSERT INTO channels(Id, Channel) VALUES(0, {[c]}) );
  return <xml>
    <button value="Send" onclick={fn _ => rpc(dosend "blabla")}/>
    <active code={spawn(x <- recv c; alert ("Got something from the channel"); set s x); return <xml/>}/>
    <dyn signal={signal s}/>
  </xml>

fun main {} : transaction page =
  s <- source <xml/>;
  return <xml>
    <head/>
    <body>
      <button value="Register" onclick={fn _ =>
        x <- rpc(mkchannel {}); set s x
      }/>
      <dyn signal={signal s}/>
    </body>
  </xml>
