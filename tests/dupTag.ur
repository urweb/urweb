structure S = struct
  fun one () = 
    let
      fun save r = return <xml/>
    in
      return <xml><body><form><submit action={save}/></form></body></xml>
    end
  fun two () =
    let
      fun save r = return <xml/>
    in
      return <xml><body><form><submit action={save}/></form></body></xml>
    end
end

fun main () : transaction page = return <xml>
    <body>
      <a link={S.one()}>one</a>
      <a link={S.two()}>two</a>  
    </body>
  </xml>
