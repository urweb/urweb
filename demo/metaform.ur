functor Make (M : sig
                  con fs :: {Unit}
                  val fl : folder fs
                  val names : $(mapU string fs)
              end) = struct

    fun handler values = return <xml><body>
      {@mapUX2 [string] [string] [body]
        (fn [nm :: Name] [rest :: {Unit}] [[nm] ~ rest] name value => <xml>
          <li> {[name]} = {[value]}</li>
        </xml>)
        M.fl M.names values}
    </body></xml>

    fun main () = return <xml><body>
      <form>
        {@foldUR [string] [fn cols :: {Unit} => xml form [] (mapU string cols)]
          (fn [nm :: Name] [rest :: {Unit}] [[nm] ~ rest] name
                           (acc : xml form [] (mapU string rest)) => <xml>
                             <li> {[name]}: <textbox{nm}/></li>
                             {useMore acc}
                           </xml>)
          <xml/>
          M.fl M.names}
        <submit action={handler}/>
      </form>
    </body></xml>

end
