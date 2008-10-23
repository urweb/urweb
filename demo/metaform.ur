functor Make (M : sig
                  con fs :: {Unit}
                  val names : $(mapUT string fs)
              end) = struct

    fun handler values = return <xml><body>
      {foldURX2 [string] [string] [body]
       (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] name value => <xml>
         <li> {[name]} = {[value]}</li>
       </xml>)
       [M.fs] M.names values}
    </body></xml>

    fun main () = return <xml><body>
      <form>
        {foldUR [string] [fn cols :: {Unit} => xml form [] (mapUT string cols)]
                (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] name
                                 (acc : xml form [] (mapUT string rest)) => <xml>
                                   <li> {[name]}: <textbox{nm}/></li>
                                   {useMore acc}
                                 </xml>)
                <xml/>
                [M.fs] M.names}
        <submit action={handler}/>
      </form>
    </body></xml>

end
