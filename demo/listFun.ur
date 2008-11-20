open List

functor Make(M : sig
                 type t
                 val toString : t -> string
                 val fromString : string -> option t
             end) = struct
    fun toXml (ls : list M.t) =
        case ls of
            Nil => <xml>[]</xml>
          | Cons (x, ls') => <xml>{[M.toString x]} :: {toXml ls'}</xml>
      
    fun console (ls : list M.t) =
        let
            fun cons (r : {X : string}) =
                case M.fromString r.X of
                    None => return <xml><body>Invalid string!</body></xml>
                  | Some v => console (Cons (v, ls))
        in
            return <xml><body>
              Current list: {toXml ls}<br/>
              Reversed list: {toXml (rev ls)}<br/>
              Length: {[length ls]}<br/>
              <br/>

              <form>
                Add element: <textbox{#X}/> <submit action={cons}/>
              </form>
            </body></xml>
        end

    fun main () = console Nil
end
