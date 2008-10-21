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
      
    fun console (ls : list M.t) = return <xml><body>
      Current list: {toXml ls}<br/>

      <form>
        Add element: <textbox{#X}/> <submit action={cons ls}/>
      </form>
    </body></xml>

    and cons (ls : list M.t) (r : {X : string}) =
        case M.fromString r.X of
            None => return <xml><body>Invalid string!</body></xml>
          | Some v => console (Cons (v, ls))

    fun main () = console Nil
end
