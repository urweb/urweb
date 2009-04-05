fun main () =
    buf <- Buffer.create;
    let
        fun loop prefix delay =
            let
                fun loop' n =
                    Buffer.write buf (prefix ^ ": Message #" ^ show n);
                    sleep delay;
                    loop' (n + 1)
            in
                loop'
            end
    in
        return <xml><body onload={spawn (loop "A" 5000 0); spawn (loop "B" 3000 100)}>
          <dyn signal={Buffer.render buf}/>
        </body></xml>
    end
