fun add x =
    let
        fun add' y = x + y
    in
        add' 1 + add' 2
    end

fun f (x : int) =
    let
        fun page () = return <xml><body>
          <a link={page ()}>{[x]}</a>
        </body></xml>
    in
        page
    end

fun f (x : int) =
    let
        fun page1 () = return <xml><body>
          <a link={page2 ()}>{[x]}</a>
        </body></xml>

        and page2 () =
            case Some True of
                Some r => return <xml><body><a link={page1 ()}>{[r]}</a></body></xml>
              | _ => return <xml>Error</xml>
    in
        page2
    end

fun f (x : int) =
    let
        fun page1 () = return <xml><body>
          <a link={page2 ()}>{[x]}</a>
        </body></xml>

        and page2 () =
            case Some True of
                Some r => return <xml><body><a link={page1 ()}>{[r]}</a></body></xml>
              | _ => return <xml><body><a link={page3 ()}>!!</a></body></xml>

        and page3 () = return <xml><body><a link={page2 ()}>!</a><a link={page1 ()}>!</a>
          <a link={page3 ()}>!</a></body></xml>
    in
        page3
    end

fun add2 (x : int) (y : int) =
    let
        fun add3 () = x + y
    in
        add3
    end

fun add3 (x : int) =
    let
        fun add2 (y : int) =
            let
                fun add1 (z : int) = x + y + z
            in
                add1
            end
    in
        add2
    end

(*datatype list t = Nil | Cons of t * list t

fun length (t ::: Type) (ls : list t) =
    let
        fun length' ls acc =
            case ls of
                Nil => acc
              | Cons (_, ls') => length' ls' (acc + 1)
    in
        length' ls 0
    end

*)
