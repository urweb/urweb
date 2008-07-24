con stringify = fold (fn nm :: Name => fn u :: Unit => fn t :: {Type} => [nm = string] ++ t) []

signature S = sig
        con rs :: {Unit}
end

signature S' = sig
        con rs :: {Unit}

        val handler : $(stringify rs) -> page
        val page : unit -> page
end

functor F (M : S) : S' where con rs = M.rs = struct
        con rs = M.rs

        val handler = fn x : $(stringify M.rs) => <html><body>
                {fold [fn rs :: {Unit} => $(stringify rs) -> xml body [] []]
                        (fn nm :: Name => fn u :: Unit => fn rest :: {Unit} =>
                                fn f : $(stringify rest) -> xml body [] [] =>
                                fn x : $(stringify ([nm] ++ rest)) =>
                                        <body><li> {cdata x.nm}</li> {f (x -- nm)}</body>)
                        (fn x => <body></body>)
                        [M.rs] x}
        </body></html>

        val page = fn () => <html><body>
                <lform>
                        {fold [fn rs :: {Unit} => xml lform [] (stringify rs)]
                                (fn nm :: Name => fn u :: Unit => fn rest :: {Unit} =>
                                        fn frag : xml lform [] (stringify rest) =>
                                                <lform><li> <textbox{nm}/></li> {useMore frag}</lform>)
                                <lform></lform>
                                [rs]}

                        <submit action={handler}/>
                </lform>
        </body></html>
end

structure M = F(struct
        con rs = [A, B, C]
end)

open M

