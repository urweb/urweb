con stringify = fold (fn nm :: Name => fn u :: Unit => fn t :: {Type} => [nm = string] ++ t) []

signature S = sig
        con rs :: {Unit}
        val names : $(stringify rs)
end

signature S' = sig
        con rs :: {Unit}

        val handler : $(stringify rs) -> page
        val page : unit -> page
end

functor F (M : S) : S' where con rs = M.rs = struct
        con rs = M.rs

        val handler = fn x : $(stringify M.rs) => <html><body>
                {fold [fn rs :: {Unit} => $(stringify rs) -> $(stringify rs) -> xml body [] []]
                        (fn nm :: Name => fn u :: Unit => fn rest :: {Unit} =>
                                fn f : $(stringify rest) -> $(stringify rest) -> xml body [] [] =>
                                fn names : $(stringify ([nm] ++ rest)) =>
                                fn x : $(stringify ([nm] ++ rest)) =>
                                        <body><li> {cdata names.nm}: {cdata x.nm}</li> {f (names -- nm) (x -- nm)}</body>)
                        (fn names => fn x => <body></body>)
                        [M.rs] M.names x}
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

        val names = {A = "A", B = "B", C = "C"}
end)

open M

