table t1 : {Id : int, A : int, B : string, C : float, D : bool}

open Crud.Make(struct
        val tab = t1

        val title = "Crud1"

        val cols = {
                A = {Show = txt _},
                B = {Show = txt _},
                C = {Show = txt _},
                D = {Show = txt _}
        }
end)
