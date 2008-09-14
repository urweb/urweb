table t1 : {Id : int, A : int, B : string, C : float, D : bool}

open Crud.Make(struct
        val tab = t1

        val title = "Crud1"

        val cols = {
                A = {Nam = "A", Show = txt _},
                B = {Nam = "B", Show = txt _},
                C = {Nam = "C", Show = txt _},
                D = {Nam = "D", Show = txt _}
        }
end)
