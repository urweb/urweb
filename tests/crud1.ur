table t1 : {Id : int, A : int, B : string, C : float, D : bool}

open Crud.Make(struct
        con cols :: {(Type * Type)} = [
                A = (int, string),
                B = (string, string),
                C = (float, string),
                D = (bool, string)
        ]

        val tab = t1

        val title = "Crud1"

        val cols = {
                A = {
                        Nam = "A",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        Parse = readError _,
                        Inject = sql_int
                    },
                B = {
                        Nam = "B",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        Parse = readError _,
                        Inject = sql_string
                    },
                C = {
                        Nam = "C",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        Parse = readError _,
                        Inject = sql_float
                    },
                D = {
                        Nam = "D",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        Parse = readError _,
                        Inject = sql_bool
                    }
        }
end)
