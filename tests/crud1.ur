table t1 : {Id : int, A : int, B : string, C : float, D : bool}

open Crud.Make(struct
        val tab = t1

        val title = "Crud1"

        val cols = {
                A = {
                        Nam = "A",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        WidgetPopulated = fn (nm :: Name) n => <lform><textbox{nm} value={show _ n}/></lform>,
                        Parse = readError _,
                        Inject = _
                    },
                B = {
                        Nam = "B",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        WidgetPopulated = fn (nm :: Name) s => <lform><textbox{nm} value={s}/></lform>,
                        Parse = readError _,
                        Inject = _
                    },
                C = {
                        Nam = "C",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
                        WidgetPopulated = fn (nm :: Name) n => <lform><textbox{nm} value={show _ n}/></lform>,
                        Parse = readError _,
                        Inject = _
                    },
                D = {
                        Nam = "D",
                        Show = txt _,
                        Widget = fn nm :: Name => <lform><checkbox{nm}/></lform>,
                        WidgetPopulated = fn (nm :: Name) b => <lform><checkbox{nm} checked={b}/></lform>,
                        Parse = fn x => x,
                        Inject = _
                    }
        }
end)
