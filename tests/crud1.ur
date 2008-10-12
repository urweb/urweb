table t1 : {Id : int, A : int, B : string, C : float, D : bool}

val a = {Nam = "A",
         Show = txt _,
         Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
         WidgetPopulated = fn (nm :: Name) n =>
                              <lform><textbox{nm} value={show _ n}/></lform>,
         Parse = readError _,
         Inject = _}

val b = {Nam = "B",
         Show = txt _,
         Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
         WidgetPopulated = fn (nm :: Name) s =>
                              <lform><textbox{nm} value={s}/></lform>,
         Parse = readError _,
         Inject = _}

val c = {Nam = "C",
         Show = txt _,
         Widget = fn nm :: Name => <lform><textbox{nm}/></lform>,
         WidgetPopulated = fn (nm :: Name) n =>
                              <lform><textbox{nm} value={show _ n}/></lform>,
         Parse = readError _,
         Inject = _}

val d = {Nam = "D",
         Show = txt _,
         Widget = fn nm :: Name => <lform><checkbox{nm}/></lform>,
         WidgetPopulated = fn (nm :: Name) b =>
                              <lform><checkbox{nm} checked={b}/></lform>,
         Parse = fn x => x,
         Inject = _}

open Crud.Make(struct
                   val tab = t1
                             
                   val title = "Crud1"
                               
                   val cols = {A = a,
                               B = b,
                               C = c,
                               D = d}
               end)
