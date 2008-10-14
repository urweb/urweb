table t1 : {Id : int, A : int, B : string, C : float, D : bool}

val a = {Nam = "A",
         Show = txt _,
         Widget = fn nm :: Name => <xml><textbox{nm}/></xml>,
         WidgetPopulated = fn (nm :: Name) n =>
                              <xml><textbox{nm} value={show _ n}/></xml>,
         Parse = readError _,
         Inject = _}

val b = {Nam = "B",
         Show = txt _,
         Widget = fn nm :: Name => <xml><textbox{nm}/></xml>,
         WidgetPopulated = fn (nm :: Name) s =>
                              <xml><textbox{nm} value={s}/></xml>,
         Parse = readError _,
         Inject = _}

val c = {Nam = "C",
         Show = txt _,
         Widget = fn nm :: Name => <xml><textbox{nm}/></xml>,
         WidgetPopulated = fn (nm :: Name) n =>
                              <xml><textbox{nm} value={show _ n}/></xml>,
         Parse = readError _,
         Inject = _}

val d = {Nam = "D",
         Show = txt _,
         Widget = fn nm :: Name => <xml><checkbox{nm}/></xml>,
         WidgetPopulated = fn (nm :: Name) b =>
                              <xml><checkbox{nm} checked={b}/></xml>,
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
