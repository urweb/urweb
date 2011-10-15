type r1 = {A : string, B : string}
type r2 = {B : string, A : string}

val show_r1 : show r1 = mkShow (fn r => r.A ^ "+" ^ r.B)
val show_r2 : show r2 = _
