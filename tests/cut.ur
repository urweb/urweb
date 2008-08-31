val r = {A = 1, B = "Hi", C = 0.0}
val rA = r -- #A

val main : unit -> page = fn () => <html><body>
        {cdata rA.B}
</body></html>
