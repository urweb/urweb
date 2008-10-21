fun sum (fs :: {Unit}) (x : $(mapUT int fs)) =
    foldUR [int] [fn _ => int]
    (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] n acc => n + acc)
    0 [fs] x

fun main () = return <xml><body>
  {[sum [[A, B]] {A = 0, B = 1}]}<br/>
  {[sum [[C, D, E]] {C = 2, D = 3, E = 4}]}
</body></xml>
