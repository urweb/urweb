fun sum [fs ::: {Unit}] (fl : folder fs) (x : $(mapU int fs)) =
    @foldUR [int] [fn _ => int]
     (fn [nm :: Name] [rest :: {Unit}] [[nm] ~ rest] n acc => n + acc)
     0 fl x

fun main () = return <xml><body>
  {[sum {}]}<br/>
  {[sum {A = 0, B = 1}]}<br/>
  {[sum {C = 2, D = 3, E = 4}]}
</body></xml>
