fun sum [t] (_ : num t) [fs ::: {Unit}] (fl : folder fs) (x : $(mapU t fs)) =
    @foldUR [t] [fn _ => t]
     (fn [nm :: Name] [rest :: {Unit}] [[nm] ~ rest] n acc => n + acc)
     zero fl x

fun main () = return <xml><body>
  {[sum {A = 0, B = 1}]}<br/>
  {[sum {C = 2.1, D = 3.2, E = 4.3}]}
</body></xml>
