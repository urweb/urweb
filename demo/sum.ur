fun sum (fs ::: {Unit}) (fl : folder fs) (x : $(mapUT int fs)) =
    foldUR [int] [fn _ => int]
    (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] n acc => n + acc)
    0 [fs] fl x

fun main () = return <xml><body>
  {[sum Folder.nil {}]}<br/>
  {[sum (Folder.cons [#A] [()] ! (Folder.cons [#B] [()] ! Folder.nil)) {A = 0, B = 1}]}<br/>
  {[sum (Folder.cons [#D] [()] ! (Folder.cons [#C] [()] ! (Folder.cons [#E] [()] ! Folder.nil))) {C = 2, D = 3, E = 4}]}
</body></xml>
