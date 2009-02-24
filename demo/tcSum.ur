fun sum (t ::: Type) (_ : num t) (fs ::: {Unit}) (fl : folder fs) (x : $(mapUT t fs)) =
    foldUR [t] [fn _ => t]
    (fn (nm :: Name) (rest :: {Unit}) [[nm] ~ rest] n acc => n + acc)
    zero [fs] fl x

fun main () = return <xml><body>
  {[sum (Folder.cons [#A] [()] ! (Folder.cons [#B] [()] ! Folder.nil)) {A = 0, B = 1}]}<br/>
  {[sum (Folder.cons [#D] [()] ! (Folder.cons [#C] [()] ! (Folder.cons [#E] [()] ! Folder.nil))) {C = 2.1, D = 3.2, E = 4.3}]}
</body></xml>
