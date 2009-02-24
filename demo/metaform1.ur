open Metaform.Make(struct
                       val names = {A = "Tic", B = "Tac", C = "Toe"}
                       val fl = Folder.cons [#A] [()] ! (Folder.cons [#B] [()] ! (Folder.cons [#C] [()] ! Folder.nil))
                   end)
