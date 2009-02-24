table t1 : {Id : int, A : int, B : string, C : float, D : bool}

open Crud.Make(struct
                   val tab = t1
                             
                   val title = "Crud1"
                               
                   val cols = {A = Crud.int "A",
                               B = Crud.string "B",
                               C = Crud.float "C",
                               D = Crud.bool "D"}

                   val fl = Folder.cons [#A] [_] !
                                        (Folder.cons [#B] [_] !
                                                     (Folder.cons [#C] [_] !
                                                                  (Folder.cons [#D] [_] !
                                                                               Folder.nil)))
               end)
