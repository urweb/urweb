con fields :: Type -> Type -> {Type}

functor Make (M : Conference.INPUT) : Conference.OUTPUT where con paper = M.paper
                                                        where con userId = M.userId
                                                        where con paperId = M.paperId
                                                        where con yourPaperTables = [Assignment
                                                                                     = fields M.userId M.paperId]
