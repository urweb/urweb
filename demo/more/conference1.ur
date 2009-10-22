open Conference.Make(struct
                         val paper = {}
                         val review = {}

                         val submissionDeadline = readError "2009-10-22 23:59:59"
                     end)
