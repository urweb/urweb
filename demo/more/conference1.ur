open ConferenceFields

open Conference.Make(struct
                         val paper = {Title = title,
                                      Abstract = abstract}
                         val review = {}

                         val submissionDeadline = readError "2009-10-22 23:59:59"

                         fun summarizePaper r = cdata r.Title
                     end)
