open ConferenceFields

open Conference.Make(struct
                         val paper = {Title = title,
                                      Abstract = abstract}
                         val review = {Rating = dropdown "Rating" (#"A" :: #"B" :: #"C" :: #"D" :: [])}

                         val submissionDeadline = readError "2009-11-22 23:59:59"

                         fun summarizePaper r = cdata r.Title

                         functor Make (M : Conference.INPUT where con paper = _) = struct
                             open Bid.Make(M)
                         end
                     end)
