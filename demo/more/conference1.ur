open ConferenceFields

open Conference.Make(struct
                         val paper = {Title = title,
                                      Abstract = abstract}
                         val review = {Rating = dropdown "Rating" (#"A" :: #"B" :: #"C" :: #"D" :: []),
                                       CommentsForAuthors = commentsForAuthors}

                         val submissionDeadline = readError "2009-11-22 23:59:59"

                         fun summarizePaper [ctx] [[Body] ~ ctx] r = cdata r.Title

                         functor Make (M : Conference.INPUT where con paper = [Title = string, Abstract = string]) = struct
                             open Bid.Make(M)
                         end
                     end)
