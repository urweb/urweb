open ConferenceFields

open Conference.Make(struct
                         val paper = {Title = title,
                                      Abstract = abstract}
                         val paperPrivate = {Decision = Decision.decision}
                         val review = {Rating = dropdown "Rating" (#"A" :: #"B" :: #"C" :: #"D" :: []),
                                       CommentsForAuthors = commentsForAuthors}

                         val submissionDeadline = readError "2009-11-22 23:59:59"

                         fun summarizePaper [ctx] [[Body] ~ ctx] r = txt r.Title
                         functor Make (M : Conference.INPUT where con paper = _
                                                            where con review = _) = struct
                             open M

                             fun status [ctx] [[Body] ~ ctx] r =
                                 queryX (SELECT review.Rating
                                         FROM review
                                         WHERE review.Paper = {[r.Id]})
                                        (fn r => <xml>{[r.Review.Rating]}; </xml>)

                             open Conference.Join(struct
                                                      structure O1 = Bid.Make(M)
                                                      structure O2 = Decision.Make(struct
                                                                                       con paperOther = _
                                                                                       open M

                                                                                       val status = @@status
                                                                                   end)
                                                  end)
                         end
                     end)
