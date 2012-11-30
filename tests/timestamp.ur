table t : { A : time }

fun many ls =
    case ls of
        [] => (WHERE TRUE)
      | tm :: ls' => (WHERE t.A = {[tm]} AND {many ls'})

task initialize = fn () =>
                     tm <- now;
                     dml (DELETE FROM t WHERE {many (tm :: [])})

