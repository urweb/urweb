type datetime = { Year : int,
                  Month : int,
                  Day : int,
                  Hour : int,
                  Minute : int,
                  Second : int
                }

val toTime : datetime -> time
val fromTime : time -> datetime
val datetimef : string -> datetime -> string
val now : transaction datetime
