type datetime = { Year : int,
                  Month : int,
                  Day : int,
                  Hour : int,
                  Minute : int,
                  Second : int
                }

datatype day_of_week = Sunday | Monday | Tuesday | Wednesday | Thursday |
         Friday | Saturday

val show : show day_of_week

val toTime : datetime -> time
val fromTime : time -> datetime
val datetimef : string -> datetime -> string
val now : transaction datetime
