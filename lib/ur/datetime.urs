datatype day_of_week = Sunday | Monday | Tuesday | Wednesday | Thursday |
         Friday | Saturday

datatype month = January | February | March | April | May | June | July |
         August | September | October | November | December


type datetime = {
     Year : int,
     Month : month,
     Day : int,
     Hour : int,
     Minute : int,
     Second : int
}

val show_day_of_week : show day_of_week
val show_month : show month
val eq_day_of_week : eq day_of_week
val eq_month : eq month
val dayOfWeekToInt : day_of_week -> int
val intToDayOfWeek : int -> day_of_week
val monthToInt : month -> int
val intToMonth : int -> month

val toTime : datetime -> time
val fromTime : time -> datetime
val format : string -> datetime -> string
val dayOfWeek : datetime -> day_of_week
val now : transaction datetime
