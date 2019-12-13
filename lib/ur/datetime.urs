datatype day_of_week = Sunday | Monday | Tuesday | Wednesday | Thursday |
         Friday | Saturday

datatype month = January | February | March | April | May | June | July |
         August | September | October | November | December


type t = {
     Year : int,
     Month : month,
     Day : int,
     Hour : int,
     Minute : int,
     Second : int
}

val ord_datetime : ord t

val show_day_of_week : show day_of_week
val show_month : show month
val eq_day_of_week : eq day_of_week
val eq_month : eq month
val ord_month : ord month
val dayOfWeekToInt : day_of_week -> int
val intToDayOfWeek : int -> day_of_week
val monthToInt : month -> int
val intToMonth : int -> month

val toTime : t -> time
val fromTime : time -> t
val format : string -> t -> string
val dayOfWeek : t -> day_of_week
val now : transaction t
val normalize : t -> t

val addSeconds : int -> t -> t
val addMinutes : int -> t -> t
val addHours : int -> t -> t
val addDays : int -> t -> t
