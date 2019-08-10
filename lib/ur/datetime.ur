datatype day_of_week = Sunday | Monday | Tuesday | Wednesday | Thursday |
         Friday | Saturday

val show_day_of_week = mkShow (fn dow => case dow of
                                          Sunday => "Sunday"
                                        | Monday => "Monday"
                                        | Tuesday => "Tuesday"
                                        | Wednesday => "Wednesday"
                                        | Thursday => "Thursday"
                                        | Friday => "Friday"
                                        | Saturday => "Saturday")

fun dayOfWeekToInt dow = case dow of
                             Sunday => 0
                           | Monday => 1
                           | Tuesday => 2
                           | Wednesday => 3
                           | Thursday => 4
                           | Friday => 5
                           | Saturday => 6

fun intToDayOfWeek i = case i of
                           0 => Sunday
                         | 1 => Monday
                         | 2 => Tuesday
                         | 3 => Wednesday
                         | 4 => Thursday
                         | 5 => Friday
                         | 6 => Saturday
                         | n => error <xml>Invalid day of week {[n]}</xml>

val eq_day_of_week = mkEq (fn a b => dayOfWeekToInt a = dayOfWeekToInt b)


datatype month = January | February | March | April | May | June | July |
         August | September | October | November | December

val show_month = mkShow (fn m => case m of
                                     January => "January"
                                   | February => "February"
                                   | March => "March"
                                   | April => "April"
                                   | May => "May"
                                   | June => "June"
                                   | July => "July"
                                   | August => "August"
                                   | September => "September"
                                   | October => "October"
                                   | November => "November"
                                   | December => "December")

type t = {
     Year : int,
     Month : month,
     Day : int,
     Hour : int,
     Minute : int,
     Second : int
}

fun monthToInt m = case m of
                       January => 0
                     | February => 1
                     | March => 2
                     | April => 3
                     | May => 4
                     | June => 5
                     | July => 6
                     | August => 7
                     | September => 8
                     | October => 9
                     | November => 10
                     | December => 11

fun intToMonth i = case i of
                       0 => January
                     | 1 => February
                     | 2 => March
                     | 3 => April
                     | 4 => May
                     | 5 => June
                     | 6 => July
                     | 7 => August
                     | 8 => September
                     | 9 => October
                     | 10 => November
                     | 11 => December
                     | n => error <xml>Invalid month number {[n]}</xml>

val eq_month = mkEq (fn a b => monthToInt a = monthToInt b)
val ord_month = mkOrd {Lt = fn a b => monthToInt a < monthToInt b,
                       Le = fn a b => monthToInt a <= monthToInt b}

fun toTime dt : time = fromDatetime dt.Year (monthToInt dt.Month) dt.Day
                                    dt.Hour dt.Minute dt.Second

fun fromTime t : t = {
    Year = datetimeYear t,
    Month = intToMonth (datetimeMonth t),
    Day = datetimeDay t,
    Hour = datetimeHour t,
    Minute = datetimeMinute t,
    Second = datetimeSecond t
}

val ord_datetime = mkOrd { Lt = fn a b => toTime a < toTime b,
                           Le = fn a b => toTime a <= toTime b }

fun format fmt dt : string = timef fmt (toTime dt)

fun dayOfWeek dt : day_of_week = intToDayOfWeek (datetimeDayOfWeek (toTime dt))

val now : transaction t =
    n <- now;
    return (fromTime n)

(* Normalize a datetime. This will convert, e.g., January 32nd into February
   1st. *)

fun normalize dt = fromTime (toTime dt)
fun addToField [nm :: Name] [rest ::: {Type}] [[nm] ~ rest]
               (delta : int) (r : $([nm = int] ++ rest))
    : $([nm = int] ++ rest) =
      (r -- nm) ++ {nm = r.nm + delta}


(* Functions for adding to a datetime. There is no addMonths or addYears since
   it's not clear what should be done; what's 1 month after January 31, or 1
   year after February 29th?

   These can't all be defined in terms of addSeconds because of leap seconds. *)

fun addSeconds n dt = normalize (addToField [#Second] n dt)
fun addMinutes n dt = normalize (addToField [#Minute] n dt)
fun addHours n dt = normalize (addToField [#Hour] n dt)
fun addDays n dt = normalize (addToField [#Day] n dt)
