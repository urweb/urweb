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


fun toTime dt : time = fromDatetime dt.Year (monthToInt dt.Month) dt.Day
                                    dt.Hour dt.Minute dt.Second

fun fromTime t : datetime = {
    Year = datetimeYear t,
    Month = intToMonth (datetimeMonth t),
    Day = datetimeDay t,
    Hour = datetimeHour t,
    Minute = datetimeMinute t,
    Second = datetimeSecond t
}

fun format fmt dt : string = timef fmt (toTime dt)

fun dayOfWeek dt : day_of_week =
    case datetimeDayOfWeek (toTime dt) of
        0 => Sunday
      | 1 => Monday
      | 2 => Tuesday
      | 3 => Wednesday
      | 4 => Thursday
      | 5 => Friday
      | 6 => Saturday
      | n => error <xml>Illegal day of week {[n]}</xml>


val now : transaction datetime =
    n <- now;
    return (fromTime n)
