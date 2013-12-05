type datetime = {
     Year : int,
     Month : int,
     Day : int,
     Hour : int,
     Minute : int,
     Second : int
}

datatype day_of_week = Sunday | Monday | Tuesday | Wednesday | Thursday |
         Friday | Saturday

val show = mkShow (fn dow => case dow of
                                 Sunday => "Sunday"
                               | Monday => "Monday"
                               | Tuesday => "Tuesday"
                               | Wednesday => "Wednesday"
                               | Thursday => "Thursday"
                               | Friday => "Friday"
                               | Saturday => "Saturday")

fun toTime dt : time = fromDatetime dt.Year dt.Month dt.Day
                                    dt.Hour dt.Minute dt.Second

fun fromTime t : datetime = {
    Year = datetimeYear t,
    Month = datetimeMonth t,
    Day = datetimeDay t,
    Hour = datetimeHour t,
    Minute = datetimeMinute t,
    Second = datetimeSecond t
}

fun datetimef fmt dt : string = timef fmt (toTime dt)

val now : transaction datetime =
    n <- now;
    return (fromTime n)
