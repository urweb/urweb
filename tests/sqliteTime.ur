table dates : { Date : time }

fun aform () =
    let
        val a : time = readError "01/02/03 04:06:07"
    in
        dml(INSERT INTO dates (Date) VALUES ({[a]}));
        ds <- queryX (SELECT * FROM dates)
                     (fn r => <xml>{[r.Dates.Date]}<br/></xml>);
        return <xml><body>{ds}</body></xml>
    end

fun main () =
  return <xml><body><form><submit action={aform}/></form></body></xml>
