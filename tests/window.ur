table empsalary : { Depname : string,
                   Empno : int,
                   Salary : int }

fun main () : transaction page =
    x <- queryX (SELECT empsalary.Depname, empsalary.Empno, empsalary.Salary, RANK() AS R
                 FROM empsalary)
                (fn r => <xml>{[r.Empsalary.Depname]}, {[r.Empsalary.Empno]}, {[r.Empsalary.Salary]}, {[r.R]}<br/></xml>);
    return <xml><body>
      {x}
    </body></xml>
