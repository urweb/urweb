table empsalary : { Depname : string,
                    Empno : int,
                    Salary : int }

fun main () : transaction page =
    x <- queryX (SELECT empsalary.Depname, empsalary.Empno, empsalary.Salary,
                   RANK() OVER (PARTITION BY empsalary.Depname ORDER BY empsalary.Salary DESC) AS R,
                   AVG(empsalary.Salary) OVER (PARTITION BY empsalary.Depname) AS A
                 FROM empsalary)
                (fn r => <xml>{[r.Empsalary.Depname]}, {[r.Empsalary.Empno]}, {[r.Empsalary.Salary]}, {[r.R]}, {[r.A]}<br/></xml>);
    return <xml><body>
      {x}
    </body></xml>
