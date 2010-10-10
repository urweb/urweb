table t : { A : int }
view v = SELECT t.A AS A FROM t WHERE t.A > 7

fun list [u] (_ : fieldsOf u [A = int]) (title : string) (x : u) : transaction xbody =
    xml <- queryX (SELECT * FROM x)
           (fn r : {X : {A : int}} => <xml><li>{[r.X.A]}</li></xml>);
    return <xml>
      <h2>{[title]}</h2>
      <ul>{xml}</ul>
    </xml>

fun main () =
    listT <- list "T" t;
    listV <- list "V" v;
    return <xml><body>
      {listT}
      {listV}
      <br/>

      <form>Insert: <textbox{#A}/> <submit action={ins}/></form>
    </body></xml>

and ins r =
    dml (INSERT INTO t (A) VALUES ({[readError r.A]}));
    main ()
