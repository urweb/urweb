table foo : { Bar : int, Baz : string }
  PRIMARY KEY Baz

fun main () : transaction page =
    rs <- queryX1 (SELECT foo.Bar FROM foo WHERE foo.Baz = 'Hi')
          (fn r => <xml>{[r.Bar]}</xml>);
    return <xml><body>{rs}</body></xml>
