open Dbgrid

table t1 : {Id : int, A : string}
  PRIMARY KEY Id

sequence s
table t : {Id : int, A : int, B : string, C : bool, D : int, E : option int, F : option int}
  PRIMARY KEY Id,
  CONSTRAINT Foreign FOREIGN KEY (D) REFERENCES t1(Id) ON DELETE CASCADE

fun page (n, s) = return <xml>A = {[n]}, B = {[s]}</xml>

open Make(struct
              structure F = Direct.Foreign(struct
                                               con nm = #Id
                                               val tab = t1
                                               fun render r = r.A
                                           end)

              val tab = t
              con key = [Id = _]

              val raw = {Id = {New = nextval s,
                               Inj = _},
                         A = {New = return 0,
                              Inj = _},
                         B = {New = return "",
                              Inj = _},
                         C = {New = return False,
                              Inj = _},
                         D = {New = return 0,
                              Inj = _},
                         E = {New = return None,
                              Inj = _},
                         F = {New = return None,
                              Inj = _}}

              val cols = {Id = Direct.readOnly [#Id] "Id" Direct.int,
                          A = Direct.editable [#A] "A" Direct.int,
                          B = Direct.editable [#B] "B" Direct.string,
                          C = Direct.editable [#C] "C" Direct.bool,
                          D = Direct.editable [#D] "D" F.meta,
                          E = Direct.editable [#E] "E" (Direct.nullable Direct.int),
                          F = Direct.editable [#F] "F" (Direct.nullable F.meta),
                          DA = computed "2A" (fn r => 2 * r.A),
                          Link = computedHtml "Link" (fn r => <xml><a link={page (r.A, r.B)}>Go</a></xml>)}

              val aggregates = {Dummy1 = {Initial = (),
                                          Step = fn _ _ => (),
                                          Display = fn _ => <xml/>},
                                Sum = {Initial = 0,
                                       Step = fn r n => r.A + n,
                                       Display = txt},
                                Dummy2 = {Initial = (),
                                          Step = fn _ _ => (),
                                          Display = fn _ => <xml/>},
                                And = {Initial = True,
                                       Step = fn r b => r.C && b,
                                       Display = txt}}

              val pageLength = Some 10
          end)

fun main () =
    grid <- grid;
    set (showSelection grid) True;
    return <xml>
      <head>
        <link rel="stylesheet" type="text/css" href="../../grid.css"/>
      </head>
      <body onload={sync grid}>
        {render grid}
        <hr/>
        <ccheckbox source={showSelection grid}/> Show selection<br/>
        Selection: <dyn signal={ls <- selection grid;
                                return (List.mapX (fn r => <xml>{[r.Id]}; </xml>) ls)}/>
      </body>
    </xml>
