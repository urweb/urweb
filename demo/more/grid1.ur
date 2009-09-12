open Dbgrid

table t1 : {Id : int, A : string}
  PRIMARY KEY Id

sequence s
table t : {Id : int, A : int, B : string, C : bool, D : int, E : option int}
  PRIMARY KEY Id,
  CONSTRAINT Foreign FOREIGN KEY (D) REFERENCES t1(Id) ON DELETE CASCADE

(*fun page (n, s) = return <xml>A = {[n]}, B = {[s]}</xml>*)

open Make(struct
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
                              Inj = _}}

              structure F = Direct.Foreign(struct
                                               con nm = #Id
                                               val tab = t1
                                               fun render r = r.A
                                           end)

              val cols = {Id = Direct.readOnly [#Id] ! "Id" Direct.int,
                          A = Direct.editable [#A] ! "A" Direct.int,
                          B = Direct.editable [#B] ! "B" Direct.string,
                          C = Direct.editable [#C] ! "C" Direct.bool(*,
                          D = Direct.editable [#D] ! "D" F.meta,
                          E = Direct.editable [#E] ! "E" (Direct.nullable Direct.int),
                          DA = computed "2A" (fn r => 2 * r.A),
                          Link = computedHtml "Link" (fn r => <xml><a link={page (r.A, r.B)}>Go</a></xml>)*)}
          end)

fun main () =
    grid <- grid;
    return <xml>
      <head>
        <link rel="stylesheet" type="text/css" href="../../grid.css"/>
      </head>
      <body onload={sync grid}>
        {render grid}
      </body>
    </xml>
