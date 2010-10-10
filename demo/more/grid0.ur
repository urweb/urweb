open Dbgrid

sequence s
table t : {Id : int, A : int}
  PRIMARY KEY Id

open Make(struct
              val tab = t
              con key = [Id = _]

              val raw = {Id = {New = nextval s,
                               Inj = _},
                         A = {New = return 0,
                              Inj = _}}

              val cols = {Id = Direct.readOnly [#Id] "Id" Direct.int,
                          A = Direct.editable [#A] "A" Direct.int}

              val aggregates = {}

              val pageLength = None
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
      </body>
    </xml>
