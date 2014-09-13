table images : { Id : int, Content : blob }
table tags : { Id : int, Tag : string }

datatype mode = Present | Absent
type condition = { Tag : string, Mode : mode }

type tag_query = sql_query [] [] [] [Id = int]

fun addCondition (c : condition) (q : tag_query) : tag_query =
    case c.Mode of
        Present => (SELECT I.Id AS Id
                    FROM ({{q}}) AS I
                      JOIN tags ON tags.Id = I.Id AND tags.Tag = {[c.Tag]})
      | Absent => (SELECT I.Id AS Id
                   FROM ({{q}}) AS I
                     LEFT JOIN tags ON tags.Id = I.Id AND tags.Tag = {[c.Tag]}
                   WHERE tags.Tag IS NULL)

fun withConditions (cs : list condition) : tag_query =
    List.foldl addCondition (SELECT images.Id AS Id FROM images) cs

fun main (cs : list condition) : transaction page =
    x <- queryX (withConditions cs) (fn r => <xml><li>{[r.Id]}</li></xml>);
    return <xml><body>
      {x}
    </body></xml>
