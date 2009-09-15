con colMeta' = fn (row :: Type) (t :: Type) =>
                  {Header : string,
                   Project : row -> transaction t,
                   Update : row -> t -> transaction row,
                   Display : t -> xbody,
                   Edit : t -> xbody,
                   Validate : t -> signal bool}
                  
con colMeta = fn (row :: Type) (global_t :: (Type * Type)) =>
                 {Initialize : transaction global_t.1,
                  Handlers : global_t.1 -> colMeta' row global_t.2}

con aggregateMeta = fn (row :: Type) (acc :: Type) =>
                       {Initial : acc,
                        Step : row -> acc -> acc,
                        Display : acc -> xbody}

functor Make(M : sig
                 type row
                 type key
                 val keyOf : row -> key

                 val list : transaction (list row)
                 val new : transaction row
                 val save : key -> row -> transaction unit
                 val delete : key -> transaction unit

                 con cols :: {(Type * Type)}
                 val cols : $(map (colMeta row) cols)

                 val folder : folder cols

                 con aggregates :: {Type}
                 val aggregates : $(map (aggregateMeta row) aggregates)
             end) : sig
    type grid

    val grid : transaction grid
    val sync : grid -> transaction unit
    val render : grid -> xbody
                           
    style tabl
    style tr
    style th
    style td
end
