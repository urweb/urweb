con colMeta' = fn (row :: Type) (input :: Type) (filter :: Type) =>
                  {Header : string,
                   Project : row -> transaction input,
                   Update : row -> input -> transaction row,
                   Display : input -> xbody,
                   Edit : input -> xbody,
                   Validate : input -> signal bool,
                   CreateFilter : transaction filter,
                   DisplayFilter : filter -> xbody,
                   Filter : filter -> row -> signal bool,
                   Sort : option (row -> row -> bool)}
                  
con colMeta = fn (row :: Type) (global :: Type, input :: Type, filter :: Type) =>
                 {Initialize : transaction global,
                  Handlers : global -> colMeta' row input filter}

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

                 con cols :: {(Type * Type * Type)}
                 val cols : $(map (colMeta row) cols)

                 val folder : folder cols

                 con aggregates :: {Type}
                 val aggregates : $(map (aggregateMeta row) aggregates)
                 val aggFolder : folder aggregates

                 val pageLength : option int
             end) : sig
    type grid

    val grid : transaction grid
    val sync : grid -> transaction unit
    val render : grid -> xbody

    val showSelection : grid -> source bool
    val selection : grid -> signal (list M.row)

    style tab
    style row
    style header
    style data
    style agg
end
