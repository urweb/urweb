con colMeta = fn (db :: Type, state :: Type) =>
                 {Nam : string,
                  Show : db -> xbody,
                  Inject : sql_injectable db,

                  NewState : transaction state,
                  Widget : state -> xbody,
                  ReadState : state -> transaction db}
con colsMeta = fn cols => $(map colMeta cols)

fun default [t] (sh : show t) (rd : read t) (inj : sql_injectable t)
            name : colMeta (t, source string) =
    {Nam = name,
     Show = txt,
     Inject = _,

     NewState = source "",
     Widget = fn s => <xml><ctextbox source={s}/></xml>,
     ReadState = fn s => v <- get s; return (readError v)}

val int = default
val float = default
val string = default

functor Make(M : sig
                 con cols :: {(Type * Type)}
                 constraint [Id] ~ cols
                 val fl : folder cols
                          
                 table tab : ([Id = int] ++ map fst cols)
                           
                 val title : string
                             
                 val cols : colsMeta cols
             end) = struct

    val t = M.tab

    datatype list t = Nil | Cons of t * list t

    fun allRows () =
        query (SELECT * FROM t)
              (fn r acc => return (Cons (r.T, acc)))
              Nil

    fun add r =
        dml (insert t
                    (@foldR2 [fst] [colMeta]
                      [fn cols => $(map (fn t => sql_exp [] [] [] t.1) cols)]
                      (fn [nm :: Name] [t ::_] [rest ::_] [[nm] ~ rest] input col acc =>
                          acc ++ {nm = @sql_inject col.Inject input})
                      {} M.fl (r -- #Id) M.cols
                      ++ {Id = (SQL {[r.Id]})}))

    fun doBatch ls =
        case ls of
            Nil => return ()
          | Cons (r, ls') =>
            add r;
            doBatch ls'

    fun del id =
        dml (DELETE FROM t WHERE t.Id = {[id]})

    fun show withDel lss =
        let
            fun show' ls =
                case ls of
                    Nil => <xml/>
                  | Cons (r, ls) => <xml>
                    <tr>
                      <td>{[r.Id]}</td>
                      {@mapX2 [colMeta] [fst] [_]
                        (fn [nm :: Name] [p ::_] [rest ::_] [[nm] ~ rest] m v =>
                            <xml><td>{m.Show v}</td></xml>)
                        M.fl M.cols (r -- #Id)}
                      {if withDel then
                           <xml><td><button value="Delete" onclick={rpc (del r.Id)}/></td></xml>
                       else
                           <xml/>}
                    </tr>
                    {show' ls}
                  </xml>
        in
            <xml><dyn signal={ls <- signal lss; return <xml><table>
              <tr>
                <th>Id</th>
                {@mapX [colMeta] [tr]
                  (fn [nm :: Name] [p ::_] [rest ::_] [[nm] ~ rest] m =>
                      <xml><th>{[m.Nam]}</th></xml>)
                  M.fl M.cols}
              </tr>
              {show' ls}
            </table></xml>}/></xml>
        end

    fun main () =
        lss <- source Nil;
        batched <- source Nil;

        id <- source "";
        inps <- @foldR [colMeta] [fn r => transaction ($(map snd r))]
                 (fn [nm :: Name] [p ::_] [rest ::_] [[nm] ~ rest] m acc =>
                     s <- m.NewState;
                     r <- acc;
                     return ({nm = s} ++ r))
                 (return {})
                 M.fl M.cols;
        
        let
            fun add () =
                id <- get id;
                vs <- @foldR2 [colMeta] [snd] [fn r => transaction ($(map fst r))]
                       (fn [nm :: Name] [p ::_] [rest ::_] [[nm] ~ rest] m s acc =>
                           v <- m.ReadState s;
                           r <- acc;
                           return ({nm = v} ++ r))
                       (return {})
                       M.fl M.cols inps;
                ls <- get batched;

                set batched (Cons ({Id = readError id} ++ vs, ls))

            fun exec () =
                ls <- get batched;

                rpc (doBatch ls);
                set batched Nil
        in
            return <xml><body>
              <h2>Rows</h2>

              {show True lss}

              <button value="Update" onclick={ls <- rpc (allRows ()); set lss ls}/><br/>
              <br/>

              <h2>Batch new rows to add</h2>

              <table>
                <tr> <th>Id:</th> <td><ctextbox source={id}/></td> </tr>
                {@mapX2 [colMeta] [snd] [_]
                  (fn [nm :: Name] [p ::_] [rest ::_] [[nm] ~ rest] m s =>
                      <xml><tr> <th>{[m.Nam]}:</th> <td>{m.Widget s}</td> </tr></xml>)
                  M.fl M.cols inps}
                <tr> <th/> <td><button value="Batch it" onclick={add ()}/></td> </tr>
              </table>

              <h2>Already batched:</h2>
              {show False batched}
              <button value="Execute" onclick={exec ()}/>
            </body></xml>
        end

end
