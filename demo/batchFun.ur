con colMeta = fn t_state :: (Type * Type) =>
                 {Nam : string,
                  Show : t_state.1 -> xbody,
                  Inject : sql_injectable t_state.1,

                  NewState : transaction t_state.2,
                  Widget : t_state.2 -> xbody,
                  ReadState : t_state.2 -> transaction t_state.1}
con colsMeta = fn cols :: {(Type * Type)} => $(map colMeta cols)

fun default (t ::: Type) (sh : show t) (rd : read t) (inj : sql_injectable t)
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
                          
                 val tab : sql_table ([Id = int] ++ map fst cols)
                           
                 val title : string
                             
                 val cols : colsMeta cols
             end) = struct

    open constraints M
    val t = M.tab

    datatype list t = Nil | Cons of t * list t

    fun allRows () =
        query (SELECT * FROM t)
              (fn r acc => return (Cons (r.T, acc)))
              Nil

    fun add r =
        dml (insert t
                    (foldR2 [fst] [colMeta]
                            [fn cols => $(map (fn t :: (Type * Type) =>
                                                  sql_exp [] [] [] t.1) cols)]
                            (fn (nm :: Name) (t :: (Type * Type)) (rest :: {(Type * Type)})
                                             [[nm] ~ rest] input col acc =>
                                acc ++ {nm = @sql_inject col.Inject input})
                            {} [M.cols] M.fl (r -- #Id) M.cols
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
                      {foldRX2 [colMeta] [fst] [_]
                               (fn (nm :: Name) (p :: (Type * Type)) (rest :: {(Type * Type)})
                                                [[nm] ~ rest] m v =>
                                   <xml><td>{m.Show v}</td></xml>)
                               [M.cols] M.fl M.cols (r -- #Id)}
                      {if withDel then
                           <xml><td><button value="Delete" onclick={del r.Id}/></td></xml>
                       else
                           <xml/>}
                    </tr>
                    {show' ls}
                  </xml>
        in
            <xml><dyn signal={ls <- signal lss; return <xml><table>
              <tr>
                <th>Id</th>
                {foldRX [colMeta] [_]
                        (fn (nm :: Name) (p :: (Type * Type)) (rest :: {(Type * Type)})
                                         [[nm] ~ rest] m =>
                            <xml><th>{[m.Nam]}</th></xml>)
                        [M.cols] M.fl M.cols}
              </tr>
              {show' ls}
            </table></xml>}/></xml>
        end

    fun main () =
        lss <- source Nil;
        batched <- source Nil;

        id <- source "";
        inps <- foldR [colMeta] [fn r => transaction ($(map snd r))]
                (fn (nm :: Name) (p :: (Type * Type)) (rest :: {(Type * Type)}) [[nm] ~ rest] m acc =>
                    s <- m.NewState;
                    r <- acc;
                    return ({nm = s} ++ r))
                (return {})
                [M.cols] M.fl M.cols;

        let
            fun add () =
                id <- get id;
                vs <- foldR2 [colMeta] [snd] [fn r => transaction ($(map fst r))]
                             (fn (nm :: Name) (p :: (Type * Type)) (rest :: {(Type * Type)})
                                              [[nm] ~ rest] m s acc =>
                                 v <- m.ReadState s;
                                 r <- acc;
                                 return ({nm = v} ++ r))
                             (return {})
                             [M.cols] M.fl M.cols inps;
                ls <- get batched;

                set batched (Cons ({Id = readError id} ++ vs, ls))

            fun exec () =
                ls <- get batched;

                doBatch ls;
                set batched Nil
        in
            return <xml><body>
              <h2>Rows</h2>

              {show True lss}

              <button value="Update" onclick={ls <- allRows (); set lss ls}/><br/>
              <br/>

              <h2>Batch new rows to add</h2>

              <table>
                <tr> <th>Id:</th> <td><ctextbox source={id}/></td> </tr>
                {foldRX2 [colMeta] [snd] [_]
                 (fn (nm :: Name) (p :: (Type * Type)) (rest :: {(Type * Type)})
                                  [[nm] ~ rest] m s =>
                     <xml><tr> <th>{[m.Nam]}:</th> <td>{m.Widget s}</td> </tr></xml>)
                 [M.cols] M.fl M.cols inps}
                <tr> <th/> <td><button value="Batch it" onclick={add ()}/></td> </tr>
              </table>

              <h2>Already batched:</h2>
              {show False batched}
              <button value="Execute" onclick={exec ()}/>
            </body></xml>
        end

end
