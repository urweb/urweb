open Meta

functor Make(M : sig
                 con keyName :: Name
                 con keyType :: Type
                 val showKey : show keyType
                 val readKey : read keyType
                 val injKey : sql_injectable keyType

                 con visible :: {(Type * Type)}
                 constraint [keyName] ~ visible
                 val folder : folder visible
                 val visible : $(map Meta.meta visible)

                 con invisible :: {Type}
                 constraint [keyName] ~ invisible
                 constraint visible ~ invisible

                 val title : string
                 val isAllowed : transaction bool
                 table t : ([keyName = keyType] ++ map fst visible ++ invisible)
             end) = struct

    open M

    fun ensql [avail] (r : $(map snd visible)) : $(map (sql_exp avail [] []) (map fst visible)) =
        map2 [meta] [snd] [fn ts :: (Type * Type) => sql_exp avail [] [] ts.1]
             (fn [ts] meta v => @sql_inject meta.Inject (meta.Parse v))
             [_] folder visible r

    fun main () =
        items <- queryX (SELECT t.{keyName}, t.{{map fst visible}} FROM t)
                 (fn r => <xml><entry><tr>
                   <hidden{keyName} value={show r.T.keyName}/>
                   {useMore (foldR2 [meta] [fst] [fn cols :: {(Type * Type)} =>
                                            xml [Body, Form, Tr] [] (map snd cols)]
                                    (fn [nm :: Name] [p :: (Type * Type)] [rest :: {(Type * Type)}] [[nm] ~ rest]
                                                     (m : meta p) v (acc : xml [Body, Form, Tr] [] (map snd rest)) => 
                                        <xml>
                                          <td>{m.WidgetPopulated [nm] v}</td>
                                          {useMore acc}
                                        </xml>)
                                    <xml/>
                                    [_] folder visible (r.T -- keyName))}
                 </tr></entry></xml>);
        
        return <xml><body>
          <h1>{[title]}</h1>

          <form><table>
            <tr>{foldRX [meta] [_]
                 (fn [nm :: Name] [p :: (Type * Type)] [rest :: {(Type * Type)}] [[nm] ~ rest] m =>
                     <xml><th>{[m.Nam]}</th></xml>) [_] folder visible}</tr>
            <subforms{#Users}>{items}</subforms>
            <tr> <td><submit value="Save" action={save}/></td> </tr>
          </table></form>
        </body></xml>

    and save r =
        List.app (fn user => dml (update [map fst visible] !
                                  (ensql (user -- keyName))
                                  t
                                  (WHERE t.{keyName} = {[readError user.keyName]}))) r.Users;
        main ()

end
