functor Make(M : sig
                 con key :: {Type}
                 con data :: {Type}
                 constraint key ~ data
                 constraint [When] ~ (key ++ data)

                 val key : $(map sql_injectable key)
                 val data : $(map (fn t => {Inj : sql_injectable_prim t,
                                            Eq : eq t}) data)

                 val keyFolder : folder key
                 val dataFolder : folder data
             end) = struct
    con all = [When = time] ++ M.key ++ map option M.data
    table t : all

    val keys = List.mapQuery (SELECT DISTINCT t.{{M.key}} FROM t) (fn r => r.T)

    con dmeta = fn t => {Inj : sql_injectable_prim t,
                         Eq : eq t}

    fun keyRecd (r : $(M.key ++ M.data)) =
        map2 [sql_injectable] [id] [sql_exp [] [] []]
             (fn [t] => @sql_inject)
             [_] M.keyFolder M.key (r --- M.data)

    fun insert r = dml (Basis.insert t
                                     ({When = (SQL CURRENT_TIMESTAMP)}
                                          ++ keyRecd r
                                          ++ map2 [dmeta] [id]
                                          [fn t => sql_exp [] [] [] (option t)]
                                          (fn [t] x v => @sql_inject (@sql_option_prim x.Inj)
                                                          (Some v))
                                          [_] M.dataFolder M.data (r --- M.key)))

    fun keyExp (r : $M.key) : sql_exp [T = all] [] [] bool =
        foldR2 [sql_injectable] [id] [fn before => after :: {Type} -> [before ~ after]
                                         => sql_exp [T = before ++ after] [] [] bool]
               (fn [nm :: Name] [t :: Type] [before :: {Type}] [[nm] ~ before]
                                (inj : sql_injectable t) (v : t)
                   (e : after :: {Type} -> [before ~ after]
                    => sql_exp [T = before ++ after] [] [] bool)
                   [after :: {Type}] [[nm = t] ++ before ~ after] =>
                   (SQL t.{nm} = {[v]} AND {e [[nm = t] ++ after] !}))
               (fn [after :: {Type}] [[] ~ after] => (SQL TRUE))
               [_] M.keyFolder M.key r
               [_] !

    fun current k =
        let
            fun current' timeOpt r =
                let
                    val complete = foldR [option] [fn ts => option $ts]
                                   (fn [nm :: Name] [v :: Type] [r :: {Type}] [[nm] ~ r]
                                                    v r =>
                                       case (v, r) of
                                           (Some v, Some r) => Some ({nm = v} ++ r)
                                         | _ => None)
                                   (Some {}) [_] M.dataFolder r
                in
                    case complete of
                        Some r => return (Some r)
                      | None =>
                        let
                            val filter = case timeOpt of
                                             None => (WHERE TRUE)
                                           | Some time => (WHERE t.When < {[time]})
                        in
                            ro <- oneOrNoRows (SELECT t.When, t.{{map option M.data}}
                                               FROM t
                                               WHERE {filter}
                                                 AND {keyExp k}
                                               ORDER BY t.When DESC
                                               LIMIT 1);
                            case ro of
                                None => return None
                              | Some r' =>
                                let
                                    val r = map2 [option] [option] [option]
                                            (fn [t ::: Type] old new =>
                                                case old of
                                                    None => new
                                                  | Some _ => old)
                                            [_] M.dataFolder r (r'.T -- #When)
                                in
                                    current' (Some r'.T.When) r
                                end
                        end
                end
        in
            current' None (map0 [option] (fn [t :: Type] => None : option t) [_] M.dataFolder)
        end

    fun update r =
        cur <- current (r --- M.data);
        case cur of
            None => error <xml>Tried to update nonexistent key</xml>
          | Some cur =>
            let
                val r' = map3 [dmeta] [id] [id] [fn t => sql_exp [] [] [] (option t)]
                              (fn [t] (meta : dmeta t) old new =>
                                  @sql_inject (@sql_option_prim meta.Inj)
                                   (if @@eq [_] meta.Eq old new then
                                        None
                                    else
                                        Some new))
                              [_] M.dataFolder M.data cur (r --- M.key)
                val r' = {When = (SQL CURRENT_TIMESTAMP)}
                             ++ keyRecd r
                             ++ r'
            in
                dml (Basis.insert t r')
            end
end
