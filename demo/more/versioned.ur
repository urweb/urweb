functor Make(M : sig
                 con key :: {Type}
                 con data :: {Type}
                 constraint key ~ data
                 constraint [When, Version] ~ (key ++ data)

                 val key : $(map sql_injectable key)
                 val data : $(map (fn t => {Inj : sql_injectable_prim t,
                                            Eq : eq t}) data)

                 val keyFolder : folder key
                 val dataFolder : folder data
             end) = struct
    type version = int
    con all = [When = time, Version = version] ++ M.key ++ map option M.data
    sequence s
    table t : all

    val keys = List.mapQuery (SELECT DISTINCT t.{{M.key}} FROM t) (fn r => r.T)
    fun keysAt vr = List.mapQuery (SELECT DISTINCT t.{{M.key}} FROM t
                                   WHERE t.Version <= {[vr]}) (fn r => r.T)

    con dmeta = fn t => {Inj : sql_injectable_prim t,
                         Eq : eq t}

    fun keyRecd (r : $(M.key ++ M.data)) =
        @map2 [sql_injectable] [id] [sql_exp [] [] []]
         (fn [t] => @sql_inject)
         M.keyFolder M.key (r --- M.data)

    fun insert r =
        vr <- nextval s;
        dml (Basis.insert t
                          ({Version = (SQL {[vr]}), When = (SQL CURRENT_TIMESTAMP)}
                               ++ keyRecd r
                               ++ @map2 [dmeta] [id]
                               [fn t => sql_exp [] [] [] (option t)]
                               (fn [t] x v => @sql_inject (@sql_option_prim x.Inj)
                                               (Some v))
                               M.dataFolder M.data (r --- M.key)))

    fun keyExp (r : $M.key) : sql_exp [T = all] [] [] bool =
        @foldR2 [sql_injectable] [id] [fn before => after :: {Type} -> [before ~ after]
                                          => sql_exp [T = before ++ after] [] [] bool]
         (fn [nm :: Name] [t :: Type] [before :: {Type}] [[nm] ~ before]
                          (inj : sql_injectable t) (v : t)
                          (e : after :: {Type} -> [before ~ after]
                           => sql_exp [T = before ++ after] [] [] bool)
                          [after :: {Type}] [[nm = t] ++ before ~ after] =>
             (SQL t.{nm} = {[v]} AND {e [[nm = t] ++ after]}))
         (fn [after :: {Type}] [[] ~ after] => (SQL TRUE))
         M.keyFolder M.key r
         [_] !

    datatype bound =
             NoBound
           | Lt of int
           | Le of int

    fun seek vro k =
        let
            fun current' vro r =
                let
                    val complete = @foldR [option] [fn ts => option $ts]
                                    (fn [nm :: Name] [v :: Type] [r :: {Type}] [[nm] ~ r]
                                                     v r =>
                                        case (v, r) of
                                            (Some v, Some r) => Some ({nm = v} ++ r)
                                          | _ => None)
                                    (Some {}) M.dataFolder r
                in
                    case complete of
                        Some r => return (Some r)
                      | None =>
                        let
                            val filter = case vro of
                                             NoBound => (WHERE TRUE)
                                           | Lt vr => (WHERE t.Version < {[vr]})
                                           | Le vr => (WHERE t.Version <= {[vr]})
                        in
                            ro <- oneOrNoRows (SELECT t.Version, t.{{map option M.data}}
                                               FROM t
                                               WHERE {filter}
                                                 AND {keyExp k}
                                               ORDER BY t.When DESC
                                               LIMIT 1);
                            case ro of
                                None => return None
                              | Some r' =>
                                let
                                    val r = @map2 [option] [option] [option]
                                             (fn [t ::: Type] old new =>
                                                 case old of
                                                     None => new
                                                   | Some _ => old)
                                             M.dataFolder r (r'.T -- #Version)
                                in
                                    current' (Lt r'.T.Version) r
                                end
                        end
                end
        in
            current' vro (@map0 [option] (fn [t :: Type] => None : option t) M.dataFolder)
        end

    val current = seek NoBound
    fun archive vr = seek (Le vr)

    fun update r =
        cur <- current (r --- M.data);
        case cur of
            None => error <xml>Tried to update nonexistent key</xml>
          | Some cur =>
            vr <- nextval s;
            let
                val r' = @map3 [dmeta] [id] [id] [fn t => sql_exp [] [] [] (option t)]
                          (fn [t] (meta : dmeta t) old new =>
                              @sql_inject (@sql_option_prim meta.Inj)
                               (if @@eq [_] meta.Eq old new then
                                    None
                                else
                                    Some new))
                          M.dataFolder M.data cur (r --- M.key)
                val r' = {Version = (SQL {[vr]}), When = (SQL CURRENT_TIMESTAMP)}
                             ++ keyRecd r
                             ++ r'
            in
                dml (Basis.insert t r')
            end

    val updateTimes = List.mapQuery (SELECT t.Version, t.When
                                     FROM t
                                     ORDER BY t.When) (fn r => (r.T.Version, r.T.When))
end
