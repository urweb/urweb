(** Row folding *)

con folder = K ==> fn r :: {K} =>
                      tf :: ({K} -> Type)
                      -> (nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
                          tf r -> tf ([nm = v] ++ r))
                      -> tf [] -> tf r

fun fold [K] [tf :: {K} -> Type]
         (f : (nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
               tf r -> tf ([nm = v] ++ r)))
         (i : tf []) [r ::: {K}] (fl : folder r) = fl [tf] f i

structure Folder = struct
    fun nil [K] [tf :: {K} -> Type]
            (f : nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
             tf r -> tf ([nm = v] ++ r))
            (i : tf []) = i

    fun cons [K] [r ::: {K}] [nm :: Name] [v :: K] [[nm] ~ r] (fold : folder r)
             [tf :: {K} -> Type]
             (f : nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
              tf r -> tf ([nm = v] ++ r))
             (i : tf []) = f [nm] [v] [r] (fold [tf] f i)

    fun concat [K] [r1 ::: {K}] [r2 ::: {K}] [r1 ~ r2]
        (f1 : folder r1) (f2 : folder r2)
        [tf :: {K} -> Type]
        (f : nm :: Name -> v :: K -> r :: {K} -> [[nm] ~ r] =>
         tf r -> tf ([nm = v] ++ r))
        (i : tf []) =
        f1 [fn r1' => [r1' ~ r2] => tf (r1' ++ r2)]
           (fn [nm :: Name] [v :: K] [r1' :: {K}] [[nm] ~ r1']
                            (acc : [r1' ~ r2] => tf (r1' ++ r2))
                            [[nm = v] ++ r1' ~ r2] =>
               f [nm] [v] [r1' ++ r2] acc)
           (fn [[] ~ r2] => f2 [tf] f i)

    fun mp [K1] [K2] [f ::: K1 -> K2] [r ::: {K1}]
        (fold : folder r)
        [tf :: {K2} -> Type]
        (f : nm :: Name -> v :: K2 -> r :: {K2} -> [[nm] ~ r] =>
         tf r -> tf ([nm = v] ++ r))
        (i : tf []) =
        fold [fn r => tf (map f r)]
        (fn [nm :: Name] [v :: K1] [rest :: {K1}] [[nm] ~ rest] (acc : tf (map f rest)) =>
            f [nm] [f v] [map f rest] acc)
        i
end


fun not b = if b then False else True

con id = K ==> fn t :: K => t
con record (t :: {Type}) = $t
con fst = K1 ==> K2 ==> fn t :: (K1 * K2) => t.1
con snd = K1 ==> K2 ==> fn t :: (K1 * K2) => t.2
con fst3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.1
con snd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.2
con thd3 = K1 ==> K2 ==> K3 ==> fn t :: (K1 * K2 * K3) => t.3

con mapU = K ==> fn f :: K => map (fn _ :: Unit => f)

con ex = K ==> fn tf :: (K -> Type) =>
            res ::: Type -> (choice :: K -> tf choice -> res) -> res

fun ex_intro [K] [tf :: K -> Type] [choice :: K] (body : tf choice) : ex tf =
 fn [res] (f : choice :: K -> tf choice -> res) =>
    f [choice] body

fun ex_elim [K] [tf ::: K -> Type] (v : ex tf) [res ::: Type] = @@v [res]

fun compose [t1 ::: Type] [t2 ::: Type] [t3 ::: Type]
            (f1 : t2 -> t3) (f2 : t1 -> t2) (x : t1) = f1 (f2 x)

fun show_option [t ::: Type] (_ : show t) =
    mkShow (fn opt : option t =>
               case opt of
                   None => ""
                 | Some x => show x)

fun read_option [t ::: Type] (_ : read t) =
    mkRead (fn s =>
               case s of
                   "" => None
                 | _ => Some (readError s : t))
           (fn s =>
               case s of
                   "" => Some None
                 | _ => case read s of
                            None => None
                          | v => Some v)

fun txt [t] [ctx ::: {Unit}] [use ::: {Type}] (_ : show t) (v : t) : xml ctx use [] =
    cdata (show v)

fun map0 [K] [tf :: K -> Type] (f : t :: K -> tf t) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc =>
        acc ++ {nm = f [t]})
    {}

fun mp [K] [tf1 :: K -> Type] [tf2 :: K -> Type] (f : t ::: K -> tf1 t -> tf2 t) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf1 r) -> $(map tf2 r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r =>
        acc (r -- nm) ++ {nm = f r.nm})
    (fn _ => {})

fun map2 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
         (f : t ::: K -> tf1 t -> tf2 t -> tf3 t) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r1 r2 =>
        acc (r1 -- nm) (r2 -- nm) ++ {nm = f r1.nm r2.nm})
    (fn _ _ => {})

fun map3 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tf :: K -> Type]
         (f : t ::: K -> tf1 t -> tf2 t -> tf3 t -> tf t) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> $(map tf r)]
    (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] acc r1 r2 r3 =>
        acc (r1 -- nm) (r2 -- nm) (r3 -- nm) ++ {nm = f r1.nm r2.nm r3.nm})
    (fn _ _ _ => {})

fun foldUR [tf :: Type] [tr :: {Unit} -> Type]
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
                      tf -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) [r ::: {Unit}] (fl : folder r) =
    fl [fn r :: {Unit} => $(mapU tf r) -> tr r]
       (fn [nm :: Name] [t :: Unit] [rest :: {Unit}] [[nm] ~ rest] acc r =>
           f [nm] [rest] r.nm (acc (r -- nm)))
       (fn _ => i)

fun foldUR2 [tf1 :: Type] [tf2 :: Type] [tr :: {Unit} -> Type]
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
                      tf1 -> tf2 -> tr rest -> tr ([nm] ++ rest))
           (i : tr []) [r ::: {Unit}] (fl : folder r) =
    fl [fn r :: {Unit} => $(mapU tf1 r) -> $(mapU tf2 r) -> tr r]
       (fn [nm :: Name] [t :: Unit] [rest :: {Unit}] [[nm] ~ rest] acc r1 r2 =>
           f [nm] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
       (fn _ _ => i)

fun foldR [K] [tf :: K -> Type] [tr :: {K} -> Type]
           (f : nm :: Name -> t :: K -> rest :: {K}
                -> [[nm] ~ rest] =>
                      tf t -> tr rest -> tr ([nm = t] ++ rest))
           (i : tr []) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf r) -> tr r]
       (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] (acc : _ -> tr rest) r =>
           f [nm] [t] [rest] r.nm (acc (r -- nm)))
       (fn _ => i)

fun foldR2 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tr :: {K} -> Type]
            (f : nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tr rest -> tr ([nm = t] ++ rest))
            (i : tr []) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> tr r]
       (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                        (acc : _ -> _ -> tr rest) r1 r2 =>
           f [nm] [t] [rest] r1.nm r2.nm (acc (r1 -- nm) (r2 -- nm)))
       (fn _ _ => i)

fun foldR3 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [tr :: {K} -> Type]
            (f : nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf1 t -> tf2 t -> tf3 t -> tr rest -> tr ([nm = t] ++ rest))
            (i : tr []) [r ::: {K}] (fl : folder r) =
    fl [fn r :: {K} => $(map tf1 r) -> $(map tf2 r) -> $(map tf3 r) -> tr r]
       (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] 
                        (acc : _ -> _ -> _ -> tr rest) r1 r2 r3 =>
           f [nm] [t] [rest] r1.nm r2.nm r3.nm (acc (r1 -- nm) (r2 -- nm) (r3 -- nm)))
       (fn _ _ _ => i)

fun mapUX [tf :: Type] [ctx :: {Unit}]
          (f : nm :: Name -> rest :: {Unit} -> [[nm] ~ rest] => tf -> xml ctx [] []) =
    @@foldR [fn _ => tf] [fn _ => xml ctx [] []]
      (fn [nm :: Name] [u :: Unit] [rest :: {Unit}] [[nm] ~ rest] r acc =>
          <xml>{f [nm] [rest] r}{acc}</xml>)
      <xml/>

fun mapX [K] [tf :: K -> Type] [ctx :: {Unit}]
            (f : nm :: Name -> t :: K -> rest :: {K}
                 -> [[nm] ~ rest] =>
                       tf t -> xml ctx [] []) =
    @@foldR [tf] [fn _ => xml ctx [] []]
      (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest] r acc =>
          <xml>{f [nm] [t] [rest] r}{acc}</xml>)
      <xml/>

fun mapUX2 [tf1 :: Type] [tf2 :: Type] [ctx :: {Unit}]
           (f : nm :: Name -> rest :: {Unit}
                -> [[nm] ~ rest] =>
            tf1 -> tf2 -> xml ctx [] []) =
    @@foldUR2 [tf1] [tf2] [fn _ => xml ctx [] []]
      (fn [nm :: Name] [rest :: {Unit}] [[nm] ~ rest] v1 v2 acc =>
          <xml>{f [nm] [rest] v1 v2}{acc}</xml>)
      <xml/>

fun mapX2 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [ctx :: {Unit}]
             (f : nm :: Name -> t :: K -> rest :: {K}
                  -> [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> xml ctx [] []) =
    @@foldR2 [tf1] [tf2] [fn _ => xml ctx [] []]
      (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest]
                       r1 r2 acc =>
          <xml>{f [nm] [t] [rest] r1 r2}{acc}</xml>)
      <xml/>

fun mapX3 [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type] [ctx :: {Unit}]
             (f : nm :: Name -> t :: K -> rest :: {K}
                  -> [[nm] ~ rest] =>
                        tf1 t -> tf2 t -> tf3 t -> xml ctx [] []) =
    @@foldR3 [tf1] [tf2] [tf3] [fn _ => xml ctx [] []]
      (fn [nm :: Name] [t :: K] [rest :: {K}] [[nm] ~ rest]
                       r1 r2 r3 acc =>
          <xml>{f [nm] [t] [rest] r1 r2 r3}{acc}</xml>)
      <xml/>

fun query1 [t ::: Name] [fs ::: {Type}] [state ::: Type] (q : sql_query [] [] [t = fs] [])
           (f : $fs -> state -> transaction state) (i : state) =
    query q (fn r => f r.t) i

fun query1' [t ::: Name] [fs ::: {Type}] [state ::: Type] (q : sql_query [] [] [t = fs] [])
            (f : $fs -> state -> state) (i : state) =
    query q (fn r s => return (f r.t s)) i

fun queryL [tables] [exps] [tables ~ exps] (q : sql_query [] [] tables exps) =
    query q
    (fn r ls => return (r :: ls))
    []

fun queryL1 [t ::: Name] [fs ::: {Type}] (q : sql_query [] [] [t = fs] []) =
    query q
    (fn r ls => return (r.t :: ls))
    []

fun queryI [tables ::: {{Type}}] [exps ::: {Type}]
           [tables ~ exps] (q : sql_query [] [] tables exps)
           (f : $(exps ++ map (fn fields :: {Type} => $fields) tables)
                -> transaction unit) =
    query q
          (fn fs _ => f fs)
          ()

fun queryI1 [nm ::: Name] [fs ::: {Type}] (q : sql_query [] [] [nm = fs] [])
            (f : $fs -> transaction unit) =
    query q
          (fn fs _ => f fs.nm)
          ()

fun queryX [tables ::: {{Type}}] [exps ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
           [tables ~ exps] (q : sql_query [] [] tables exps)
           (f : $(exps ++ map (fn fields :: {Type} => $fields) tables)
                -> xml ctx inp []) =
    query q
          (fn fs acc => return <xml>{acc}{f fs}</xml>)
          <xml/>

fun rev [a] (ls : list a) : list a =
    let
        fun rev' ls acc =
            case ls of
                [] => acc
              | x :: ls => rev' ls (x :: acc)
    in
        rev' ls []
    end

fun queryXI [tables ::: {{Type}}] [exps ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
            [tables ~ exps] (q : sql_query [] [] tables exps)
            (f : int -> $(exps ++ map (fn fields :: {Type} => $fields) tables)
                 -> xml ctx inp []) =
    let
        fun qxi ls i =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f i x}{qxi ls (i+1)}</xml>
    in
        ls <- queryL q;
        return (qxi (rev ls) 0)
    end

fun queryX1 [nm ::: Name] [fs ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
            (q : sql_query [] [] [nm = fs] [])
            (f : $fs -> xml ctx inp []) =
    query q
          (fn fs acc => return <xml>{acc}{f fs.nm}</xml>)
          <xml/>

fun queryX1I [nm ::: Name] [fs ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
             (q : sql_query [] [] [nm = fs] [])
             (f : int -> $fs -> xml ctx inp []) =
    let
        fun qx1i ls i =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f i x.nm}{qx1i ls (i+1)}</xml>
    in
        ls <- queryL q;
        return (qx1i (rev ls) 0)
    end

fun queryX' [tables ::: {{Type}}] [exps ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
            [tables ~ exps] (q : sql_query [] [] tables exps)
            (f : $(exps ++ map (fn fields :: {Type} => $fields) tables)
                 -> transaction (xml ctx inp [])) =
    query q
          (fn fs acc =>
              r <- f fs;
              return <xml>{acc}{r}</xml>)
          <xml/>

fun queryX1' [nm ::: Name] [fs ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
             (q : sql_query [] [] [nm = fs] [])
             (f : $fs -> transaction (xml ctx inp [])) =
    query q
          (fn fs acc =>
              r <- f fs.nm;
              return <xml>{acc}{r}</xml>)
          <xml/>

fun queryXE' [exps ::: {Type}] [ctx ::: {Unit}] [inp ::: {Type}]
             (q : sql_query [] [] [] exps)
             (f : $exps -> transaction (xml ctx inp [])) =
    query q
          (fn fs acc =>
              r <- f fs;
              return <xml>{acc}{r}</xml>)
          <xml/>

fun hasRows [tables ::: {{Type}}] [exps ::: {Type}]
            [tables ~ exps]
            (q : sql_query [] [] tables exps) =
    query q
          (fn _ _ => return True)
          False

fun oneOrNoRows [tables ::: {{Type}}] [exps ::: {Type}]
                [tables ~ exps]
                (q : sql_query [] [] tables exps) =
    query q
          (fn fs _ => return (Some fs))
          None

fun oneOrNoRows1 [nm ::: Name] [fs ::: {Type}] (q : sql_query [] [] [nm = fs] []) =
    query q
          (fn fs _ => return (Some fs.nm))
          None

fun oneOrNoRowsE1 [tabs ::: {Unit}] [nm ::: Name] [t ::: Type] [tabs ~ [nm]] (q : sql_query [] [] (mapU [] tabs) [nm = t]) =
    query q
          (fn fs _ => return (Some fs.nm))
          None

fun oneRow [tables ::: {{Type}}] [exps ::: {Type}]
           [tables ~ exps] (q : sql_query [] [] tables exps) =
    o <- oneOrNoRows q;
    return (case o of
                None => error <xml>Query returned no rows</xml>
              | Some r => r)

fun oneRow1 [nm ::: Name] [fs ::: {Type}] (q : sql_query [] [] [nm = fs] []) =
    o <- oneOrNoRows q;
    return (case o of
                None => error <xml>Query returned no rows</xml>
              | Some r => r.nm)

fun oneRowE1 [tabs ::: {Unit}] [nm ::: Name] [t ::: Type] [tabs ~ [nm]] (q : sql_query [] [] (mapU [] tabs) [nm = t]) =
    o <- oneOrNoRows q;
    return (case o of
                None => error <xml>Query returned no rows</xml>
              | Some r => r.nm)

fun nonempty [fs] [us] (t : sql_table fs us) =
    oneRowE1 (SELECT COUNT( * ) > 0 AS B FROM t)

fun eqNullable [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
    [t ::: Type] (_ : sql_injectable (option t))
    (e1 : sql_exp tables agg exps (option t))
    (e2 : sql_exp tables agg exps (option t)) =
    (SQL ({e1} IS NULL AND {e2} IS NULL) OR {e1} = {e2})

fun eqNullable' [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
    [t ::: Type] (_ : sql_injectable (option t))
    (e1 : sql_exp tables agg exps (option t))
    (e2 : option t) =
    case e2 of
        None => (SQL {e1} IS NULL)
      | Some _ => sql_binary sql_eq e1 (sql_inject e2)

fun mkRead' [t ::: Type] (f : string -> option t) (name : string) : read t =
    mkRead (fn s => case f s of
                        None => error <xml>Invalid {txt name}: {txt s}</xml>
                      | Some v => v) f
