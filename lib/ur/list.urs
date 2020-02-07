datatype t = datatype Basis.list

val show : a ::: Type -> show a -> show (t a)
val eq : a ::: Type -> eq a -> eq (t a)

val foldl : a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b
val foldlAbort : a ::: Type -> b ::: Type -> (a -> b -> option b) -> b -> t a -> option b
val foldlMapAbort : a ::: Type -> b ::: Type -> c ::: Type
                    -> (a -> b -> option (c * b)) -> b -> t a -> option (t c * b)
val foldli : a ::: Type -> b ::: Type
             -> (int -> a -> b -> b) -> b -> t a -> b

val foldr : a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b

val length : a ::: Type -> t a -> int

val rev : a ::: Type -> t a -> t a

val revAppend : a ::: Type -> t a -> t a -> t a

val append : a ::: Type -> t a -> t a -> t a

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

val mapConcat : a ::: Type -> b ::: Type -> (a -> t b) -> t a -> t b

val mapConcatM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> (a -> m (t b)) -> t a -> m (t b)

val mapPartial : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val mapi : a ::: Type -> b ::: Type -> (int -> a -> b) -> t a -> t b

val mapX : a ::: Type -> ctx ::: {Unit} -> (a -> xml ctx [] []) -> t a -> xml ctx [] []

val mapXi : a ::: Type -> ctx ::: {Unit} -> (int -> a -> xml ctx [] []) -> t a -> xml ctx [] []

val mapM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
           -> (a -> m b) -> t a -> m (t b)

val mapMi : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
            -> (int -> a -> m b) -> t a -> m (t b)

val mapPartialM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> (a -> m (option b)) -> t a -> m (t b)
                                                                        
val mapXM : m ::: (Type -> Type) -> monad m -> a ::: Type -> ctx ::: {Unit}
            -> (a -> m (xml ctx [] [])) -> t a -> m (xml ctx [] [])

val mapXiM : m ::: (Type -> Type) -> monad m -> a ::: Type -> ctx ::: {Unit} -> (int -> a -> m (xml ctx [] [])) -> t a -> m (xml ctx [] [])

val filter : a ::: Type -> (a -> bool) -> t a -> t a

val exists : a ::: Type -> (a -> bool) -> t a -> bool

val existsM : m ::: (Type -> Type) -> monad m -> a ::: Type -> (a -> m bool) -> t a -> m bool

val foldlM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
             -> (a -> b -> m b) -> b -> t a -> m b

val foldlMi : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type
             -> (int -> a -> b -> m b) -> b -> t a -> m b

val filterM : m ::: (Type -> Type) -> monad m -> a ::: Type
              -> (a -> m bool) -> t a -> m (t a)

val foldlMap : a ::: Type -> b ::: Type -> c ::: Type
               -> (a -> b -> c * b) -> b -> t a -> t c * b

val mem : a ::: Type -> eq a -> a -> t a -> bool

val find : a ::: Type -> (a -> bool) -> t a -> option a

val findM : m ::: (Type -> Type) -> monad m -> a ::: Type -> (a -> m bool) -> t a -> m (option a)

val search : a ::: Type -> b ::: Type -> (a -> option b) -> t a -> option b

val searchM : m ::: (Type -> Type) -> monad m -> a ::: Type -> b ::: Type -> (a -> m (option b)) -> t a -> m (option b)

val all : a ::: Type -> (a -> bool) -> t a -> bool

val allM : m ::: (Type -> Type) -> monad m -> a ::: Type -> (a -> m bool) -> t a -> m bool

val app : m ::: (Type -> Type) -> monad m -> a ::: Type
          -> (a -> m unit) -> t a -> m unit

val appi : m ::: (Type -> Type) -> monad m -> a ::: Type
           -> (int -> a -> m unit) -> t a -> m unit

val tabulateM : m ::: (Type -> Type) -> monad m -> a ::: Type
                -> (int -> m a) -> int -> m (t a)

val mapQuery : tables ::: {{Type}} -> exps ::: {Type} -> t ::: Type
               -> [tables ~ exps] =>
    sql_query [] [] tables exps
    -> ($(exps ++ map (fn fields :: {Type} => $fields) tables) -> t)
    -> transaction (list t)

val mapQueryM : tables ::: {{Type}} -> exps ::: {Type} -> t ::: Type
               -> [tables ~ exps] =>
    sql_query [] [] tables exps
    -> ($(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction t)
    -> transaction (list t)

val mapQueryPartialM : tables ::: {{Type}} -> exps ::: {Type} -> t ::: Type
               -> [tables ~ exps] =>
    sql_query [] [] tables exps
    -> ($(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction (option t))
    -> transaction (list t)

val sort : a ::: Type -> (a -> a -> bool) (* > predicate *) -> t a -> t a

val nth : a ::: Type -> list a -> int -> option a
val replaceNth : a ::: Type -> list a -> int -> a -> list a

(** Association lists *)

val assoc : a ::: Type -> b ::: Type -> eq a -> a -> t (a * b) -> option b

val assocAdd : a ::: Type -> b ::: Type -> eq a -> a -> b -> t (a * b) -> t (a * b)

val assocAddSorted : a ::: Type -> b ::: Type -> eq a -> ord a -> a -> b -> t (a * b) -> t (a * b)
(* Assume the list is already sorted in ascending order and maintain that ordering. *)

(** Converting records to lists *)

val recToList : a ::: Type -> r ::: {Unit} -> folder r -> $(mapU a r) -> t a

(* Divide a list into two sections at a particular 0-based position, returning the second, first, or both parts, respectively. *)
val drop : t ::: Type -> int -> list t -> list t
val take : t ::: Type -> int -> list t -> list t
val splitAt : t ::: Type -> int -> list t -> list t * list t

(** Longest prefix of elements that satisfy a predicate, returned along with the remaining suffix *)
val span : a ::: Type -> (a -> bool) -> t a -> t a * t a

(** Group a list into maximal adjacent segments where all elements compare as equal, according to the provided predicate. *)
val groupBy : a ::: Type -> (a -> a -> bool) -> t a -> t (t a)
