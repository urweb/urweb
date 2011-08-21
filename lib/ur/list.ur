datatype t = datatype Basis.list

val show = fn [a] (_ : show a) =>
              let
                  fun show' (ls : list a) =
                      case ls of
                          [] => "[]"
                        | x :: ls => show x ^ " :: " ^ show' ls
              in
                  mkShow show'
              end

val eq = fn [a] (_ : eq a) =>
            let
                fun eq' (ls1 : list a) ls2 =
                    case (ls1, ls2) of
                        ([], []) => True
                      | (x1 :: ls1, x2 :: ls2) => x1 = x2 && eq' ls1 ls2
                      | _ => False
            in
                mkEq eq'
            end

fun foldl [a] [b] (f : a -> b -> b) =
    let
        fun foldl' acc ls =
            case ls of
                [] => acc
              | x :: ls => foldl' (f x acc) ls
    in
        foldl'
    end

val rev = fn [a] =>
             let
                 fun rev' acc (ls : list a) =
                     case ls of
                         [] => acc
                       | x :: ls => rev' (x :: acc) ls
             in
                 rev' []
             end

fun foldr [a] [b] f (acc : b) (ls : list a) = foldl f acc (rev ls)

fun foldlAbort [a] [b] f =
    let
        fun foldlAbort' acc ls =
            case ls of
                [] => Some acc
              | x :: ls =>
                case f x acc of
                    None => None
                  | Some acc' => foldlAbort' acc' ls
    in
        foldlAbort'
    end

val length = fn [a] =>
                let
                    fun length' acc (ls : list a) =
                        case ls of
                            [] => acc
                          | _ :: ls => length' (acc + 1) ls
                in
                    length' 0
                end

fun foldlMapAbort [a] [b] [c] f =
    let
        fun foldlMapAbort' ls' acc ls =
            case ls of
                [] => Some (rev ls', acc)
              | x :: ls =>
                case f x acc of
                    None => None
                  | Some (x', acc') => foldlMapAbort' (x' :: ls') acc' ls
    in
        foldlMapAbort' []
    end

val revAppend = fn [a] =>
                   let
                       fun ra (ls : list a) acc =
                           case ls of
                               [] => acc
                             | x :: ls => ra ls (x :: acc)
                   in
                       ra
                   end

fun append [a] (ls1 : t a) (ls2 : t a) = revAppend (rev ls1) ls2                

fun mp [a] [b] f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (f x :: acc) ls
    in
        mp' []
    end

fun mapi [a] [b] f =
    let
        fun mp' n acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (n + 1) (f n x :: acc) ls
    in
        mp' 0 []
    end

fun mapPartial [a] [b] f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (case f x of
                                    None => acc
                                  | Some y => y :: acc) ls
    in
        mp' []
    end

fun mapX [a] [ctx ::: {Unit}] f =
    let
        fun mapX' ls =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f x}{mapX' ls}</xml>
    in
        mapX'
    end

fun mapXi [a] [ctx ::: {Unit}] f =
    let
        fun mapX' i ls =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f i x}{mapX' (i + 1) ls}</xml>
    in
        mapX' 0
    end

fun mapM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls => x' <- f x; mapM' (x' :: acc) ls
    in
        mapM' []
    end

fun mapPartialM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapPartialM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls =>
                v <- f x;
                mapPartialM' (case v of
                                  None => acc
                                | Some x' => x' :: acc) ls
    in
        mapPartialM' []
    end

fun mapXM [m ::: (Type -> Type)] (_ : monad m) [a] [ctx ::: {Unit}] f =
    let
        fun mapXM' ls =
            case ls of
                [] => return <xml/>
              | x :: ls =>
                this <- f x;
                rest <- mapXM' ls;
                return <xml>{this}{rest}</xml>
    in
        mapXM'
    end

fun filter [a] f =
    let
        fun fil acc ls =
            case ls of
                [] => rev acc
              | x :: ls => fil (if f x then x :: acc else acc) ls
    in
        fil []
    end

fun exists [a] f =
    let
        fun ex ls =
            case ls of
                [] => False
              | x :: ls =>
                if f x then
                    True
                else
                    ex ls
    in
        ex
    end

fun foldlMap [a] [b] [c] f =
    let
        fun fold ls' st ls =
            case ls of
                [] => (rev ls', st)
              | x :: ls =>
                case f x st of
                    (y, st) => fold (y :: ls') st ls
    in
        fold []
    end

fun find [a] f =
    let
        fun find' ls =
            case ls of
                [] => None
              | x :: ls =>
                if f x then
                    Some x
                else
                    find' ls
    in
        find'
    end

fun search [a] [b] f =
    let
        fun search' ls =
            case ls of
                [] => None
              | x :: ls =>
                case f x of
                    None => search' ls
                  | v => v
    in
        search'
    end

fun foldlM [m] (_ : monad m) [a] [b] f =
    let
        fun foldlM' acc ls =
            case ls of
                [] => return acc
              | x :: ls =>
                acc <- f x acc;
                foldlM' acc ls
    in
        foldlM'
    end

fun foldlMi [m] (_ : monad m) [a] [b] f =
    let
        fun foldlMi' i acc ls =
            case ls of
                [] => return acc
              | x :: ls =>
                acc <- f i x acc;
                foldlMi' (i + 1) acc ls
    in
        foldlMi' 0
    end

fun all [m] f =
    let
        fun all' ls =
            case ls of
                [] => True
              | x :: ls => f x && all' ls
    in
        all'
    end

fun app [m] (_ : monad m) [a] f =
    let
        fun app' ls =
            case ls of
                [] => return ()
              | x :: ls =>
                f x;
                app' ls
    in
        app'
    end

fun mapQuery [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> t) =
    ls <- query q
                (fn fs acc => return (f fs :: acc))
                [];
    return (rev ls)

fun mapQueryM [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction t) =
    ls <- query q
                (fn fs acc => v <- f fs; return (v :: acc))
                [];
    return (rev ls)

fun mapQueryPartialM [tables ::: {{Type}}] [exps ::: {Type}] [t ::: Type]
             [tables ~ exps] (q : sql_query [] [] tables exps)
             (f : $(exps ++ map (fn fields :: {Type} => $fields) tables) -> transaction (option t)) =
    ls <- query q
                (fn fs acc => v <- f fs;
                    return (case v of
                                None => acc
                              | Some v => v :: acc))
                [];
    return (rev ls)

fun sort [a] (gt : a -> a -> bool) (ls : t a) : t a =
    let
        fun split ls acc1 acc2 =
            case ls of
                [] => (rev acc1, rev acc2)
              | x :: [] => (rev (x :: acc1), rev acc2)
              | x1 :: x2 :: ls' => split ls' (x1 :: acc1) (x2 :: acc2)

        fun merge ls1 ls2 acc =
            case (ls1, ls2) of
                ([], _) => revAppend acc ls2
              | (_, []) => revAppend acc ls1
              | (x1 :: ls1', x2 :: ls2') => if gt x1 x2 then merge ls1 ls2' (x2 :: acc) else merge ls1' ls2 (x1 :: acc)

        fun sort' ls =
            case ls of
                [] => ls
              | _ :: [] => ls
              | _ =>
                let
                    val (ls1, ls2) = split ls [] []
                in
                    merge (sort' ls1) (sort' ls2) []
                end
    in
        sort' ls
    end

val nth [a] =
    let
        fun nth (ls : list a) (n : int) : option a =
            case ls of
                [] => None
              | x :: ls' =>
                if n <= 0 then
                    Some x
                else
                    nth ls' (n-1)
    in
        nth
    end

fun replaceNth [a] (ls : list a) (n : int) (v : a) : list a =
    let
        fun repNth (ls : list a) (n : int) (acc : list a) =
            case ls of
                [] => rev acc
              | x :: ls' => if n <= 0 then
                                revAppend acc (v :: ls')
                            else
                                repNth ls' (n-1) (x :: acc)
    in
        repNth ls n []
    end

fun assoc [a] [b] (_ : eq a) (x : a) =
    let
        fun assoc' (ls : list (a * b)) =
            case ls of
                [] => None
              | (y, z) :: ls =>
                if x = y then
                    Some z
                else
                    assoc' ls
    in
        assoc'
    end

fun assocAdd [a] [b] (_ : eq a) (x : a) (y : b) (ls : t (a * b)) =
    case assoc x ls of
        None => (x, y) :: ls
      | Some _ => ls

fun recToList [a ::: Type] [r ::: {Unit}] (fl : folder r)
  = @foldUR [a] [fn _ => list a] (fn [nm ::_] [rest ::_] [[nm] ~ rest] x xs =>
				      x :: xs) [] fl
