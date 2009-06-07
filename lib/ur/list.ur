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

val rev = fn [a] =>
             let
                 fun rev' acc (ls : list a) =
                     case ls of
                         [] => acc
                       | x :: ls => rev' (x :: acc) ls
             in
                 rev' []
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

fun mapM [m ::: (Type -> Type)] (_ : monad m) [a] [b] f =
    let
        fun mapM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls => x' <- f x; mapM' (x' :: acc) ls
    in
        mapM' []
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

fun assoc [a] [b] (_ : eq a) (x : a) =
    let
        fun assoc' ls =
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

fun all [m] f =
    let
        fun all' ls =
            case ls of
                [] => True
              | x :: ls => f x && all' ls
    in
        all'
    end
