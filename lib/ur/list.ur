datatype t = datatype Basis.list

val show (a ::: Type) (_ : show a) =
    let
        fun show' (ls : list a) =
            case ls of
                [] => "[]"
              | x :: ls => show x ^ " :: " ^ show' ls
    in
        mkShow show'
    end

val rev (a ::: Type) =
    let
        fun rev' acc (ls : list a) =
            case ls of
                [] => acc
              | x :: ls => rev' (x :: acc) ls
    in
        rev' []
    end

val revAppend (a ::: Type) =
    let
        fun ra (ls : list a) acc =
            case ls of
                [] => acc
              | x :: ls => ra ls (x :: acc)
    in
        ra
    end

fun append (a ::: Type) (ls1 : t a) (ls2 : t a) = revAppend (rev ls1) ls2                

fun mp (a ::: Type) (b ::: Type) f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (f x :: acc) ls
    in
        mp' []
    end

fun mapPartial (a ::: Type) (b ::: Type) f =
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

fun mapX (a ::: Type) (ctx ::: {Unit}) f =
    let
        fun mapX' ls =
            case ls of
                [] => <xml/>
              | x :: ls => <xml>{f x}{mapX' ls}</xml>
    in
        mapX'
    end

fun mapM (m ::: (Type -> Type)) (_ : monad m) (a ::: Type) (b ::: Type) f =
    let
        fun mapM' acc ls =
            case ls of
                [] => return (rev acc)
              | x :: ls => x' <- f x; mapM' (x' :: acc) ls
    in
        mapM' []
    end

fun filter (a ::: Type) f =
    let
        fun fil acc ls =
            case ls of
                [] => rev acc
              | x :: ls => fil (if f x then x :: acc else acc) ls
    in
        fil []
    end
