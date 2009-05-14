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

fun mp (a ::: Type) (b ::: Type) f =
    let
        fun mp' acc ls =
            case ls of
                [] => rev acc
              | x :: ls => mp' (f x :: acc) ls
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
                [] => acc
              | x :: ls => mapM' (x' <- f x; ls' <- acc; return (x' :: ls')) ls
    in
        mapM' (return [])
    end
