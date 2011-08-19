datatype t = datatype Basis.option

val monad = mkMonad {Return = @@Some,
                     Bind = fn [a] [b] (m1 : t a) (m2 : a -> t b) =>
                               case m1 of
                                   None => None
                                 | Some v => m2 v}

fun eq [a] (_ : eq a) =
    mkEq (fn x y =>
             case (x, y) of
                 (None, None) => True
               | (Some x, Some y) => x = y
               | _ => False)

fun ord [a] (_ : ord a) =
    mkOrd {Lt = fn x y =>
                   case (x, y) of
                       (None, Some _) => True
                     | (Some x, Some y) => x < y
                     | _ => False,
           Le = fn x y =>
                   case (x, y) of
                       (None, _) => True
                     | (Some x, Some y) => x <= y
                     | _ => False}

fun isNone [a] x =
    case x of
        None => True
      | Some _ => False

fun isSome [a] x =
    case x of
        None => False
      | Some _ => True

fun mp [a] [b] f x =
    case x of
        None => None
      | Some y => Some (f y)

fun bind [a] [b] f x =
    case x of
        None => None
      | Some y => f y

fun get [a] (x : a) (o : option a) =
    case o of
        None => x
      | Some v => v
