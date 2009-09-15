datatype dlist'' t =
         Nil
       | Cons of t * source (dlist'' t)

datatype dlist' t =
         Empty
       | Nonempty of { Head : dlist'' t, Tail : source (source (dlist'' t)) }

con dlist t = {Filter: t -> signal bool,
               Elements : source (dlist' t)}

type position = transaction unit

fun headPos [t] (dl : dlist t) =
    dl' <- get dl.Elements;
    case dl' of
        Nonempty { Head = Cons (_, tl), Tail = tl' } =>
        cur <- get tl;
        set dl.Elements (case cur of
                             Nil => Empty
                           | _ => Nonempty {Head = cur, Tail = tl'})
      | _ => return ()

fun tailPos [t] (cur : source (dlist'' t)) new tail =
    new' <- get new;
    set cur new';

    case new' of
        Nil => set tail cur
      | _ => return ()

fun create [t] r =
    s <- source Empty;
    return {Filter = r.Filter,
            Elements = s}

fun clear [t] (s : dlist t) = set s.Elements Empty

fun append [t] dl v =
    dl' <- get dl.Elements;
    case dl' of
        Empty =>
        tl <- source Nil;
        tl' <- source tl;
        set dl.Elements (Nonempty {Head = Cons (v, tl), Tail = tl'});
        return (headPos dl)
                
      | Nonempty {Tail = tl, ...} =>
        cur <- get tl;
        new <- source Nil;
        set cur (Cons (v, new));
        set tl new;
        return (tailPos cur new tl)

fun render [ctx] [ctx ~ body] [t] f dl = <xml>
  <dyn signal={dl' <- signal dl.Elements;
               return (case dl' of
                           Empty => <xml/>
                         | Nonempty {Head = hd, Tail = tlTop} => 
                           let
                               fun render' prev dl'' =
                                   case dl'' of
                                       Nil => <xml/>
                                     | Cons (v, tl) =>
                                       let
                                           val pos = case prev of
                                                         None => headPos dl
                                                       | Some prev => tailPos prev tl tlTop
                                       in
                                           <xml><dyn signal={b <- dl.Filter v;
                                                             return (if b then
                                                                         f v pos
                                                                     else
                                                                         <xml/>)}/>
                                             <dyn signal={tl' <- signal tl;
                                                          return (render' (Some tl) tl')}/></xml>
                                       end
                           in
                               render' None hd
                           end)}/>
</xml>

fun delete pos = pos

fun elements' [t] (dl'' : dlist'' t) =
    case dl'' of
        Nil => return []
      | Cons (x, dl'') =>
        dl'' <- signal dl'';
        tl <- elements' dl'';
        return (x :: tl)

fun elements [t] (dl : dlist t) =
    dl' <- signal dl.Elements;
    case dl' of
        Empty => return []
      | Nonempty {Head = hd, ...} => elements' hd

fun foldl [t] [acc] (f : t -> acc -> signal acc) =
    let
        fun foldl'' (i : acc) (dl : dlist'' t) : signal acc =
            case dl of
                Nil => return i
              | Cons (v, dl') =>
                dl' <- signal dl';
                i' <- f v i;
                foldl'' i' dl'

        fun foldl' (i : acc) (dl : dlist t) : signal acc =
            dl <- signal dl.Elements;
            case dl of
                Empty => return i
              | Nonempty {Head = dl, ...} => foldl'' i dl
    in
        foldl'
    end
