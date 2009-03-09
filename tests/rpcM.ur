datatype list t = Nil | Cons of t * list t

sequence s

fun main () : transaction page =
    let
        fun getIndices srcs =
            case srcs of
                Nil => return Nil
              | Cons (src, srcs') =>
                i <- nextval s;
                set src i;
                ls <- getIndices srcs';
                return (Cons (i, ls))

        fun show ls =
            case ls of
                Nil => <xml/>
              | Cons (x, ls') => <xml>{[x]}<br/>{show ls'}</xml>
    in
        src1 <- source 0;
        src2 <- source 1;
        s <- source Nil;
        return <xml><body>
          <button value="Get It On!"
                  onclick={ns <- getIndices (Cons (src1, Cons (src2, Nil)));
                           set s ns}/><br/>
          <br/>
          #1: <dyn signal={n <- signal src1; return <xml>{[n]}</xml>}/><br/>
          #2: <dyn signal={n <- signal src2; return <xml>{[n]}</xml>}/><br/>
          Current: <dyn signal={ns <- signal s; return (show ns)}/>
        </body></xml>
    end
