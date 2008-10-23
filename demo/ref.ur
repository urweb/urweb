structure IR = RefFun.Make(struct
                               type t = int
                               val inj = _
                           end)

structure SR = RefFun.Make(struct
                               type t = string
                               val inj = _
                           end)

fun main () =
    ir <- IR.new 3;
    ir' <- IR.new 7;
    sr <- SR.new "hi";

    () <- IR.write ir' 10;

    iv <- IR.read ir;
    iv' <- IR.read ir';
    sv <- SR.read sr;

    () <- IR.delete ir;
    () <- IR.delete ir';
    () <- SR.delete sr;

    return <xml><body>
      {[iv]}, {[iv']}, {[sv]}
    </body></xml>
