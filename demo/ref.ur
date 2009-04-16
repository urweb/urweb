structure IR = RefFun.Make(struct
                               type data = int
                           end)

structure SR = RefFun.Make(struct
                               type data = string
                           end)

fun mutate () =
    ir <- IR.new 3;
    ir' <- IR.new 7;
    sr <- SR.new "hi";

    IR.write ir' 10;

    iv <- IR.read ir;
    iv' <- IR.read ir';
    sv <- SR.read sr;

    IR.delete ir;
    IR.delete ir';
    SR.delete sr;

    return <xml><body>
      {[iv]}, {[iv']}, {[sv]}
    </body></xml>

fun main () = return <xml><body>
  <form><submit action={mutate} value="Do some pointless stuff"/></form>
</body></xml>
