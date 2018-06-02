fun main () : transaction page =
  (a, b) <- return (1, 2);
  {C = c, ...} <- return {C = "hi", D = False};
  let
  val d = 2.34
  val {1 = e, 2 = f} = (8, 9)
  in
  return <xml>{[a]}, {[b]}, {[c]}, {[d]}, {[e]}, {[f]}</xml>
  end