fun main () : transaction page =
  (a, b) <- return (1, 2);
  {C = c, ...} <- return {C = "hi", D = False};
  d <- return 2.34;
  {1 = e, 2 = f} <- return (8, 9);
  return <xml>{[a]}, {[b]}, {[c]}, {[d]}, {[e]}, {[f]}</xml>
