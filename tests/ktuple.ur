type q = (fn p => p.1) (int, float)
type q = (fn p => p.1 * $p.3) (int, float, [])
