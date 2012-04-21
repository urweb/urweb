fun main [r] (r : $([A = int] ++ ([B = float] ++ r))) : $([A = float] ++ ([B = int] ++ r)) = r
