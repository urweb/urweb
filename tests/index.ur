table t : {A : int, B : string, C : float}
  PRIMARY KEY A

ensure_index t : {A = equality}
ensure_index t : {B = equality, C = equality}
ensure_index t : {B = equality, C = equality}
ensure_index t : {B = trigram}
ensure_index t : {B = trigram}
ensure_index t : {B = trigram, C = equality}
