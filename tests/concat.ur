functor Make(M : sig
                 con ts :: {(Type * Type)}
                 val tab : sql_table (map fst ts) []
                 val cols : $(map (fn p => p.2 -> string) ts)
             end) = struct
end

table t : {A : string}

open Make(struct
              val tab = t
              val cols = {A = fn p : {B : string, C : string} => p.B ^ p.C}
          end)
