functor F(M : sig
              table t : { A : int, B : int }
                PRIMARY KEY A
          end) = struct
    open M

    fun getByA a = oneRow1 (SELECT * FROM t WHERE t.A = {[a]})
end

table u : { A : int, B : int }
  PRIMARY KEY A,
  CONSTRAINT B UNIQUE B

open F(struct
           val t = u
       end)
