functor Make(M : sig
                 type data
                 val inj : sql_injectable data
             end) = struct

    type ref = int

    sequence s
    table t : { Id : int, Data : M.data }
      PRIMARY KEY Id

    fun new d =
        id <- nextval s;
        dml (INSERT INTO t (Id, Data) VALUES ({[id]}, {[d]}));
        return id

    fun read r =
        o <- oneOrNoRows (SELECT t.Data FROM t WHERE t.Id = {[r]});
        case o of
            None => error <xml>You already deleted that ref!</xml>
          | Some r => return r.T.Data

    fun write r d =
        dml (UPDATE t SET Data = {[d]} WHERE Id = {[r]})

    fun delete r =
        dml (DELETE FROM t WHERE Id = {[r]})

end
