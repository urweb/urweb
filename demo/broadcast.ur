functor Make(M : sig type t end) = struct
    sequence s
    table t : {Id : int, Client : client, Channel : channel M.t}
      PRIMARY KEY (Id, Client)

    type topic = int

    val inj : sql_injectable topic = _

    val create = nextval s

    fun subscribe id =
        cli <- self;
        ro <- oneOrNoRows (SELECT t.Channel FROM t WHERE t.Id = {[id]} AND t.Client = {[cli]});
        case ro of
            None =>
            ch <- channel;
            dml (INSERT INTO t (Id, Client, Channel) VALUES ({[id]}, {[cli]}, {[ch]}));
            return ch
          | Some r => return r.T.Channel

    fun send id msg =
        queryI (SELECT t.Channel FROM t WHERE t.Id = {[id]})
        (fn r => Basis.send r.T.Channel msg)

    fun subscribers id =
        r <- oneRow (SELECT COUNT( * ) AS N FROM t WHERE t.Id = {[id]});
        return r.N
end
