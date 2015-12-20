functor MultimapFn (structure KeyMap : ORD_MAP structure ValSet : ORD_SET) = struct
    type key = KeyMap.Key.ord_key
    type item = ValSet.item
    type itemSet = ValSet.set
    type multimap = ValSet.set KeyMap.map
    val empty : multimap = KeyMap.empty
    fun insertSet (kToVs : multimap, k : key, vs : itemSet) : multimap =
        KeyMap.unionWith ValSet.union (kToVs, KeyMap.singleton (k, vs))
    fun insert (kToVs : multimap, k : key, v : item) : multimap =
        insertSet (kToVs, k, ValSet.singleton v)
    fun findSet (kToVs : multimap, k : key) =
        case KeyMap.find (kToVs, k) of
            SOME vs => vs
          | NONE => ValSet.empty
    val findList : multimap * key -> item list = ValSet.listItems o findSet
end
