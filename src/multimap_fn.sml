functor MultimapFn (structure KeyMap : ORD_MAP structure ValSet : ORD_SET) = struct
    type key = KeyMap.Key.ord_key
    type item = ValSet.item
    type items = ValSet.set
    type multimap = ValSet.set KeyMap.map
    fun inserts (kToVs : multimap, k : key, vs : items) : multimap =
        KeyMap.unionWith ValSet.union (kToVs, KeyMap.singleton (k, vs))
    fun insert (kToVs : multimap, k : key, v : item) : multimap =
        inserts (kToVs, k, ValSet.singleton v)
    fun find (kToVs : multimap, k : key) =
        case KeyMap.find (kToVs, k) of
            SOME vs => vs
          | NONE => ValSet.empty
end
