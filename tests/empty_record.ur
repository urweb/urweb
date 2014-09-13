val concatX [ctx ::: {Unit}] [use ::: {Type}]
    : list (xml ctx use []) -> xml ctx use []
  = List.foldl join <xml/>
