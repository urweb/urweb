table t : { Url : url }

task initialize = fn () =>
     dml (INSERT INTO t (Url) VALUES ({[bless "http://www.google.com/"]}))
