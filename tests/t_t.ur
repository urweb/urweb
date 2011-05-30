table t : {A : int}

task initialize = fn () => dml (UPDATE t SET A = A + 1 WHERE TRUE)
