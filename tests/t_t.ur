table t : {A : int, B : string}

task initialize = fn () => dml (UPDATE t SET A = A + 1 WHERE TRUE);
                     dml (UPDATE t SET B = 'q' WHERE TRUE)
