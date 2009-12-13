sequence seq
table fred : {A : int, B : int}

initializer
    setval seq 1;
    dml (INSERT INTO fred (A, B) VALUES (0, 1))
