structure M = struct
    con the_best_name = #Wiggles
    con the_runner_up = #Beppo
end

val x : {M.the_best_name : int, A : int, M.the_runner_up : int} =
  {M.the_best_name = 8, A = 9, M.the_runner_up = 10}
