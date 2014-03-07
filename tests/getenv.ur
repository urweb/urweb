task initialize = fn _ =>
  v <- getenv (blessEnvVar "USER");
  case v of
      None => debug "No USER"
    | Some u => debug u
