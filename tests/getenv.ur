task initialize = fn _ =>
  v <- getenv (blessEnvVar "USER");
  return {}
