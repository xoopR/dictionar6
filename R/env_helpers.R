ls_env <- function(env) {
  names(as_list_env(env))
}

length_env <- function(env) {
  length(as_list_env(env))
}

as_list_env <- function(env) {
  c(
    as.list(env),
    tryCatch(as_list_env(parent.env(env)), error = function(e) NULL)
  )
}

where_env <- function(nm, env) {
  if (exists(nm, env, inherits = FALSE)) {
    env
  } else {
    if (!identical(parent.env(env), emptyenv())) {
      where_env(nm, parent.env(env))
    } else {
      stop(sprintf("value for '%s' not found", nm))
    }
  }
}

rm_env <- function(nm, env) {
  rm(list = nm, envir = where_env(nm, env))
}

update_env_value <- function(nm, value, env) {
  if (exists(nm, env, inherits = FALSE)) {
    env[[nm]] <- value
  } else {
    update_env_value(nm, parent.env(env))
  }
}

rename_env <- function(nm, new_nm, env) {
  if (exists(nm, env, inherits = FALSE)) {
    env[[new_nm]] <- env[[nm]]
    rm(list = nm, envir = env)
  } else {
    rename_env(nm, new_nm, parent.env(env))
  }
}
