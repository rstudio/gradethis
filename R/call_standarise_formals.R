call_standardise_formals <- function(code, env = rlang::current_env(), include_defaults = TRUE) {
  # try to catch invalid function, i.e., corrupt language object
  tryCatch({
    fn <- rlang::call_fn(code, env = env)
  }, error = function(e) {
    return(code)
  })
  
  if (!exists("fn") || !is_function(fn)) {
    ## if for some reason the above tryCatch doesn't go to the error part
    return(code)
  }

  # if include_defaults == FALSE standardize, but don't bother trying to fill
  # out default formals. For primitives like mean, we're unable to distinguish
  # between mean() and mean.default()
  if (is_false(include_defaults) || is_infix(code) || is.primitive(fn)) {
    return(rlang::call_standardise(code, env = env))
  }

  fmls <- rlang::fn_fmls(fn)
  args_default <- fmls[!vapply(fmls, is.symbol, logical(1), USE.NAMES = FALSE)]

  code_std <- rlang::call_standardise(code, env = env) # order and label existing params

  # get arguments passed from user
  args_user <- rlang::call_args(code_std)
  
  args_default_missing <- setdiff(names(args_default), names(args_user))
  if (length(args_default_missing) == 0) {
    return(code_std)
  }
  
  ## Add implicit default args to the call
  rlang::call_standardise(
    rlang::call_modify(code_std, !!!args_default[args_default_missing]), 
    env = env
  )
}
