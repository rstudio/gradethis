call_standardise_formals <- function(code, env = rlang::current_env()) {
  # browser()
  
  # try to catch invalid function, i.e., corrupt language object
  tryCatch({
    fxn <- rlang::call_fn(code, env = env)
  }, error = function(e) {
    return(code)
  })
  if (!exists("fxn")) {return(code)} ## some reason the above tryCatch doesn't go to the error part
  if(class(fxn) != "function") {return(code)}
  
  # standarise, but dont bother trying to fill out default formals
  # for primitives like mean, unable to distinguish between mean and mean.default
  if (is_infix(code) || is.primitive(fxn)) {
    return(rlang::call_standardise(code))
  }
  
  forms <- rlang::fn_fmls(fxn)
  default_params <- forms[!vapply(forms, is.symbol, logical(1), USE.NAMES = FALSE)]
  
  code_std <- rlang::call_standardise(code, env = env) # order and label existing params
  
  code_params <- rlang::call_args(code_std) # get arguments passed from user
  code_missing_default_args <- default_params[!names(default_params) %in% names(code_params)]
  if (length(code_missing_default_args) == 0) {
    return(code_std)
  }
  return(
    rlang::call_standardise(rlang::call_modify(code_std, !!!code_missing_default_args),
                            env = env)
  )
}