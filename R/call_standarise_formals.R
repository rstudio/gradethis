call_standardise_formals <- function(code, env = rlang::current_env()) {
  
  # look up the function definition both to use and 
  # to spot invalid language objects
  tryCatch({
    fxn <- rlang::call_fn(code, env = env)
  }, error = function(e) {
    return(code)
  })
  
  ## some reason the above tryCatch doesn't go to the error part
  if (!exists("fxn")) {return(code)} 
  if(class(fxn) != "function") {return(code)}
  
  # standarise, but dont bother trying to fill out default formals for
  # primitives like mean, unable to distinguish between mean and mean.default
  if (is_infix(code) || is.primitive(fxn)) {
    return(rlang::call_standardise(code))
  }
  
  forms <- rlang::fn_fmls(fxn)
  default_params <- forms[!vapply(forms, is.symbol, logical(1), USE.NAMES = FALSE)]
  
  # match.call in call_standardise will throw an error if code contains
  # unmatched arguments (we'd prefer to inform the student), so add ... to
  # function definition if it is not already there.
  if (!("..." %in% names(forms))) {
    formals(fxn) <- eval(rlang::call2("alist", !!!forms, ... = ))
    env <- list(fxn)
    names(env) <- as.character(code[[1]]) # can every non-primitve call be coerced ot character?
    env <- as.environment(env)
  }
  
  # order and label existing params
  code_std <- rlang::call_standardise(code, env = env)
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

