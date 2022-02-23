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
    return(call_standardise_keep_partials(code, env = env))
  }

  fmls <- rlang::fn_fmls(fn)
  args_default <- fmls[!vapply(fmls, is.symbol, logical(1), USE.NAMES = FALSE)]

  # order and label existing params
  code_std <- call_standardise_keep_partials(code, env = env)

  # get named arguments passed from user
  args_user <- rlang::call_args(code_std)
  args_user <- args_user[nzchar(names(args_user))]

  args_default_missing <- names(args_default)[
    !grepl(paste0("^", names(args_user), collapse = "|"), names(args_default))
  ]
  if (length(args_default_missing) == 0) {
    return(code_std)
  }

  ## Add implicit default args to the call
  call_standardise_keep_partials(
    rlang::call_modify(code_std, !!!args_default[args_default_missing]),
    env = env
  )
}

call_standardise_keep_partials <- function(code, env = rlang::caller_env()) {
  tryCatch(
    rlang::call_standardise(code, env = env),
    error = function(e) {
      # Find index of (first) problematic partial match from error message
      # "argument 1 matches multiple formal arguments"
      index <- as.integer(str_extract(e$message, "\\d+")) + 1

      # Try to standardize the call while ignoring the un-matchable arg
      standardised_call <- call_standardise_keep_partials(code[-index])

      # Reassemble original call, by re-adding unaltered problematic arguments
      # to the standardized call
      rlang::call_modify(standardised_call, !!!as.list(code[index]))
    }
  )
}

call_standardise_formals_recursive <- function( # nolint
  code, env = rlang::current_env(), include_defaults = TRUE
) {
  if (is.list(code)) {
    return(lapply(code, call_standardise_formals_recursive))
  }

  # `code` must be parsed call
  if (!rlang::is_call(code)) {
    return(code)
  }

  code <- purrr::map(code, call_standardise_formals_recursive)
  code <- as.call(code)
  call_standardise_formals(code)
}
