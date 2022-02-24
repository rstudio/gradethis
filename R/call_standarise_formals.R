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
  # If the function from the user code cannot be found, early exit because
  #   standardization is impossible
  fn <- tryCatch(call_fn(code, env), error = as.null)
  if (is.null(fn)) return(code)

  tryCatch(
    rlang::call_match(code, fn),
    error = function(e) {
      # Check that error is caused by an ambiguous partial argument
      # If not, return the code unaltered
      pattern <- paste0(
        "\\Q",
        gsub(
          pattern = "%d",
          replacement = "\\E\\d+\\Q",
          x = gettext("argument %d matches multiple formal arguments", domain = "R"),
          fixed = TRUE
        ),
        "\\E"
      )
      if (!grepl(pattern, e$message)) {
        return(code)
      }

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

# https://github.com/r-lib/rlang/blob/379b387af5ec29a583f206aa6bc6acd46378a6c5/R/lifecycle-deprecated.R#L775-L790
call_fn <- function(call, env = rlang::caller_env()) {
  expr <- rlang::get_expr(call)
  env <- rlang::get_env(call, env)

  stopifnot(rlang::is_call(expr))

  switch(call_type(expr),
         recursive = rlang::abort("`call` does not call a named or inlined function"),
         inlined = rlang::node_car(expr),
         named = ,
         namespaced = ,
         rlang::eval_bare(rlang::node_car(expr), env)
  )
}

# https://github.com/r-lib/rlang/blob/47df80809cfccd3e7c0a9ca456dbd14a2ecdf2a7/R/call.R#L994-L1010
call_type <- function(x) {
  x <- rlang::get_expr(x)
  stopifnot(typeof(x) == "language")

  type <- typeof(rlang::node_car(x))
  if (type == "symbol") {
    "named"
  } else if (is_namespaced_symbol(rlang::node_car(x))) {
    "namespaced"
  } else if (type == "language") {
    "recursive"
  } else if (type %in% c("closure", "builtin", "special")) {
    "inlined"
  } else {
    rlang::abort("corrupt language object")
  }
}

# https://github.com/r-lib/rlang/blob/47df80809cfccd3e7c0a9ca456dbd14a2ecdf2a7/R/call.R#L980-L992
is_namespaced_symbol <- function(x, ns = NULL, private = NULL) {
  if (typeof(x) != "language") return(FALSE)
  if (!is_null(ns) && !identical(rlang::node_cadr(x), rlang::sym(ns))) return(FALSE)

  head <- rlang::node_car(x)
  if (is_null(private)) {
    identical(head, quote(`::`)) || identical(head, quote(`:::`))
  } else if (private) {
    identical(head, quote(`:::`))
  } else {
    identical(head, quote(`::`))
  }
}
