call_standardise_formals <- function(code, env = rlang::current_env(), include_defaults = TRUE) {
  # try to catch invalid function, i.e., corrupt language object
  tryCatch({
    fn <- call_fn(code, env = env)
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
      # Check that error is caused by an ambiguous partial argument, which we
      # are forced to identify from its error message. The \Q...\E regex meta
      # characters mark the text inside them as literal. This way we can use the
      # literal, translated error message, but replace the `%d` with `\d+`.
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

  code <- purrr::map(as.list(code), call_standardise_formals_recursive)
  code <- as.call(code)
  call_standardise_formals(code)
}

call_fn <- function(code, env = parent.frame()) {
  if (rlang::is_quosure(code) || rlang::is_formula(code)) {
    code <- rlang::get_expr(code)
  }
  stopifnot(rlang::is_call(code))

  head <- code[[1]]

  fn <- rlang::eval_bare(head, env)
  stopifnot(rlang::is_function(fn))

  fn_is_s3_generic <- isS3stdGeneric(fn)
  fn_name <- names(fn_is_s3_generic)

  if (fn_is_s3_generic) {
    tryCatch(
      fn <- resolve_s3_generic(fn_name, arg = code[[2]], env = env) %||% fn,
      error = function(e) return(fn)
    )
  }

  fn
}

resolve_s3_generic <- function(fn_name, arg, env = parent.frame()) {
  class <- expand_class(arg, env)

  while (length(class) > 0) {
    method <- utils::getS3method(
      fn_name,
      class[[1]],
      optional = TRUE,
      envir = env
    )

    if (!is.null(method)) {
      break
    }

    class <- class[-1]
  }

  method
}

expand_class <- function(arg, env) {
  arg <- rlang::eval_bare(arg, env)
  class <- unique(class(arg))

  if ("array" %in% class) {
    non_array_arg <- arg
    dim(non_array_arg) <- NULL
    non_array_class <- class(non_array_arg)
    class <- unique(append(class, non_array_class))
  }

  if ("numeric" %in% class) {
    class <- unique(append(class, "double", which(class == "numeric") - 1))
  }

  if ("integer" %in% class) {
    class <- unique(append(class, "numeric", which(class == "integer")))
  }

  class <- unique(append(class, "default"))
  class
}

# Backport of `utils::isS3stdGeneric()` from R 4.1.0
#
#  File src/library/utils/R/objects.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/
isS3stdGeneric <- function(f) {
  bdexpr <- body(if(methods::is(f, "traceable")) f@original else f)
  ## protect against technically valid but bizarre
  ## function(x) { { { UseMethod("gen")}}} by
  ## repeatedly consuming the { until we get to the first non { expr
  while(is.call(bdexpr) && bdexpr[[1L]] == "{")
    bdexpr <- bdexpr[[2L]]

  ## We only check if it is a "standard" s3 generic. i.e. the first non-{
  ## expression is a call to UseMethod. This will return FALSE if any
  ## work occurs before the UseMethod call ("non-standard" S3 generic)
  ret <- is.call(bdexpr) && bdexpr[[1L]] == "UseMethod"
  if(ret)
    names(ret) <- bdexpr[[2L]] ## arg passed to UseMethod naming generic
  ret
}
