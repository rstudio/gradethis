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

  # order and label existing params
  code_std <- call_standardise_keep_partials(code, env = env)

  if (is_infix(code) || is.primitive(fn)) {
    return(code_std)
  }

  fmls <- rlang::fn_fmls(fn)

  # if include_defaults == FALSE: standardize, but don't bother trying to fill
  # out default formals
  if (is_true(include_defaults)) {
    # get named arguments passed from user
    args_user <- rlang::call_args(code_std)
    args_user <- args_user[nzchar(names(args_user))]

    args_default <- fmls[!vapply(fmls, is.symbol, logical(1), USE.NAMES = FALSE)]
    args_default_missing <- names(args_default)[
      !grepl(paste0("^", names(args_user), collapse = "|"), names(args_default))
    ]
    if (length(args_default_missing) > 0) {
      ## Add implicit default args to the call
      code_std <- call_standardise_keep_partials(
        rlang::call_modify(code_std, !!!args_default[args_default_missing]),
        env = env
      )
    }
  }

  code_std <- call_standardise_passed_arguments(code_std, fn, fmls, env)

  if (rlang::is_installed("ggplot2") && is_ggplot2_function(fn)) {
    names(code_std) <- ggplot2::standardise_aes_names(names(code_std))
  }

  code_std
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
  if (rlang::is_bare_list(code)) {
    return(
      purrr::map(
        code,
        call_standardise_formals_recursive,
        env = env,
        include_defaults = include_defaults
      )
    )
  }

  # `code` must be parsed call
  if (!rlang::is_call(code)) {
    return(code)
  }

  code <- call_standardise_formals(
    code,
    env = env,
    include_defaults = include_defaults
  )
  code <- purrr::map(
    as.list(code),
    call_standardise_formals_recursive,
    env = env,
    include_defaults = include_defaults
  )
  as.call(code)
}

call_standardise_passed_arguments <- function(code, fn, fmls, env) {
  mappers <- mapping_function_list()
  if (!purrr::some(unlist(mappers), identical, fn)) {
    return(code)
  }

  dot_arg_indices <- which(!names(code) %in% names(fmls))[-1]
  dot_args <- as.list(code)[dot_arg_indices]
  dot_args <- dot_args_standardise(code, fn, mappers, dot_args, env)

  code_without_dot_args <- if (length(dot_arg_indices) > 0) {
    code[-dot_arg_indices]
  } else {
    code
  }

  code_list <- append(
    as.list(code_without_dot_args),
    dot_args,
    after = which(names(fmls) == "...")
  )

  as.call(code_list)
}

dot_args_standardise <- function(code, fn, mappers, dot_args, env) {
  if (purrr::some(mappers$map_functions, identical, fn)) {
    f_arg <- code$.f

    x_arg <- code$.x
    try(x_arg <- rlang::eval_bare(x_arg, env)[[1]], silent = TRUE)

    call <- rlang::call2(f_arg, x_arg, !!!dot_args)
    n_args <- 1
  } else if (purrr::some(mappers$map2_functions, identical, fn)) {
    f_arg <- code$.f

    x_arg <- code$.x
    try(x_arg <- rlang::eval_bare(x_arg, env)[[1]], silent = TRUE)

    y_arg <- code$.y
    try(y_arg <- rlang::eval_bare(y_arg, env)[[1]], silent = TRUE)

    call <- rlang::call2(f_arg, x_arg, y_arg, !!!dot_args)
    n_args <- 2
  } else if (purrr::some(mappers$imap_functions, identical, fn)) {
    f_arg <- code$.f

    x_args <- code$.x
    try(x_args <- rlang::eval_bare(x_args, env), silent = TRUE)

    x_arg <- x_args[[1]]
    y_arg <- names(x_args)[[1]]

    call <- rlang::call2(f_arg, x_arg, y_arg, !!!dot_args)
    n_args <- 2
  } else if (purrr::some(mappers$lmap_functions, identical, fn)) {
    f_arg <- code$.f

    x_arg <- code$.x
    try(x_arg <- rlang::eval_bare(x_arg, env)[1], silent = TRUE)

    call <- rlang::call2(f_arg, x_arg, !!!dot_args)
    n_args <- 1
  } else if (purrr::some(mappers$pmap_functions, identical, fn)) {
    f_arg <- code$.f

    l_arg <- code$.l
    try(l_arg <- rlang::eval_bare(l_arg, env), silent = TRUE)
    try(l_arg <- purrr::map(l_arg, 1), silent = TRUE)

    call <- rlang::call2(f_arg, !!!l_arg, !!!dot_args)
    n_args <- length(l_arg)
  } else if (purrr::some(mappers$apply_functions, identical, fn)) {
    f_arg <- code$FUN

    x_arg <- code$X
    try(x_arg <- rlang::eval_bare(x_arg, env)[[1]], silent = TRUE)

    call <- rlang::call2(f_arg, x_arg, !!!dot_args)
    n_args <- 1
  }

  return(as.list(call_standardise_formals(call))[-seq_len(n_args + 1)])
}

mapping_function_list <- function() {
  purrr <- utils::lsf.str(asNamespace("purrr"))

  list(
    map_functions = mget(str_subset(purrr, "^map(_.|$)"), asNamespace("purrr")),
    map2_functions = mget(str_subset(purrr, "map2(_.|$)"), asNamespace("purrr")),
    imap_functions = mget(str_subset(purrr, "imap(_.|$)"), asNamespace("purrr")),
    lmap_functions = mget(str_subset(purrr, "lmap(_.|$)"), asNamespace("purrr")),
    pmap_functions = mget(str_subset(purrr, "pmap(_.|$)"), asNamespace("purrr")),
    apply_functions = list(apply, lapply, sapply, tapply, vapply)
  )
}

call_fn <- function(code, env = parent.frame()) {
  if (rlang::is_quosure(code) || rlang::is_formula(code)) {
    code <- rlang::get_expr(code)
  }
  stopifnot(rlang::is_call(code))

  head <- code[[1]]

  fn <- rlang::eval_bare(head, env)
  stopifnot(rlang::is_function(fn))

  try_is_s3 <- purrr::possibly(utils::isS3stdGeneric, otherwise = FALSE)
  fn_is_s3_generic <- try_is_s3(fn)

  if (fn_is_s3_generic) {
    fn_name <- names(fn_is_s3_generic) %||% head
    try_get_s3_method <- purrr::possibly(get_s3_method, otherwise = NULL)
    fn <- try_get_s3_method(fn_name, arg = code[[2]], env = env) %||% fn
  }

  fn
}

get_s3_method <- function(fn_name, arg, env = parent.frame()) {
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

is_ggplot2_function <- function(fn) {
  identical(
    try(getNamespaceInfo(environment(fn), "spec")[["name"]], silent = TRUE),
    "ggplot2"
  )
}
