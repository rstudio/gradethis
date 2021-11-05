#' unpipe
#'
#' Unpipe one layer of piped code.
#'
#' `unpipe()` removes one call to \code{\link[magrittr]{\%>\%}} from an expression,
#' reformatting the result as a nested function call. If the code does not
#' contain a top-level call to \code{\link[magrittr]{\%>\%}}, `unpipe()` returns it as is.
#'
#' @param code a quoted piece of code
#' @noRd
unpipe <- function(code) {

  # Ceci n'est pas une pipe
  if (!is_pipe(code)) return(code)

  # une pipe
  lhs <- code[[2]]
  rhs <- code[[3]]

  if (!is.call(rhs)) {
    # rhs need to be a call
    # mainly because some user do `1 %>% print` instead of `1 %>% print()`
    rhs <-  call(deparse(rhs))
  }


  if (length(rhs) == 1) {
    rhs[[2]] <- lhs
    return(rhs)
  }

  dot <- purrr::map_lgl(as.list(rhs), is_dot)
  if (any(dot)) {
    rhs[[which(dot)]] <- lhs
  } else {
    rhs <- as.call(c(list(rhs[[1]], lhs), as.list(rhs[2:length(rhs)])))
  }
  rhs
}

is_dot <- function(name) {
  length(name) == 1 && as.character(name) == "."
}

unpipe_all <- function(code_expr, .top_level = TRUE) {
  if (is.character(code_expr) && .top_level) {
    code_expr <- str2expression(code_expr)
    if (length(code_expr) == 1) {
      return(unpipe_all(code_expr[[1]]))
    } else {
      return(purrr::map(as.list(code_expr), unpipe_all, .top_level = FALSE))
    }
  }
  code_expr_len <- length(code_expr)
  if (code_expr_len == 0) return(code_expr)
  if (code_expr_len == 1) return(code_expr)
  if (code_expr_len == 2 && is.null(code_expr[[2]])) return(code_expr)
  if (is_function_definition(code_expr)) {
    # Remove source ref information to be safe
    code_expr <- code_expr[-4]
  }
  re_call <- if (is.pairlist(code_expr)) as.pairlist else as.call
  code_expr <- re_call(purrr::map(as.list(code_expr), unpipe_all, .top_level = FALSE))
  unpipe(code_expr)
}

unpipe_all_str <- function(code, ..., collapse = "\n") {
  code_unpiped <- unpipe_all(code)
  expr_text <- purrr::partial(rlang::expr_text, ...)
  if (!is.list(code_unpiped)) {
    return(expr_text(code_unpiped))
  }
  paste(purrr::map_chr(as.list(code_unpiped), expr_text), collapse = collapse)
}

find_pipe <- function(code_expr) {
  if (length(code_expr) == 1) grepl("%>%", code_expr)
  else unlist(purrr::map(as.list(code_expr), find_pipe))
}

uses_pipe <- function(code) {
  code_expr <- str2expression(code)
  any(find_pipe(code_expr))
}

is_function_definition <- function(code_expr) {
  # `code_expr` positions:
  # * 1: as.symbol("function")
  # * 2: list of arguments
  # * 3: body
  # * 4: srcref (or NULL after unpipe_all() removes the srcref)
  length(code_expr) == 4 &&
    identical(code_expr[[1]], as.symbol("function")) &&
    is.pairlist(code_expr[[2]]) &&
    (is.null(code_expr[[4]]) || inherits(code_expr[[4]], "srcref"))
}
