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

unpipe_all <- function(code_expr) {
  code_expr_len <- length(code_expr)
  if (code_expr_len == 0) return(code_expr)
  if (code_expr_len == 1) return(code_expr)
  if (code_expr_len == 2 && is.null(code_expr[[2]])) return(code_expr)
  if (length(code_expr) == 4 && code_expr[[1]] == "function") return(code_expr)
  code_expr <- as.call(purrr::map(as.list(code_expr), unpipe_all))
  unpipe(code_expr)
}

find_pipe <- function(code_expr) {
  if (length(code_expr) == 1) grepl("%>%", code_expr)
  else unlist(purrr::map(as.list(code_expr), find_pipe))
}

uses_pipe <- function(code) {
  code_expr <- str2expression(code)
  any(find_pipe(code_expr))
}
