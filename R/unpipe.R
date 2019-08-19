#' unpipe
#'
#' Unpipe one layer of piped code.
#'
#' `unpipe()` removes one call to \code{\link[magrittr]{\%>\%}} from an expression,
#' reformatting the result as a nested function call. If the code does not
#' contain a top-level call to \code{\link[magrittr]{\%>\%}}, `unpipe()` returns it as is.
#'
#' @param code a quoted piece of code
unpipe <- function(code) {

  # Ceci n'est pas une pipe
  if (!is_pipe(code)) return(code)

  # une pipe
  lhs <- code[[2]]
  rhs <- code[[3]]

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

unpipe_all <- function(code) {
  if (length(code) == 1) return(code)
  if (length(code) == 2 && is.null(code[[2]])) return(code)
  code <- as.call(purrr::map(as.list(code), unpipe_all))
  unpipe(code)
}

find_pipe <- function(code) {
  if (length(code) == 1) grepl("%>%", code)
  else unlist(purrr::map(as.list(code), find_pipe))
}

uses_pipe <- function(code) {
  any(find_pipe(code))
}
