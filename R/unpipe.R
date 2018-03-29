#' unpipe
#'
#' Unpipe one layer of piped code.
#'
#' \code{unpipe()} removes one call to \%>\% from an expression,
#' reformatting the result as a nested function call. If the code does not
#' contain a top-level call to \%>\%, \code{unpipe()} returns it as is.
#'
#' @param code a quoted piece of code
unpipe <- function(code) {
  
  # Ceci n'est pas une pipe
  if (!is.call(code)) return(code)
  if (as.character(code[[1]]) != "%>%") return(code)
  
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
    rhs[3:(length(rhs) + 1)] <- rhs[2:length(rhs)]
    rhs[[2]] <- lhs
  }
  rhs
}

is_dot <- function(name) {
  length(name) == 1 && as.character(name) == "."
}