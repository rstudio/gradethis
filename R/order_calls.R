#' Order calls
#'
#' Turns a quoted object into a list of symbols that would represent the call as
#' a pipe if you placed a \code{%>%} between each element of the list. This
#' let's checking code evaluate the elements in the same order that R would.
#'
order_calls <- function(code) {
  if (is.name(code) ||
    is.call(code) ||
    is.atomic(code)) {
    code <- list(code)
  }
  if (is.call(code[[1]]) && length(code[[1]]) != 1) {
    code[[1]] <- standardize_call(code[[1]])
    code <- c(pre_pipe(code[[1]]), code[-1])
    code <- order_calls(code)
  }
  code
}

pre_pipe <- function(code) {
  if (is.call(code)) {
    list(code[[2]], code[c(1, 3)])
  }
}

repipe <- function(lst) {
  text <- purrr::map(lst, deparse)

  # alternatively use accumulate for step by step checking
  text <- purrr::reduce(text, paste, sep = "%>%")
  eval(parse(text = text))
}

# Modified from pryr::standardise_call
standardize_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (is.primitive(f)) {
    if (!is.null(args(f))) {
      return(match.call(args(f), call))
    } else {
      return(call)
    }
  }
  match.call(f, call)
}
