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
    code <- c(pre_pipe(code[[1]], name = names(code[1])), code[-1])
    code <- order_calls(code)
  }
  code
}

pre_pipe <- function(code, name = "") {
  if (is.call(code)) {
    new <- list(code[[2]], code[c(1, 3)])
    
    name2 <- names(code)[[2]]
    if (is.null(name2) || name2 == "") arg_name <- ""
    else arg_name <- name2
    if (is.null(name)) name <- ""
    names(new) <- c(arg_name, name)
    
    new
  } else {
    code
  }
}

repipe <- function(lst, .call = FALSE) {
  text <- purrr::map(lst, deparse)
  text <- purrr::reduce(text, paste, sep = " %>% ")
  text <- gsub("\\(NULL\\)", "()", text)
  
  if(.call) parse(text = text)[[1]]
  else text
}

renest <- function(lst, .call = FALSE) {
  
  nest <- function(a, b) {
    if (is.call(b)) {
      # is b[[2]] not a NULL placeholder?
      if (!is.null(b[[2]]) || length(b) != 2) b[3:(length(b) + 1)] <- b[2:length(b)]
      b[[2]] <- a
    }
    b
  }
  
  code <- purrr::reduce(lst, nest)
  if (.call) code
  else deparse(code)
}

# Modified from pryr::standardise_call
standardize_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (!is.null(args(f))) {
    match.call(args(f), call)
  } else {
    call
  }
}
