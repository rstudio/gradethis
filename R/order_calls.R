#' Order calls
#'
#' Turns a quoted object into a list of symbols that would represent the call as
#' a pipe if you placed a \code{\%>\%} between each element of the list. This
#' let's checking code evaluate the elements in the same order that R would.
#'
#' @noRd
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
  code <- purrr::discard(code, is.null)
  purrr::map(code, remove_null_from_call)
}

pre_pipe <- function(code, name = "") {
  if (is.call(code)) {
    new <- list(code[[2]], code[-2])

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
  lst <- rev(lst)

  nest <- function(b, a) {
    if (is.call(a)) {
      if (length(a) > 1) {

        # double check that the function does
        # not contain a placeholder NULL argument
        if (length(a) != 2 || !is.null(a[[2]])) {
          if (!is.null(names(a))) {
            names_a <- names(a)
            names_a <- c(names(a)[1], "", names(a)[2:length(a)])
            a[3:(length(a) + 1)] <- a[2:length(a)]
            names(a) <- names_a
          } else {
            a[3:(length(a) + 1)] <- a[2:length(a)]
          }
        }
      }
      a[[2]] <- b
    }
    a
  }

  code <- purrr::reduce(lst, nest)
  if (.call) code
  else deparse(code)
}

# Modified from pryr::standardise_call
# Returns a version of the call that has
# arguments in a standard order and
# argument names supplied for each argument after the first
standardize_call <- function(code, env = parent.frame()) {
  stopifnot(is.call(code))
  f <- eval(code[[1]], env)
  if (!is.null(args(f))) {
    call <- match.call(args(f), code)

    # because checking code should follow practice
    # of not naming the first argument (unless the
    # user deliberately does so) and not naming the
    # arguments of infix operators
    first_arg <- names(as.list(args(f)))[1]
    if (is_infix(code)) {
      names(call) <- NULL
    } else if (!any(names(code) == first_arg)) {
      names(call)[which(names(call) == first_arg)] <- ""
    }
  } else {
    call <- code
  }
  call
}

remove_null_from_call <- function(code){
  if (is.call(code) && length(code) > 1) {
    if (is.null(code[[2]]) && is.null(names(code[2]))) {
      code[[2]] <- NULL
    }
  }
  code
}
