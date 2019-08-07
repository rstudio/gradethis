#' Condition object
#' Captures what the user passes into \code{\link{pass_if}} or \code{\link{fail_if}},
#' figures out what type of object was passed into \code{x},
#' and returns a \code{grader_condition} object that will be passed into \code{evaluate_condi}
#'
#' @param x expression to be evaluated
#' @param message character string for message returned
#' @param correct logical whether the condition is the correct answer
#'
#' @return a \code{grader_condition} object that contains
#'   the expression \code{x},
#'   the message \code{message},
#'   whether or not the expression is the correct answer or not, \code{correct},
#'   the type of expression (formula, function, or value), \code{type}
#' @export
condition <- function(x, message, correct) {
  type <-
    if (rlang::is_formula(x)) {
      "formula"
    } else if (rlang::is_function(x)) {
      "function"
    } else {
      "value"
    }

  ret <- list(
    x = x,
    message = message,
    correct = correct,
    type = type
  )
  class(ret) <- "grader_condition"
  ret
}
