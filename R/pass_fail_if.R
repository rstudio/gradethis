#' Condition definition
#'
#' \code{pass_if} and \code{fail_if} are wrappers around \code{\link{condition}}
#' that sets the proper \code{correct} argument.
#' This allows the instructor to specify just the comparison code, \code{x},
#' and \code{message}.
#'
#' @describeIn condition
#'    a condition that if matched means the student provided result is correct
#'
#' @export
#' @template check_result_examples
pass_if <- function(x, message = NULL) {
  condition(x, message, correct = TRUE)
}

#' @describeIn condition
#'    a condition that if matched means the student provided result is incorrect
#'
#' @export
fail_if <- function(x, message = NULL) {
  condition(x, message, correct = FALSE)
}
