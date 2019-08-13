#' Condition definition
#'
#' Wrapper around \code{\link{condition}} that sets the proper \code{correct} argument.
#' This allows the instructor to specify just the comparison code, \code{x},
#' and \code{message}.
#'
#' @template pass_fail_x_condition
#' @param message chracter string for message returned
#'
#' @describeIn pass_fail_if
#'    a condition that if matched means the student provided result is correct
#'
#' @export
#' @template check_result_examples
pass_if <- function(x, message = NULL) {
  condition(x, message, correct = TRUE)
}

#' @describeIn pass_fail_if
#'    a condition that if matched means the student provided result is incorrect
#'
#' @export
fail_if <- function(x, message = NULL) {
  condition(x, message, correct = FALSE)
}
