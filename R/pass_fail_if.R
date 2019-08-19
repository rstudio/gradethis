#' Condition definition
#'
#' `pass_if` and `fail_if` are wrappers around [condition()]
#' that sets the proper `correct` argument.
#' This allows the instructor to specify just the comparison code, `x`,
#' and `message`.
#'
#' @describeIn condition
#'    a condition that if matched means the student provided result is correct
#'
#' @export
#' @template grade_result_examples
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
