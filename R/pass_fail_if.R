#' Pass if condition matches
#' @template pass_fail_x_condition
#' @param message chracter string for message returned
#' @export
pass_if <- function(x, message = NULL) {
  condition(x, message, correct = TRUE)
}

#' Fail if condition matches
#' @template pass_fail_x_condition
#' @param message chracter string for message returned
#' @export
fail_if <- function(x, message = NULL) {
  condition(x, message, correct = FALSE)
}
