#' Run an expression as if it were in an exercise's `grade_this()` block
#'
#' This function is not intended to be used within grading code,
#' but may be helpful for testing grading code.
#'
#' @export
#' @example man/examples/example-with_exercise.R
#' @return The value of `grade_this(<expr>)(exercise)`
#'
#' @param exercise An exercise, as created by [mock_this_exercise()]
#' @param expr An unquoted expression
with_exercise <- function(exercise, expr) {
  grade_this(!!rlang::enexpr(expr))(exercise)
}
