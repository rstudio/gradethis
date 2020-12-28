#' Graded object for submission value
#'
#' The return value from `graded` should be returned by every
#' `*-check` chunk when used with [grade_learnr()].
#'
#' `graded()` objects are signaled to the calling functions.
#'
#'   * [grade_result()] ignores when grades are created. `graded()` objects must be returned
#'   * [grade_code()] handles `graded()` objects internally
#'   * [grade_this()] will stop execution once a `pass()`, `pass_if_equal()`,
#'     `fail()`, `fail_if_equal()`, or `grade()` is called. To generate programmatic grades,
#'     use `graded()` or if statements around `pass*()` and `fail*()`
#'
#' @param message A character string of the message to be displayed.
#' @param correct A logical value of whether or not the checked code is correct.
#' @describeIn graded Programatic function to produce a graded a result.
#' @export
graded <- function(correct, message = NULL) {
  checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)

  obj <- structure(
    list(
      message = message %||% "",
      correct = correct
    ),
    class = c("gradethis_graded", "condition")
  )

  # Signal to parent function calls that a grade has been made
  signalCondition(obj)

  # return the object
  obj
}

is_graded <- function(x) {
  inherits(x, "gradethis_graded")
}


#' @describeIn graded Produce a _passing_ grade
#' @param env environment to evaluate the glue `message`
#' @export
pass <- function(message = getOption("gradethis.pass", "Correct!"), env = parent.frame()) {
  graded(message = glue_message_with_env(env, message), correct = TRUE)
}
#' @describeIn graded Produce a _failing_ grade
#' @export
fail <- function(message = getOption("gradethis.fail", "Incorrect"), env = parent.frame()) {
  graded(message = glue_message_with_env(env, message), correct = FALSE)
}


#' @describeIn graded Produce a _passing_ grade only if [testthat::compare()] passes on `x` and `y`
#' @param x First item in the comparison. Typically, this will be the user result
#' @param y Required value in which to compare `x`. Comparison is done using `testthat::compare(x, y)`
#' @export
pass_if_equal <- function(
  y,
  message = getOption("gradethis.pass", "Correct!"),
  x = get(".result", envir = env),
  env = parent.frame()
) {
  grade_if_equal(x = x, y = y, message = message, correct = TRUE, env = env)
}
#' @describeIn graded Produce a _failing_ grade only if [testthat::compare()] passes on `x` and `y`
#' @export
fail_if_equal <- function(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = get(".result", envir = env),
  env = parent.frame()
) {
  grade_if_equal(x = x, y = y, message = message, correct = FALSE, env = env)
}
grade_if_equal <- function(x, y, message, correct, env) {
  compare_msg <- waldo::compare(x, y)
  if (length(compare_msg) > 0) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_message_with_env(env, message), correct = correct)
}


legacy_graded <- function(...) {
  capture_graded(
    graded(...)
  )
}
