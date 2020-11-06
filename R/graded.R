#' Graded object for submission value
#'
#'The return value from `graded` should be returned by every
#'`*-check` chunk when used with [grade_learnr()].
#'
#' @param message A character string of the message to be displayed.
#' @param correct A boolean value of whether or not the checked code is correct.
#' @export
graded <- function(correct, message = NULL) {
  chkm8_single_character(message)
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


#' @export
pass <- function(message = getOption("gradethis.pass", "Correct!"), env = parent.frame()) {
  graded(message = glue_with_env(env, message), correct = TRUE)
}
#' @export
fail <- function(message = getOption("gradethis.fail", "Incorrect"), env = parent.frame()) {
  graded(message = glue_with_env(env, message), correct = FALSE)
}


#' @export
pass_if_equal <- function(
  y,
  message = getOption("gradethis.pass", "Correct!"),
  x = get(".result", envir = env),
  env = parent.frame()
) {
  grade_if_equal(x = x, y = y, message = message, correct = TRUE, env = env)
}
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
  compare_info <- testthat::compare(x, y)
  if (!compare_info$equal) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_with_env(env, message), correct = correct)
}


legacy_graded <- function(...) {
  capture_graded(
    graded(...)
  )
}
