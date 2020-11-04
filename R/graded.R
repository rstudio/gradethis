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

  # _throw_ condition object
  # also pretty prints the condition
  signalCondition(obj)

  # return the object
  obj
}

is_graded <- function(x) {
  inherits(x, "gradethis_graded")
}


#' @rdname grade_result
#' @export
pass <- function(message = getOption("gradethis.pass", "Correct!")) {
  graded(message = glue_with_env(parent.frame(1), message), correct = TRUE)
}
#' @rdname grade_result
#' @export
fail <- function(message = getOption("gradethis.fail", "Incorrect")) {
  graded(message = glue_with_env(parent.frame(1), message), correct = FALSE)
}


#' @export
pass_if_equal <- function(y, message = "Correct!", x = get(".result", envir = parent.frame())) {
  grade_if_equal(x = x, y = y, message = message, correct = TRUE)
}
#' @export
fail_if_equal <- function(y, message = "Incorrect!", x = get(".result", envir = parent.frame())) {
  grade_if_equal(x = x, y = y, message = message, correct = FALSE)
}
grade_if_equal <- function(x, y, message, correct, glue_env = parent.frame(2)) {
  if (!isTRUE(identical(x, y))) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_with_env(glue_env, message), correct = correct)
}
