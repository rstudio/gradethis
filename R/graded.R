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

  structure(
    list(message = message %||% "", correct = correct),
    class = "grader_graded"
  )
}

is_grade <- function(x) {
  inherits(x, "grader_graded")
}


#' Provide grade feedback
#'
#' Creates a feedback object suitable for returning in a learnr checking function
#' (e.g., the `exercise.checker` option in [learnr::tutorial_options()])
#'
#' @param grade a [graded()] object.
#' @param type Feedback type (visual presentation style). Can be "auto", "success", "info", "warning", "error", or "custom".
#' Note that "custom" implies that the "message" field is custom HTML rather than a character vector.
#' @param location Location for feedback ("append", "prepend", or "replace").
#' @export
grade_feedback <- function(grade,
                           type = c("auto", "success", "info", "warning", "error", "custom"),
                           location = c("append", "prepend", "replace")) {
  # do not allow grade objects to throw
  grade <- capture_gradethis_conditions(grade)

  if (!(is_grade(grade) || is_gradethis_condition(grade))) {
    stop("`grade` must be a `graded` object", call. = FALSE)
  }

  type <- match.arg(type)

  if (identical("auto", type)) {
    type <- if (grade$correct) "success" else "error"
  }

  structure(
    list(
      message = grade$message,
      correct = grade$correct,
      type = type,
      location = match.arg(location)
    ),
    class = "grader_feedback"
  )
}


is_feedback <- function(x) {
  inherits(x, "grader_feedback")
}
