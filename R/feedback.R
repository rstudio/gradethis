

#' Provide grade feedback
#'
#' Creates a feedback object suitable for returning in a learnr checking function
#' (e.g., the `exercise.checker` option in [learnr::tutorial_options()])
#'
#' @param grade a [graded()] object.
#' @param type Feedback type (visual presentation style). Can be "auto", "success", "info", "warning", "error", or "custom".
#' Note that "custom" implies that the "message" field is custom HTML rather than a character vector.
#' @param location Location for feedback ("append", "prepend", or "replace").
#' @noRd
feedback <- function(grade,
                           type = c("auto", "success", "info", "warning", "error", "custom"),
                           location = c("append", "prepend", "replace")) {
  # do not allow grade objects to throw
  grade <- capture_gradethis_conditions(grade)

  if (!is_graded(grade)) {
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
    class = "gradethis_feedback"
  )
}


is_feedback <- function(x) {
  inherits(x, "gradethis_feedback")
}
