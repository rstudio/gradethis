#' An error checking function for use with learnr
#'
#' @description
#' \pkg{learnr} uses the checking code in `exercise.error.check.code` when the
#' user's submission produces an error during evaluation.
#' `gradethis_error_checker()` provides default error checking suitable for most
#' situations where an error was _not expected_.
#'
#' If a solution for the exercise is available, the user's submission will be
#' compared to the example solution and the message to the student will include
#' code feedback. Otherwise, the error message from R is returned.
#'
#' If you _are expecting_ the user to submit code that throws an error, use the
#' `*-error-check` chunk to write custom grading code that validates that the
#' correct error was created.
#'
#' @param ... Ignored but included for future compatibility.
#' @param message The feedback message when an error occurred and no solution is
#'   provided for the exercise. May reference `.error` or any of the
#'   [grade_this-objects]. The default value is set by [gradethis_setup()].
#' @inheritParams fail
#'
#' @return A checking function compatible with [gradethis_exercise_checker()].
#'
#' @examples
#' # The default error checker is run on an exercise that produces an error.
#' # In the following example, the object `b` is not defined.
#'
#' # This is the error that the user's submission creates:
#' tryCatch(
#'   b,
#'   error = function(e) message(e$message)
#' )
#'
#' # If you haven't provided a model solution:
#' gradethis_error_checker()(mock_this_exercise(b))
#'
#' # If a model solution is available:
#' gradethis_error_checker()(mock_this_exercise(b, a))
#' @seealso [gradethis_setup()], [gradethis_exercise_checker()]
#' @export
gradethis_error_checker <- function(
  ...,
  hint = getOption("gradethis.fail.hint", TRUE),
  message = getOption("gradethis.error_checker.message", NULL),
  encourage = getOption("gradethis.fail.encourage", FALSE)
) {
  function(check_env) {
    solution_code <- get0(".solution_code", envir = check_env, ifnotfound = NULL)
    if (!is.null(solution_code) && isTRUE(hint)) {
      signal_grade(grade_this_code()(check_env))
    }

    .error <- get0(".last_value", envir = check_env, ifnotfound = NULL)
    msg <- glue::glue(
      message %||% gradethis_default_options$error_checker.message,
      .trim = FALSE
    )
    if (is.null(msg) || length(msg) < 1) {
      msg <-
        "An error occurred with your code. Check your syntax and try again." #nocov
    }
    fail(msg, hint = FALSE, encourage = TRUE)
  }
}
