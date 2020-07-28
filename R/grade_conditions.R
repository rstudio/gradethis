#' Grade all specified conditions
#'
#' This function is deprecated, use [grade_result_strict()] instead.
#'
#' @inheritParams grade_code
#'
#' @param correct A character string to display if all tests pass. This
#'   character string will be run through [glue::glue_data] with:
#'
#'   * `num_correct`: Number of correct tests. (Equals `num_total`)
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found. (`NULL`)
#'
#' @param incorrect A character string to display if at least one test fails.
#'   This character string will be run through [glue::glue_data()] with:
#'
#'   * `num_correct`: Number of correct tests
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found
#'
#' @param ... ignored
#'
#'
#' @return a `grader_graded` structure from [graded()] containing a formatted
#'   `correct` or `incorrect` message.
#' @export
#'
grade_conditions <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct_test"),
  glue_incorrect = getOption("gradethis_glue_incorrect_test")
) {
  
  .Deprecated("grade_result_strict")
  
  grade_result_strict(
    ..., 
    correct = correct, 
    incorrect = incorrect, 
    grader_args = grader_args, 
    learnr_args = learnr_args, 
    glue_correct = glue_correct, 
    glue_incorrect = glue_incorrect
  )
}