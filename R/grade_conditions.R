#' Grade all specified conditions
#'
#' This function is deprecated, use [grade_result_strict()] instead.
#'
#' @keywords internal
#' @export
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
