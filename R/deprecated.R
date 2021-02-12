

#' @title Deprecated
#' @keywords internal
#' @aliases gradethis-deprecated
#' @name gradethis-deprecated
NULL


#' @describeIn gradethis-deprecated \lifecycle{deprecated} Removed from package
#' @export
grade_feedback <- function(...) {
  deprecate_warn("0.2.0", "grade_feedback()")

  ret <- feedback(...)
  class(ret) <- "grader_feedback"
  ret
}


#' @describeIn gradethis-deprecated \lifecycle{superseded} Use [grade_result_strict()]
#' @export
grade_conditions <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  grader_args = deprecated(),
  learnr_args = deprecated(),
  glue_correct = getOption("gradethis.glue_correct_test"),
  glue_incorrect = getOption("gradethis.glue_incorrect_test")
) {
  deprecate_warn("0.1.0", "grade_result_strict()")

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


#' @describeIn gradethis-deprecated \lifecycle{superseded} Use [random_encouragement()].
#' @export
random_encourage <- function() {
  lifecycle::deprecate_soft("0.2.1", "random_encourage()", "random_encouragement()")
  random_encouragement()
}
