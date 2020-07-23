#' Grade result of exercise code
#'
#' Compares the final result of the student code to known [pass_if()] and
#' [fail_if()] [condition()]s. If the student result exactly matches a known
#' case, returns the matching message value.
#'
#' @inheritParams grade_code
#'
#' @param ... [pass_if()] or [fail_if()] [condition()]s to check
#' @param default_correct In the event that no [condition()]s are met, should the end result
#'   be correct? When `"auto"`, this will be `TRUE` when all the [conditions()] are [fail_if()] 
#'   (and `FALSE` otherwise).
#' @param default_message In the event that no [condition()]s are met, what message should be
#'   included with the returned [graded()] object?
#'   
#' @return a [graded()] object from either [pass_if()] or [fail_if()] containing
#'   a formatted `correct` or `incorrect` message and whether or not a match was
#'   found.
#'
#' @seealso [grade_code()], [grade_result()], and [grade_conditions()]
#' @export
#' @examples
#' \dontrun{gradethis_demo()}
#'
#' @template grade_result_examples
grade_result <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct"),
  glue_incorrect = getOption("gradethis_glue_incorrect"),
  default_correct = "auto",
  default_message = NULL
) {

  conditions <- list(...)
  if (!length(conditions)) {
    stop("At least one condition object (e.g., `pass_if()`, `fail_if()`, `condition()`) must be provided to `grade_result()`", call. = FALSE)
  }
  chkm8_item_class(conditions, "grader_condition")

  # If there is at least one pass_if() condition, then default to an incorrect grade;
  # otherwise, default to a correct grade https://github.com/rstudio-education/gradethis/issues/118
  if (identical(default_correct, "auto")) {
    default_correct <- !any(vapply(conditions, `[[`, logical(1), "correct"))
  }
  chkm8_class(default_correct, "logical")
  
  final_grade <- graded(correct = default_correct, message = default_message)
  found_grade <- FALSE
  for (cond in conditions) {
    grade <- evaluate_condition(cond, grader_args, learnr_args)
    if (length(grade)) {
      final_grade <- grade
      found_grade <- TRUE
      break
    }
  }

  message <- glue_message(
    if (final_grade$correct) glue_correct else glue_incorrect, # nolint
    .is_match = found_grade,
    .is_correct = final_grade$correct,
    .message = final_grade$message,
    .correct = correct,
    .incorrect = incorrect
  )

  graded(
    correct = final_grade$correct,
    message = message
  )
}
