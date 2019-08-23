#' Grade result of exercise code
#'
#' Compares the final result of the student code to known [pass_if()] and
#' [fail_if()] [condition()]s. If the student result exactly matches a known
#' case, returns the matching message value.
#'
#' @inheritParams grade_code
#'
#' @param ... [pass_if()] or [fail_if()] [condition()]s to check
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
  glue_incorrect = getOption("gradethis_glue_incorrect")
) {

  results <- list(...)
  chkm8_item_class(results, "grader_condition")

  if (!any(vapply(results, `[[`, logical(1), "correct"))) {
    stop("At least one correct result must be provided")
  }

  # init final answer as not found
  final_result <- graded(correct = FALSE, message = NULL)
  found_match <- FALSE

  for (resu in results) {
    evaluated_condi <- evaluate_condition(resu, grader_args, learnr_args)
    if (! is.null(evaluated_condi)) {
      final_result <- evaluated_condi
      found_match <- TRUE
      break
    }
  }

  message <- glue_message(
    {if (final_result$correct) glue_correct else glue_incorrect}, # nolint
    .is_match = found_match,
    .is_correct = final_result$correct,
    .message = final_result$message,
    .correct = correct,
    .incorrect = incorrect
  )

  return(graded(
    correct = final_result$correct,
    message = message
  ))
}
