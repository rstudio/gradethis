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
  args <- list(..., grader_args = grader_args, learnr_args = learnr_args)
  mat <- do.call(find_matching_condition, args)
  final_result <- mat$result
  
  message <- glue_message(
    {if (final_result$correct) glue_correct else glue_incorrect}, # nolint
    .is_match = mat$matching_conditional,
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

#' Find a matching condition
#' 
#' Looks through a given set of conditions to attempt to find a match.
#' @param ... [pass_if()] or [fail_if()] [condition()]s
#' @inheritParams grade_code
#' @return A list with two elements: 
#' 
#'  - `matching_conditional` - `TRUE` if the given list of conditions contained a match. Otherwise, FALSE
#'  - `result` - The evaluated [condition()] object; a [graded()] value.
#' @export
find_matching_condition <- function(..., grader_args, learnr_args){
  
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
  
  list(matching_conditional = found_match, result = final_result)
}
