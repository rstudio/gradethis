
#' Check result of exercise code
#'
#' \code{check_result()} compares the final result of the student code to known
#' \code{\link{pass_if()}} and \code{\link{fail_if()}} \code{\link{condition}}s.
#' If the student result exactly matches a known case, \code{check_result}
#' returns the matching message value.
#'
#' @param ... \code{\link{pass_if()}} or \code{\link{fail_if()}} \code{\link{condition}}s to check
#'
#' @return a \code{\link{graded}} object from either
#'   \code{\link{pass_if}} or \code{\link{fail_if}} containing a formatted
#'   \code{correct} or \code{incorrect} message and whether or not a match was found.
#'
#' @seealso \code{\link{check_code}}, \code{\link{check_result}}, and \code{\link{test_result}}
#' @export
#' @examples
#' \dontrun{grading_demo()}
#' 
#' check_result(
#'   pass_if(~ .result == 5, "This is a correct message"),
#'           grader_args = list(),
#'           learnr_args = list(last_value = 5, envir_prep = new.env())
#' )
#'
#' check_result(
#'   pass_if(function(x) x == 5, "This is a correct message"),
#'   learnr_args = list(last_value = 5)
#' )
#'
#' check_result(
#'   pass_if(5, "This is a correct message"),
#'           learnr_args = list(last_value = 5)
#' )
#'
#' check_result(
#'   fail_if(5, "You were supposed to do this other thing!"),
#'           pass_if(~ TRUE, "should never reach here"),
#'           learnr_args = list(last_value = 5)
#' )
check_result <- function(
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
